{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Text.ParserCombinators.Parsec hiding (try)
import qualified Data.Map as Map

import Data.Maybe

import Data.Text (pack, Text)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Text.IO as TIO

import Text.Parsec
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Language as L

import GHC.Generics
import Data.Aeson (encode, ToJSON, toEncoding , toJSON, (.=), object, pairs, Value, toJSONList, Value(Null))

clausewitz :: P.TokenParser st
clausewitz = P.makeTokenParser clausewitzDef

clausewitzDef :: P.LanguageDef st
clausewitzDef = L.emptyDef
  { P.commentStart = ""
  , P.commentEnd = ""
  , P.commentLine = "#"
  , P.nestedComments = False
  , P.identStart = letter
  , P.identLetter = alphaNum <|> oneOf "_"
  , P.opStart = P.opLetter clausewitzDef
  , P.opLetter = oneOf "<=>"
  , P.reservedNames = ["yes", "no"]
  , P.reservedOpNames = []
  , P.caseSensitive = True
  }

whiteSpace = P.whiteSpace clausewitz
stringLiteral = P.stringLiteral clausewitz
natural = P.natural clausewitz
naturalOrFloat = P.naturalOrFloat clausewitz
reserved = P.reserved clausewitz
braces = P.braces clausewitz
operator = P.operator clausewitz

type CReference = String
type CIdentifier = String
type CLiteral = String

data COperator = L | LE | E | GE | G
  deriving (Show, Generic)

data Clausewitz = CIdent CIdentifier | CLit CLiteral | CRef CReference
  | CBool Bool | CNum (Either Integer Double) | CDate Integer Integer Integer
  | CMap [(Either CIdentifier Integer, COperator, Clausewitz)]
  | CLitList [CLiteral] | CIdentList [CIdentifier]
  deriving (Show, Generic)

data Definition = Variable CReference Clausewitz | Data CIdentifier Clausewitz
  deriving (Show, Generic)

operatorToText :: COperator -> Text
operatorToText L  = ("<" :: Text)
operatorToText LE = ("<=" :: Text)
operatorToText E  = ("=" :: Text)
operatorToText GE = (">=" :: Text)
operatorToText G  = (">" :: Text)

instance ToJSON COperator where
  toJSON op = toJSON $ operatorToText op

instance ToJSON Clausewitz where
  toJSON (CIdent x) = toJSON x
  toJSON (CLit x) = toJSON $ "!" ++ x
  toJSON (CRef x) = toJSON ('@':x)
  toJSON (CBool x) = toJSON x
  toJSON (CNum (Left x)) = toJSON x
  toJSON (CNum (Right x)) = toJSON x
  toJSON (CDate y m d) = toJSON . pack . concat
                         $ [show y, ('-':show m), ('-':show d)]
  
  toJSON (CMap x) = (object . concat) $ item <$> x
    where
      item (Left i, E,  c) = [pack i .= toJSON c]
      item (Left i, op, c) = [pack ('~':i) .= object [(operatorToText op) .= toJSON c]]
      item (Right i, E,  c) = [(pack . show) i .= toJSON c]
    
  toJSON (CLitList x) = toJSON x
  toJSON (CIdentList x) = toJSON x

identifier :: Parser CIdentifier
identifier = P.identifier clausewitz

literal :: Parser CLiteral
literal = stringLiteral

reference :: Parser CReference
reference = char '@' *> identifier

assignment :: Parser a -> Parser b -> (a -> b -> c) -> Parser c
assignment a b c = c <$> (a <* binaryOp) <*> b

binaryOp :: Parser COperator
binaryOp = op <$> operator
  where
    op "<" = L
    op "<=" = LE
    op "=" = E
    op ">=" = GE
    op ">" = G

mappingItem :: Parser (Either CIdentifier Integer, COperator, Clausewitz)
mappingItem = (\a b c-> (a, b, c)) <$>
  ((Left <$> identifier) <|>( Right <$> natural)) <*> binaryOp <*> value

value :: Parser Clausewitz
value = (CIdent <$> identifier)
        <|> (braces $ (
                try (CMap <$> many1 mappingItem)
                <|> (CIdentList <$> many1 identifier)
                <|> (CLitList <$> many1 literal)
                <|> (pure $ CMap []))
            )
        <|> try (signedDate
                 <$> option '+' (char '-')
                 <*> (natural <* char '.')
                 <*> (natural <* char '.')
                 <*> natural)
        <|> (signedNumber <$> (option '+' $ oneOf "+-") <*> naturalOrFloat)
        <|> (CLit <$> literal)
        <|> (CRef <$> reference)
        <|> (CBool <$> bool)

  where
    signedDate :: Char -> Integer -> Integer -> Integer -> Clausewitz
    signedDate s y m d = case s of
      '+' -> CDate y m d
      '-' -> CDate (negate y) m d
    bool = (reserved "yes" >> return True)
           <|> (reserved "no" >> return False)
    signedNumber :: Char -> Either Integer Double -> Clausewitz
    signedNumber s n = CNum $ case s of
      '+' -> n
      '-' -> negate <$> n


clausewitzParser :: Parser [Definition]
clausewitzParser = whiteSpace >> many (
  (assignment reference value Variable)
  <|> (assignment identifier value Data)
  )

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _) = Nothing
rightToMaybe (Right x) = Just x

collect :: [Definition] -> (Map.Map CReference Clausewitz,
                            [Map.Map CIdentifier Clausewitz])
collect ds = (
  Map.fromList $ tuple <$> filter isVar ds,
  ((\x -> Map.fromList [x]) . tuple) <$> filter (not . isVar) ds
  )
  where
    tuple (Variable a b) = (a, b)
    tuple (Data a b) = (a, b)
    isVar (Variable _ _) = True
    isVar (Data _ _) = False

dereference :: Map.Map CReference Clausewitz
            -> [Map.Map CIdentifier Clausewitz]
            -> [Map.Map CIdentifier Clausewitz]
dereference lut cs = Map.map (unvariable1 lut) <$> cs

unvariable1 :: Map.Map CReference Clausewitz -> Clausewitz -> Clausewitz
unvariable1 lut r@(CRef s) = Map.findWithDefault r s lut
unvariable1 lut (CMap ms) = CMap $ (unvarMap lut) <$> ms
  where
    unvarMap :: Map.Map CReference Clausewitz
      -> (Either CIdentifier Integer, COperator, Clausewitz)
      -> (Either CIdentifier Integer, COperator, Clausewitz)
    unvarMap lut (s, op, c) = (s, op, unvariable1 lut c)
unvariable1 _ c = c

parseStr :: String -> [Definition]
parseStr s = case parse clausewitzParser "" s of
  Left e -> error $ show e
  Right r -> r

main :: IO ()
main = do
  input <- getContents
  BL.putStr $ encode $((\(lut, cs) -> dereference lut cs) . collect) $ parseStr input
