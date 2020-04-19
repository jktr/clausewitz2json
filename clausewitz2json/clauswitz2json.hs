{-# LANGUAGE OverloadedStrings #-}

import Text.ParserCombinators.Parsec hiding (try)
import qualified Data.Map as Map

import Data.Text

import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Text.Parsec
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Language as L

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
naturalOrFloat = P.naturalOrFloat clausewitz
reserved = P.reserved clausewitz
braces = P.braces clausewitz
operator = P.operator clausewitz

type Reference = String
type Identifier = String
type Literal = String

data Operator = L | LE | E | GE | G
  deriving (Show)

data Value = Ident Identifier | Lit Literal | Ref Reference
  | Boolean Bool | Number (Either Integer Double)
  | Mapping [(Identifier, Operator, Value)]
  | LitList [Literal] | IdentList [Identifier]
  deriving (Show)

data Definition = Variable Reference Value | Data Identifier Value
  deriving (Show)

identifier :: Parser Identifier
identifier = P.identifier clausewitz

literal :: Parser Literal
literal = stringLiteral

reference :: Parser Reference
reference = char '@' *> identifier

assignment :: Parser a -> Parser b -> (a -> b -> c) -> Parser c
assignment a b c = c <$> (a <* binaryOp) <*> b

binaryOp :: Parser Operator
binaryOp = op <$> operator
  where
    op "<" = L
    op "<=" = LE
    op "=" = E
    op ">=" = GE
    op ">" = G

mappingItem :: Parser (Identifier, Operator, Value)
mappingItem = (\a b c-> (a, b, c)) <$> identifier <*> binaryOp <*> value

value :: Parser Value
value = (Ident <$> identifier)
        <|> (Lit <$> literal)
        <|> (Ref <$> reference)
        <|> (Boolean <$> bool)
        <|> (Number <$> number)
        <|> (braces $ (
                try (Mapping <$> many1 mappingItem)
                <|> (IdentList <$> many1 identifier)
                <|> (LitList <$> many1 literal)
                <|> (pure $ Mapping [])))
  where
    bool = (reserved "yes" >> return True)
                <|> (reserved "no" >> return False)
    number = do
       sign <- option '+' $ oneOf "+-"
       num <- naturalOrFloat
       return $ case sign of
         '+' -> num
         '-' -> negate <$> num

clausewitzParser :: Parser [Definition]
clausewitzParser = whiteSpace >> many (
  (assignment reference value Variable)
  <|> (assignment identifier value Data)
  )

parseStr :: String -> [Definition]
parseStr s = case parse clausewitzParser "" s of
  Left e -> error $ show e
  Right r -> r

main :: IO ()
main = do
  input <- getContents
  putStr $ show $ parseStr input
