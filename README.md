# clausewitz2json

Paradox Interactive's Clausewitz Engine uses a custem plain-text file
format in its game files. The use of a format unique to the engine makes
automated processing and analysis of these files difficult. As a
result, this project aims to implement a format conversion to (and
from) json, which should simplify working with these files considerably.

These features are currently implemented:
  - clausewitz source parser (lossless)
  - JSON DSL emitter (lossy)
  
Notable missing features are:
  - clausewitz DSL emitter
  - JSON DSL emitter (lossless)

As the structure of clausewitz files is somewhat more expressive than
json types, the json conversion must use some extra semantics to
fully represent clausewitz files.

Extra datatypes are handled via string prefixes as follows:

  - literals (`"FOO_BAR"`) → `!lit:FOO_BAR`
  - variables (`… = @foo`) → `!var:foo`
  - dates (`2200.00.00`) → `!date:YYYY-MM-DD`
  - comparisons (`{ a >= b }`) → `"a": { ">=": "b" }`
  
Furthermore, some transformations are applied:
  
  - use of (defined) variables is replaced by that variables
    value where possible
  - unused variable definitions are dropped
  - empty maps `{}` are interpreted as empty lists (`[]`),
    as empty maps and empty lists are otherwise ambiguous
  - repeated definitions with the same name become have
    their values merged into a list under that name
  - clausewitz files are represented as a list of  definitions
  
Test sets and examples are provided in test/raw and test/json.
