local lpeg = require('lpeg')
local re = require('re')

local P, R, S, C, Cc, Ct, V = lpeg.P, lpeg.R, lpeg.S, lpeg.C, lpeg.Cc, lpeg.Ct, lpeg.V

-- Turtle - Terse RDF Triple Language EBNF


----------------------------------------------
-- [40]hex::=[#x30-#x39] | [#x41-#x46]
local hex = R"09"+R"AF"

-- [29]language::=[a-z]+ ('-' [a-z0-9]+ )*
local language = R"az"^1*(P"-"*R"az"+R"09")^0

-- [30]nameStartChar::=[A-Z] | "_" | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
local nameStartChar = R"AZ"+P"_"+R"az" -- TODO hex parts

-- [19]exponent::=[eE] ('-' | '+')? [0-9]+
local exponent = (S"e"+S"E")*(S"-"+S"+")^-1*R"09"^1

-- [10]comment::='#' ( [^#xA#xD] )*
local comment = S"#"*re.compile("[^\x0A\x0D]")^0

-- [20]boolean::='true' | 'false'
local boolean = S"true"+S"false"

-- [18]decimal::=('-' | '+')? ( [0-9]+ '.' [0-9]* | '.' ([0-9])+ | ([0-9])+ )
local decimal = (S"-"+S"+")^-1*((R"09"^1*S"."*R"09"^0)+(S"."*R"09"^1)+R"09"^1)

-- [16]integer::=('-' | '+') ? [0-9]+
local integer = (S"-"+S"+")^-1*R"09"^1

-- [17]double::=('-' | '+') ? ( [0-9]+ '.' [0-9]* exponent | '.' ([0-9])+ exponent | ([0-9])+ exponent )
local double = (S"-"+S"+")^-1*((R"09"^1*S"."*R"09"^0*exponent)+(S"."*R"09"^1*exponent)+(R"09"^1*exponent))

-- [38]character::= '\u' hex hex hex hex |
-- '\U' hex hex hex hex hex hex hex hex |
-- '\\' | [#x20-#x5B] | [#x5D-#x10FFFF]
local character =
   (S"\\u"*hex*hex*hex*hex)+
   (S"\\U"*hex*hex*hex*hex*hex*hex*hex*hex)+
   S"\\"+R"\x20\x5b"+R"\x5d\xff" -- TODO multi-byte skipped

-- [39]echaracter::=character | '\t' | '\n' | '\r'
local echaracter = character+S"\t"+S"\n"+S"\r"

-- [41]ucharacter::= ( character - #x3E ) | '\>'
local ucharacter = character+S">" -- TODO handle removing \x3e

-- [42]scharacter::= ( echaracter - #x22 ) | '\"'
local scharacter = echaracter+S"\"" -- TODO handle removing \x22

-- [43]lcharacter::=echaracter | '\"' | #x9 | #xA | #xD
local lcharacter = echaracter+S"\x22"+S"\x09"+S"\x0a"+S"\x0d"

-- [36]string::=#x22 scharacter* #x22
local string_ = S"\x22"*scharacter^0*S"\x22"

-- [37]longString::=#x22 #x22 #x22 lcharacter* #x22 #x22 #x22
local longString = S"\x22\x22\x22"*lcharacter^0*S"\x22\x22\x22"

-- [31]nameChar::=nameStartChar | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
local nameChar = S""

-- [32]name::=nameStartChar nameChar*
local name = S"" -- TODO

-- [26]nodeID::='_:' name
local nodeID = S"_:"*name

-- [33]prefixName::=( nameStartChar - '_' ) nameChar*
local prefixName = S"" -- TODO

-- [34]relativeURI::=ucharacter*
local relativeURI = ucharacter^0

-- [28]uriref::='<' relativeURI '>'
local uriref = S"<"*relativeURI*S">"

-- [27]qname::=prefixName? ':' name?
local qname = prefixName^-1*S":"*name^-1

-- [35]quotedString::=string | longString
local quotedString = string_+longString

-- [24]ws::=#x9 | #xA | #xD | #x20 | comment
local ws = S"\x09"+S"\x0a"+S"\x0d"+S"\x20"+comment

-- [25]resource::=uriref | qname
local resource = uriref+qname

-- [12]predicate::=resource
local predicate = resource

-- [9]verb::=predicate | 'a'
local verb = predicate+S"a"

-- [15]datatypeString::=quotedString '^^' resource
local datatypeString = quotedString*S"^^"*resource

-- [14]literal::=quotedString ( '@' language )? | datatypeString | integer | double | decimal | boolean
local literal = (quotedString*(S"@"*language)^-1)+datatypeString+integer+double+decimal+boolean

-- There's a cycle:
--  predicateObjectList -> objectList -> object -> blank -> predicateObjectList

----------------
local object = P{"object";
				 -- [13]object::=resource | blank | literal
				 object = resource+V"blank"+literal,

				 -- [21]blank::=nodeID | '[]' | '[' predicateObjectList ']' | collection
				 blank = nodeID+S"[]"+(S"["*V"predicateObjectList"*S"]")+V"collection",

				 -- [8]objectList::=object ( ',' object)*
				 objectList = V"object"*(S","*V"object")^0,

				 -- [7]predicateObjectList::=verb objectList ( ';' verb objectList )* ( ';')?
				 predicateObjectList = verb*V"objectList"*(S";"*verb*V"objectList")^0*S";"^-1,

				 -- [22]itemList::=object+
				 itemList = V"object"^1,

				 -- [23]collection::='(' itemList? ')'
				 collection = S"("*V"itemList"^-1*S")"
}
-- exact same, repeated as above, but with a different match element
local blank = P{"blank";
				object = resource+V"blank"+literal,
				blank = nodeID+S"[]"+(S"["*V"predicateObjectList"*S"]")+V"collection",
				objectList = V"object"*(S","*V"object")^0,
				predicateObjectList = verb*V"objectList"*(S";"*verb*V"objectList")^0*S";"^-1,
				itemList = V"object"^1,
				collection = S"("*V"itemList"^-1*S")"
}
local predicateObjectList = P{"predicateObjectList";
							  object = resource+V"blank"+literal,
							  blank = nodeID+S"[]"+(S"["*V"predicateObjectList"*S"]")+V"collection",
							  objectList = V"object"*(S","*V"object")^0,
							  predicateObjectList = verb*V"objectList"*(S";"*verb*V"objectList")^0*S";"^-1,
							  itemList = V"object"^1,
							  collection = S"("*V"itemList"^-1*S")"
}
----------------

-- [11]subject::=resource | blank
local subject = resource+blank

-- [6]triples::=subject predicateObjectList
local triples = subject+predicateObjectList

-- [5]base::='@base' ws+ uriref
local base = S"@base"*ws^1*uriref

-- [4]prefixID::='@prefix' ws+ prefixName? ':' uriref
local prefixID = S"@prefix"*ws^1*prefixName^-1*S":"*uriref

-- [3]directive::=prefixID | base
local directive = prefixID+base

-- [2]statement::=directive '.' | triples '.' | ws+
local statement = (directive*S".")+(triples*S".")+ws^1

-- [1]turtleDoc::=statement*
local turtleDoc = statement^0

local test1 = [[
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix dc: <http://purl.org/dc/elements/1.1/> .
@prefix ex: <http://example.org/stuff/1.0/> .

<http://www.w3.org/TR/rdf-syntax-grammar>
  dc:title "RDF/XML Syntax Specification (Revised)" ;
  ex:editor [
    ex:fullname "Dave Beckett";
    ex:homePage <http://purl.org/net/dajobe/>
  ] .
]]

require('pl.pretty').dump(lpeg.match(turtleDoc, test1))
