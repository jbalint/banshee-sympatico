local turtleparse = {}

local lpeg = require('lpeg')
local re = require('re')

local P, R, S, V = lpeg.P, lpeg.R, lpeg.S, lpeg.V
local C, Cc, Cf, Cg, Cs, Ct, Cmt = lpeg.C, lpeg.Cc, lpeg.Cf, lpeg.Cg, lpeg.Cs, lpeg.Ct, lpeg.Cmt

-- Turtle - Terse RDF Triple Language EBNF
-- from: http://www.w3.org/TeamSubmission/turtle/
-- this may not have been the best reference
-- c.f. http://www.w3.org/TR/2014/REC-turtle-20140225/

-- I added the whitespace marked as "JB:ws"

----------------------------------------------
-- [40]hex::=[#x30-#x39] | [#x41-#x46]
local hex = R"09"+R"AF"

-- [29]language::=[a-z]+ ('-' [a-z0-9]+ )*
local language = R"az"^1*(P"-"*R"az"+R"09")^0

-- [30]nameStartChar::=[A-Z] | "_" | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
local nameStartChar = R"AZ"+P"_"+R"az"+R"\xc0\xd6"+R"\xd8\xf6" -- TODO multibyte

-- [19]exponent::=[eE] ('-' | '+')? [0-9]+
local exponent = (P"e"+P"E")*(P"-"+P"+")^-1*R"09"^1

-- [10]comment::='#' ( [^#xA#xD] )*
local comment = P"#"*re.compile("[^\x0A\x0D]")^0

-- [20]boolean::='true' | 'false'
local boolean = P"true"+P"false"

-- [18]decimal::=('-' | '+')? ( [0-9]+ '.' [0-9]* | '.' ([0-9])+ | ([0-9])+ )
local decimal = (P"-"+P"+")^-1*((R"09"^1*P"."*R"09"^0)+(P"."*R"09"^1)+R"09"^1)

-- [16]integer::=('-' | '+') ? [0-9]+
local integer = (P"-"+P"+")^-1*R"09"^1

-- [17]double::=('-' | '+') ? ( [0-9]+ '.' [0-9]* exponent | '.' ([0-9])+ exponent | ([0-9])+ exponent )
local double = (P"-"+P"+")^-1*((R"09"^1*P"."*R"09"^0*exponent)+(P"."*R"09"^1*exponent)+(R"09"^1*exponent))

-- [38]character::= '\u' hex hex hex hex |
-- '\U' hex hex hex hex hex hex hex hex |
-- '\\' | [#x20-#x5B] | [#x5D-#x10FFFF]
local character =
   (P"\\u"*hex*hex*hex*hex)+
   (P"\\U"*hex*hex*hex*hex*hex*hex*hex*hex)+
   P"\\"+R"\x20\x5b"+R"\x5d\xff" -- TODO multi-byte skipped

-- [39]echaracter::=character | '\t' | '\n' | '\r'
local echaracter = character+P"\t"+P"\n"+P"\r"

-- [41]ucharacter::= ( character - #x3E ) | '\>'
-- ">" must be escaped
local ucharacter = (character-P"\x3e")+P"\\>"

-- [42]scharacter::= ( echaracter - #x22 ) | '\"'
local scharacter = P"\\\""+(echaracter-P"\x22")

-- Process escapes for strings
function processEscapes(subj, pos, str)
   str = str:gsub('\\"', '"'):gsub('\\t', "\x09"):gsub('\\n', "\x0a"):gsub('\\r', "\x0d")
   return pos, str
end

-- [36]string::=#x22 scharacter* #x22
local string_ = Cmt(P"\x22"*C(scharacter^0)*P"\x22", processEscapes)

-- [43]lcharacter::=echaracter | '\"' | #x9 | #xA | #xD
-- [37]longString::=#x22 #x22 #x22 lcharacter* #x22 #x22 #x22

-- from later turtle spec, better for parsing
-- c.f. http://lists.w3.org/Archives/Public/public-rdf-comments/2014Feb/0018.html
--[26]	UCHAR	::=	'\u' HEX HEX HEX HEX | '\U' HEX HEX HEX HEX HEX HEX HEX HEX
local UCHAR = (P"\\u"*hex*hex*hex*hex)+(P"\\U"*hex*hex*hex*hex*hex*hex*hex*hex)
--[159s]	ECHAR	::=	'\' [tbnrf"'\]
local ECHAR = P"\\"*S"tbrnf\"'\\"
--[25]	STRING_LITERAL_LONG_QUOTE	::=	'"""' (('"' | '""')? ([^"\] | ECHAR | UCHAR))* '"""'
local longStringContents = C(((P'""'+P'"')^-1*(re.compile('[^"\\]')+ECHAR+UCHAR))^0)
local longString = Cmt(P'"""'*longStringContents*P'"""', processEscapes)

-- [31]nameChar::=nameStartChar | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
local nameChar = nameStartChar+P"-"+R"09"+P"\xb7" -- TODO multibyte

-- [32]name::=nameStartChar nameChar*
local name = nameStartChar*nameChar^0

-- [26]nodeID::='_:' name
local nodeID = P"_:"*C(name)

-- [33]prefixName::=( nameStartChar - '_' ) nameChar*
local prefixName = (nameStartChar-P"_")*nameChar^0

-- [34]relativeURI::=ucharacter*
local relativeURI = ucharacter^0

-- [28]uriref::='<' relativeURI '>'
-- TODO using the Cc(nil) here to make sure the fold function is
-- called, bad usage needs fixed
local uriref = Cf(P"<"*C(relativeURI)*Cc(nil)*P">", function (uri)
					 return {type="UriRef",
							 uri=uri}
end)

-- [27]qname::=prefixName? ':' name?
local prefixQname = Cf(C(prefixName)*P":"*C(name)^-1, function (prefix, name)
					return {type="Qname",
							prefix=prefix,
							name=name}
end)
local blankQname = Cf(P":"*Cc('')*C(name), function (prefix, name)
					return {type="Qname",
							prefix=prefix,
							name=name}
end)
local qname = prefixQname+blankQname

-- [35]quotedString::=string | longString
local quotedString = longString+string_

-- [24]ws::=#x9 | #xA | #xD | #x20 | comment
local ws = P"\x09"+P"\x0a"+P"\x0d"+P"\x20"+comment

-- [25]resource::=uriref | qname
local resource = uriref+Cg(qname)

-- [12]predicate::=resource
local predicate = resource

-- [9]verb::=predicate | 'a'
local verb = Cg(predicate+C(P"a"), "verb")

-- [15]datatypeString::=quotedString '^^' resource
local datatypeString = Cf(quotedString*P"^^"*resource, function (str, datatype)
							 return {type="TypedString",
									 value=str,
									 datatype=datatype}
end)

-- [14]literal::=quotedString ( '@' language )? | datatypeString | integer | double | decimal | boolean
local literal = datatypeString+(quotedString*(P"@"*language)^-1)+C(integer+double+decimal+boolean)

-----------------------------
local makeGrammar = function (elem)
   return P{elem;
			-- [13]object::=resource | blank | literal
			object = resource+V"blank"+literal,

			-- [21]blank::=nodeID | '[]' | '[' JB:ws* predicateObjectList JB:ws* ']' | collection
			blankNode = Cmt(nodeID, function (subj, pos, nodeID)
							   return pos, {type="Blank", nodeID=nodeID}
			end),
			blank = V"blankNode"+P"[]"+(P"["*ws^0*V"predicateObjectList"*ws^0*P"]")+V"collection",

			-- [8]objectList::=object (JB:ws* ',' JB:ws* object)*
			objectList = Cg(Ct(V"object"*(ws^0*P","*ws^0*V"object")^0), "objects"),

			predicateObject = Ct(verb*ws^1*(V"objectList")),

			-- [7]predicateObjectList::=verb JB:ws+ objectList JB:ws*
			--                          ( ';' JB:ws* verb JB:ws+ objectList JB:ws* )* ( ';')?
			predicateObjectList = Cg(Ct(V"predicateObject"*ws^0*
										   (P";"*ws^0*V"predicateObject"*ws^0)^0*P";"^-1), "preds"),

			-- [22]itemList::=object+
			itemList = V"object"^1,

			-- [23]collection::='(' JB:ws* itemList? JB:ws* ')'
			collection = P"("*ws^0*V"itemList"^-1*ws^0*P")"
   }
end

----------------
local object = makeGrammar("object")
-- testing Only
local objectList = makeGrammar("objectList")

local blank = makeGrammar("blank")

local predicateObjectList = makeGrammar("predicateObjectList")
----------------

-- [11]subject::=resource | blank
local subject = Cg(resource+blank, "subject")

-- [6]triples::=subject JB:ws+ predicateObjectList
local triple = Ct(subject*ws^1*predicateObjectList)

-- [5]base::='@base' ws+ uriref
local base = Cf(P"@base"*ws^1*uriref, function (uri)
				   return {type='Base',
						   uri=uri.uri}
end)

-- [4]prefixID::='@prefix' ws+ prefixName? ':' JB:ws+ uriref
local namedPrefixID = Cf(P"@prefix"*ws^1*C(prefixName)*P":"*ws^1*uriref,
					function (name, uri)
					   return {type="Prefix",
							   name=name,
							   uri=uri.uri}
end)
local blankPrefixID = Cf(P"@prefix"*ws^1*Cc('')*P":"*ws^1*uriref,
					function (name, uri)
					   return {type="Prefix",
							   name=name,
							   uri=uri.uri}
end)
local prefixID = namedPrefixID+blankPrefixID

-- [3]directive::=prefixID | base
local directive = prefixID+base

-- [2]statement::=directive '.' | triple '.' | ws+
local statement = ((directive+triple)*ws^0*P".")+ws^1

-- [1]turtleDoc::=statement*
local turtleDoc = statement^0

function serializeTerm(term)
   if term == "a" then
	  return "a"
   elseif type(term) ~= "table" then
	  return string.format("\"%s\"", term)
   end

   if term.type == "UriRef" then
	  return "<" .. term.uri .. ">"
   elseif term.type == "Qname" then
	  return term.prefix .. ":" .. term.name
   else
	  require('pl.pretty').dump(term)
	  error("Unable to serialize term")
   end
end

function serializePredObj(po)
   local pred, obj
   pred = serializeTerm(po.verb)
   obj = ""

   for i3, o in ipairs(po.objects) do
	  if i3 > 1 then
		 obj = obj .. ", "
	  end
	  obj = obj .. serializeTerm(o)
   end

   if po.objects.preds then
	  if obj ~= "" then
		 obj = obj .. ", "
	  end
	  obj = obj .. "["
	  for i2, p in ipairs(po.objects.preds) do
		 if i2 > 1 then
			obj = obj .. "; "
		 end
		 obj = obj .. serializePredObj(p)
	  end
	  obj = obj .. "]"
   end

   return pred .. " " .. obj
end

function serialize(rdfDoc)
   local ser = ""
   for idx, elem in ipairs(rdfDoc) do
	  if elem.type == "Prefix" then
		 ser = ser ..string.format("@prefix %s: <%s>.\n", elem.name, elem.uri)
	  elseif elem.type == "Base" then
		 -- TODO
	  else
		 local subj, po
		 -- statement
		 subj = serializeTerm(elem.subject)
		 for i2, p in ipairs(elem.preds) do
			po = serializePredObj(p)

			ser = ser ..string.format("%s %s.\n", subj, po)
		 end
	  end
   end
   return ser
end

local test1 = [[
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix dc: <http://purl.org/dc/elements/1.1/> .
@prefix ex: <http://example.org/stuff/1.0/> .

ex:jess a ex:NonExistingClass ; a ex:AnotherClass .
ex:jess2 a ex:NonExistingClass , ex:AnotherClass .
<http://www.w3.org/TR/rdf-syntax-grammar>
  dc:title "RDF/XML Syntax Specification (Revised)" ;
  ex:editor [
    ex:fullname "Dave Beckett";
    ex:homePage <http://purl.org/net/dajobe/>
  ] .
]]

local test2 = [[
@prefix ericFoaf: <http://www.w3.org/People/Eric/ericP-foaf.rdf#> .
@prefix : <http://xmlns.com/foaf/0.1/> .

ericFoaf:ericP :givenName "Eric" ;
              :knows <http://norman.walsh.name/knows/who/dan-brickley> ,
                      [ :mbox <mailto:timbl@w3.org> ] ,
                      <http://getopenid.com/amyvdh> .
]]

-- TODO collection support
local test3 = [[
@prefix : <http://example.org/stuff/1.0/> .
:a :b ( "apple" "banana" ) .
]]

local test4 = [[
@prefix : <http://example.org/stuff/1.0/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
:a :b
  [ rdf:first "apple";
    rdf:rest [ rdf:first "banana";
               rdf:rest rdf:nil ]
  ] .
]]

local test5 = [[
@prefix : <http://example.org/stuff/1.0/> .

:a :b "1The first line\nThe second line\n  more" .

:a :b """2The first line
The second line
  more""" .
]]

if false then
   require('pl.pretty').dump({lpeg.match(turtleDoc, test1)})
   print("-------------------")
   require('pl.pretty').dump({lpeg.match(turtleDoc, test2)})
   print("-------------------")
   require('pl.pretty').dump({lpeg.match(turtleDoc, test3)})
   print("-------------------")
   require('pl.pretty').dump({lpeg.match(turtleDoc, test4)})
   print("-------------------")
   require('pl.pretty').dump({lpeg.match(turtleDoc, test5)})
   print("-------------------")


   -- test by running the serialized version back through the parser
   print(serialize({lpeg.match(turtleDoc, serialize({lpeg.match(turtleDoc, test1)}))}))
   print(serialize({lpeg.match(turtleDoc, serialize({lpeg.match(turtleDoc, test2)}))}))
   print(serialize({lpeg.match(turtleDoc, serialize({lpeg.match(turtleDoc, test3)}))}))
   print(serialize({lpeg.match(turtleDoc, serialize({lpeg.match(turtleDoc, test4)}))}))
   print(serialize({lpeg.match(turtleDoc, serialize({lpeg.match(turtleDoc, test5)}))}))
end

function turtleparse.parse(turtleString)
   return {lpeg.match(turtleDoc, turtleString)}
end

return turtleparse
