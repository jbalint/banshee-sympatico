local turtleparse = require("turtleparse")
local _dump = require('pl.pretty').dump

-- assigned locally in test, pass here before calling external
-- functions
local Massert_equal

local function testPrefix(s, uri, name)
   Massert_equal("Prefix", s.type)
   Massert_equal(name, s.name)
   Massert_equal(uri, s.uri)
end

local function testQname(s, prefix, name)
   Massert_equal("Qname", s.type)
   Massert_equal(prefix, s.prefix)
   Massert_equal(name, s.name)
end

local function testUriRef(s, uri)
   Massert_equal("UriRef", s.type)
   Massert_equal(uri, s.uri)
end

local function testParseAndSerialize(ttl_string)
   local s = turtleparse.parse(ttl_string)
   Massert_equal("table", type(s[1]), "parse should produce a list of tables")
   local serialized = turtleparse.serialize(s)
   Massert_equal(ttl_string, serialized)
end

describe("turtleparse", function ()

			context("strings", function ()
					   it("should parse simple strings", function ()
							 local s = turtleparse.parse('test:X a "abc".')
							 assert_equal("abc", s[1].preds[1].objects[1])
					   end)
					   it("should un-escape embedded escaped quotes", function ()
							 local s = turtleparse.parse('test:X a "a\\\"bc".')
							 assert_equal("a\"bc", s[1].preds[1].objects[1])
					   end)
			end)

			context("long strings", function ()
					   it("should parse long strings", function ()
							 local s = turtleparse.parse([[test:X a """blablabla""".]])
							 assert_equal("blablabla", s[1].preds[1].objects[1])
					   end)
					   it("should process escapes in long strings", function ()
							 -- \n appears literally when inside [[ ... ]]
							 local s = turtleparse.parse([[test:X a """bla\nbla\nbla""".]])
							 assert_equal("bla\nbla\nbla", s[1].preds[1].objects[1])
					   end)
					   it("should unescape and support two quotes", function ()
							 local s = turtleparse.parse([[test:X a """hey \"BOY\"z he said "hi" two quotes "" cool """.]])
							 assert_equal("hey \"BOY\"z he said \"hi\" two quotes \"\" cool ", s[1].preds[1].objects[1])
					   end)
					   it("should process other escapes and handle single quotes", function ()
							 local s = turtleparse.parse([[test:X a """hey \"BOY"z ''" bsaid
two \tquotes\ncool """.]])
							 assert_equal("hey \"BOY\"z ''\" bsaid\ntwo 	quotes\ncool ", s[1].preds[1].objects[1])
					   end)
					   it("support many quotes", function ()
							 local s = turtleparse.parse([[test:X a """string inside a string -> ""\"x""\"""".]])
							 assert_equal("string inside a string -> \"\"\"x\"\"\"", s[1].preds[1].objects[1])
					   end)
			end)

			context("long string specific tests", function ()
					   -- TODO support single quote long string in parser
					   -- <s> <p> ''' ''\' ''' .
					   it("three quotes", function ()
							 local s = turtleparse.parse([[<s> <p> """ ""\" """ .]])
							 assert_equal(" \"\"\" ", s[1].preds[1].objects[1])
					   end)
					   -- TODO support unicode escapes in parser
					   -- <s> <p> """ ""\u0061 """ .
					   -- <s> <p> """""\u0061""" .
					   it("three quotes no space", function ()
							 local s = turtleparse.parse([[<s> <p> """""\"""" .]])
							 assert_equal("\"\"\"", s[1].preds[1].objects[1])
					   end)
			end)

			-- language tags are recognized by the parser, but not
			-- included in the parse tree output
			context("language tags", function ()
					   it("should parse simple language tags", function ()
							 local s = turtleparse.parse([[:a :b "abc"@ru.]])
							 assert_equal("abc", s[1].preds[1].objects[1])
					   end)
					   it("should parse all language tags", function ()
							 -- fixed this issue on dc-1.1.ttl, which
							 -- uses en-US

							 -- the turtle grammar I used didn't
							 -- define the second part as supporting
							 -- uppercase letters
							 local s = turtleparse.parse([[:a :b "abcXYZ"@en-US.]])
							 assert_equal("abcXYZ", s[1].preds[1].objects[1])
					   end)
			end)

			-- basic collection support, this is not decoded in
			-- RDF-list style by the turtle parser
			context("collections", function ()
					   it("should parse basic collections", function ()
							 local s = turtleparse.parse([[:a :b ( "apple" "banana" ) .]])
							 assert_equal("Collection", s[1].preds[1].objects[1].type)
							 assert_equal(2, #s[1].preds[1].objects[1].values)
							 assert_equal("apple", s[1].preds[1].objects[1].values[1])
							 assert_equal("banana", s[1].preds[1].objects[1].values[2])
					   end)
					   it("should parse collections with qnames and urirefs", function ()
							 Massert_equal = assert_equal
							 local s = turtleparse.parse([[:a :b ( :apple "banana" <http://example.org/stuff/1.0/Pear> ) .]])
							 assert_equal("Collection", s[1].preds[1].objects[1].type)
							 assert_equal(3, #s[1].preds[1].objects[1].values)
							 testQname(s[1].preds[1].objects[1].values[1], "", "apple")
							 assert_equal("banana", s[1].preds[1].objects[1].values[2])
							 testUriRef(s[1].preds[1].objects[1].values[3], "http://example.org/stuff/1.0/Pear")
							 -- end
							 Massert_equal = nil
					   end)
			end)

			context("miscellaneous", function ()
					   it("should parse prefix-only qnames", function ()
							 Massert_equal = assert_equal
							 local s = turtleparse.parse([[a: :b b:.]])
							 assert_equal(1, #s)
							 assert_equal(1, #s[1].preds)
							 assert_equal(1, #s[1].preds[1].objects)
							 testQname(s[1].subject, "a", "")
							 testQname(s[1].preds[1].verb, "", "b")
							 testQname(s[1].preds[1].objects[1], "b", "")
							 -- end
							 Massert_equal = nil
					   end)
			end)

			-- TODO: serialization tests are very incomplete
			context("serialization", function ()
					   it("should serialize unlabeled bnodes", function ()
							 -- we just use the same string and test
							 -- the parse+serialization is identical
							 -- to the original
							 -- (needs the newline as the serializer
							 -- adds it)
							 local ttl_string = 'bsbase:Clear bsbase:homepage [rdf:type bibo:Webpage; bibo:uri "http://frdcsa.org/frdcsa/internal/clear/"^^xsd:string].\n'
							 Massert_equal = assert_equal
							 testParseAndSerialize(ttl_string)
							 Massert_equal = nil
					   end)
					   it("should handle embedded quotes", function ()
							 local ttl_string = 'bstest:xyz skos:editorialNote "based on \\\"Heinz Steals the Drug\\\""^^xsd:string.\n'
							 Massert_equal = assert_equal
							 testParseAndSerialize(ttl_string)
							 Massert_equal = nil
					   end)
					   it("should handle lists", function ()
							 local ttl_string = 'bstest:something bstest:hasThese ( bstest:a bstest:b bstest:c ).\n'
							 Massert_equal = assert_equal
							 testParseAndSerialize(ttl_string)
							 Massert_equal = nil
					   end)
			end)

			context("document parsing", function ()
					   it("should parse test from spec", function ()
							 Massert_equal = assert_equal
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
							 local s = turtleparse.parse(test1)
							 -- uncomment this to print the full table structure
							 --_dump(s)
							 assert_equal(6, #s)

							 -- first, prefixes
							 testPrefix(s[1], "http://www.w3.org/1999/02/22-rdf-syntax-ns#", "rdf")
							 testPrefix(s[2], "http://purl.org/dc/elements/1.1/", "dc")
							 testPrefix(s[3], "http://example.org/stuff/1.0/", "ex")

							 -- statement: ex:jess a ex:NonExistingClass ; a ex:AnotherClass .
							 local stmt = s[4]
							 testQname(stmt.subject, "ex", "jess")
							 assert_equal(2, #stmt.preds)

							 local pred = stmt.preds[1]
							 assert_equal("a", pred.verb)
							 assert_equal(1, #pred.objects)
							 testQname(pred.objects[1], "ex", "NonExistingClass")

							 pred = stmt.preds[2]
							 assert_equal("a", pred.verb)
							 assert_equal(1, #pred.objects)
							 testQname(pred.objects[1], "ex", "AnotherClass")

							 -- statement: ex:jess2 a ex:NonExistingClass , ex:AnotherClass .
							 stmt = s[5]
							 testQname(stmt.subject, "ex", "jess2")
							 assert_equal(1, #stmt.preds)
							 pred = stmt.preds[1]
							 assert_equal("a", pred.verb)
							 assert_equal(2, #pred.objects)
							 testQname(pred.objects[1], "ex", "NonExistingClass")
							 testQname(pred.objects[2], "ex", "AnotherClass")

							 -- statement: <http://www.w3.org/TR/rdf-syntax-grammar>
							 --   dc:title "RDF/XML Syntax Specification (Revised)" ;
							 --   ex:editor [
							 --     ex:fullname "Dave Beckett";
							 --     ex:homePage <http://purl.org/net/dajobe/>
							 --   ] .
							 stmt = s[6]
							 testUriRef(stmt.subject, "http://www.w3.org/TR/rdf-syntax-grammar")
							 assert_equal(2, #stmt.preds)
							 pred = stmt.preds[1]
							 assert_equal(1, #pred.objects)
							 testQname(pred.verb, "dc", "title")
							 assert_equal("RDF/XML Syntax Specification (Revised)", pred.objects[1])

							 -- nested
							 pred = stmt.preds[2]
							 assert_equal(0, #pred.objects)
							 testQname(pred.verb, "ex", "editor")
							 assert_equal(2, #pred.objects.preds)
							 local npred = pred.objects.preds[1]
							 testQname(npred.verb, "ex", "fullname")
							 assert_equal(1, #npred.objects)
							 assert_equal("Dave Beckett", npred.objects[1])
							 npred = pred.objects.preds[2]
							 testQname(npred.verb, "ex", "homePage")
							 assert_equal(1, #npred.objects)
							 testUriRef(npred.objects[1], "http://purl.org/net/dajobe/")

							 -- end
							 Massert_equal = nil
					   end)
			end)
end)
