local turtleparse = require("turtleparse")
local dump = require('pl.pretty').dump

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
							 --dump(s)
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
