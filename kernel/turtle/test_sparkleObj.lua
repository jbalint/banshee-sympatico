local sparkleObj = require("sparkleObj")
local dump = require("pl.pretty").dump

-- TODO parameterize the SPARQL endpoint
sparkleObj.init{sparqlEndpointUrl="http://localhost:5820/banshee-sympatico/query"}

local rdfs = sparkleObj.ns.rdfs
local bstest = sparkleObj.ns.bstest

describe("sparkleObj", function ()
			context("namespace access", function ()
					   it("should allow basic namespace access", function ()
							 local bstest = sparkleObj.ns.bstest
							 assert_equal("bstest", sparkleObj.namespacePrefix(bstest))
							 assert_equal("http://banshee-sympatico/test#", sparkleObj.namespaceUri(bstest))
							 assert_equal("namespace", sparkleObj.typeof(bstest))
					   end)
			end)

			context("missing object errors", function ()
					   it("should signal an error when objects are not found", function()
							 local attempt = function ()
								local x = bstest.NONEXISTINGOBJECT
							 end
							 local res, err = pcall(attempt)
							 assert_equal(false, res)
							 assert_match("Cannot load bstest:NONEXISTINGOBJECT", err)
					   end)
			end)

			context("basic object operations", function ()
					   it("should access existing instances", function ()
							 local testClass = bstest.SomeTestClass
							 assert_equal("bstest:SomeTestClass", tostring(testClass))
							 assert_equal("SomeTestClass", sparkleObj.objectName(testClass))
							 assert_equal("Some Test Class", tostring(testClass[rdfs.label]))
							 assert_equal("owl:Thing", tostring(testClass[rdfs.subClassOf]))
					   end)
			if true then return end
					   it("should create a new object and delete it", function ()
							 local x = sparkleObj.createObject(bstest, "SparkleTestObject1",
															   bstest.SomeTestClass)
							 assert_equal("bstest:SparkleTestObject1", tostring(x))
							 assert_equal("SparkleTestObject1", sparkleObj.objectName(x))
							 -- reload it
							 x = bstest.SparkleTestObject1
							 assert_equal("bstest:SparkleTestObject1", tostring(x))
							 assert_equal("SparkleTestObject1", sparkleObj.objectName(x))
							 sparkleObj.deleteObject(x)
					   end)
			end)

			context("qname string properties and assignments", function ()
					   -- TODO
			end)
end)
