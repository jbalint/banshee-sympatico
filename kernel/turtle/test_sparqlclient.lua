
local sparqlclient = require("sparqlclient")

-- tests require a configured (Stardog) SPARQL endpoint
local testEndpointUrl = "http://localhost:5820/banshee-sympatico/query"

describe("sparqlclient.query()", function ()
			context("basic", function ()
					   it("should execute trivial queries", function ()
							 local ret = sparqlclient.query(testEndpointUrl, "construct { <a> <b> <c> } {}")
							 -- TODO shouldn't be "tag:" here, but I guess it is
							 assert_equal("\n<tag:/a> <tag:/b> <tag:/c> .\n", ret)
					   end)
			end)

			context("error", function ()
					   it("fail on bad connection", function ()
							 local ret, msg = pcall(sparqlclient.query, "http://127.0.0.2/", "")
							 assert_equal(false, ret)
							 -- these error messages might change with luasocket changes
							 assert_match("connection refused", msg)
							 ret, msg = pcall(sparqlclient.query, "http://localhost:2/", "")
							 assert_equal(false, ret)
							 assert_match("closed", msg)
					   end)
					   it("fail on empty or malformed queries", function ()
							 local ret, msg = pcall(sparqlclient.query, testEndpointUrl, "aljskdasd")
							 assert_equal(false, ret)
							 assert_match("Failed to query. rc=400, status=HTTP/1.1 400 Encountered", msg)
							 ret, msg = pcall(sparqlclient.query, testEndpointUrl, "")
							 assert_equal(false, ret)
							 assert_match("Failed to query. rc=406, status=HTTP/1.1 406 Not Acceptable", msg)
					   end)
			end)
end)
