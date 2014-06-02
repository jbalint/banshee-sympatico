local sparqlclient = {}

-- http://w3.impa.br/~diego/software/luasocket/http.html
local http = require("socket.http")
local url = require("socket.url")
local mime = require("mime")
local ltn12 = require("ltn12")

sparqlclient.headers = {
   -- may need to change for different requirements
   Accept = "application/x-turtle",
   -- internal for now
   Authorization = "Basic " .. (mime.b64("admin:admin"))
}

function sparqlclient.query(endpointUrl, queryString)
   local output = {}
   local ret, msg, hdrs, status =
	  http.request{headers = sparqlclient.headers,
				   method = "GET",
				   url = endpointUrl .. "?query=" .. url.escape(queryString),
				   sink = ltn12.sink.table(output)
	  }

   if not ret then
	  error(msg)
   elseif msg ~= 200 then
	  error(string.format("Failed to query. rc=%d, status=%s", msg, status))
   end

   return table.concat(output)
end

return sparqlclient
