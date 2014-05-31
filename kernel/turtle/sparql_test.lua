
local http = require("socket.http")
local url = require("socket.url")
local mime = require("mime")
local ltn12 = require("ltn12")

local headers = {
   Accept = "application/x-turtle",
   --Accept = "application/json",
   --["Content-Type"] = "application/json",
   Authorization = "Basic " .. (mime.b64("admin:admin"))
-- SD-Connection-String: reasoning=QL
}

local output = {}
local x = http.request{headers = headers,
					   method = "GET",
					   url = "http://localhost:5820/banshee-sympatico/query?query=" .. url.escape("CONSTRUCT { bsbase:KenWilber ?p ?o } WHERE { bsbase:KenWilber ?p ?o }"),
					   --url = "http://localhost:5820/admin/databases/banshee-sympatico/options",
					   --source = ltn12.source.string('{"database.name":false}'),
					   --sink = ltn12.sink.file(io.open("temp-out.ttl", "w"))
					   sink = ltn12.sink.table(output)
}

x = table.concat(output)

print(x)
require('pl.pretty').dump(x)
