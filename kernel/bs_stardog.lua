-- module to interact with stardog
local stardog = {}

local counter = 0

-- TODO configurable paths (even though this module is supposed to be
-- temporary)
local stardog_executable = "/home/jbalint/sw/stardog-2.1.3/bin/stardog"

local bstmp = "/home/jbalint/sw/banshee-sympatico/tmp"

function _sparql_quote_string(s)
   s = s:gsub("\\", "\\\\")
   s = s:gsub("\"", "\\\"")
   return string.format('"%s"^^xsd:string', s)
end

function _writeq(q)
   local filename = string.format("%s/bs_stardog-q-%s-%s.log",
								  bstmp, os.date("%Y%m%d_%H%M"), counter)
   counter = counter + 1
   local f = io.open(filename, "a")
   assert(f, "Cannot open log")
   f:write(q)
   f:close()
   return filename
end

function _execute(filename)
   local cmd = string.format("%s query execute banshee-sympatico %s",
							 stardog_executable, filename)
   local proc = io.popen(cmd, "r")
   for i in proc:lines() do
	  if i ~= "Update query processed successfully." then
		 proc:close()
		 error("Error from Stardog: " .. tostring(i) .. " during query " .. tostring(filename))
	  end
   end
   proc:close()
end

function _resource_tostring(r)
   local class = getmetatable(r).classname
   if not class or
	  not (class == "boxinglove.reference" or class == "boxinglove.instance") then
		 require("pl.pretty").dump(r)
		 error("Invalid resource")
   end
   return string.format("%s:%s",
						r._boxinglove_ns._boxinglove_prefix,
						r._boxinglove_name)
end

function _mutate(action, s, p, o)
   local subject = _resource_tostring(s)
   local predicate = _resource_tostring(p)

   -- object serialization
   local object
   if type(o) == "string" then
	  object = _sparql_quote_string(o)
	  -- TODO numbers
   elseif type(o) == "table" and
	  (getmetatable(o).classname == "boxinglove.reference" or
	   getmetatable(o).classname == "boxinglove.instance") then
		 object = string.format("%s:%s", o._boxinglove_ns._boxinglove_prefix, o._boxinglove_name)
   else
	  require("pl.pretty").dump(o)
	  error("Invalid object")
   end

   local q = string.format("%s data { %s %s %s }", action, subject, predicate, object)

   local filename = _writeq(q)
   _execute(filename)
end

function stardog.delete(s, p, o)
   _mutate("delete", s, p, o)
end

function stardog.insert(s, p, o)
   _mutate("insert", s, p, o)
end

function stardog.export()
   local filename = string.format("%s/bs_stardog-export-%s.ttl",
								  bstmp, os.date("%Y%m%d_%H%M"), counter)
   local cmd = string.format("%s data export banshee-sympatico %s",
							 stardog_executable, filename)
   local proc = io.popen(cmd, "r")
   -- Exported data as Turtle to file 'export-2014-07-01-2021.ttl' in 888.9 ms.
   for i in proc:lines() do
	  if not i:gmatch("Exported data as Turtle") then
		 proc:close()
		 error("Error during Stardog export: " .. tostring(i))
	  end
   end
   proc:close()
   return filename
end

return stardog
