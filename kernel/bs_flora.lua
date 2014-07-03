local flora = {}

local sep = "\x03"

-- Flora's internal prefix coding for IRIs
local function _flora_iri_to_string(iri)
   -- () -> return only the first value
   return (iri:gsub("^i\x08", ""))
end

local
function _parse_xsb_output(str, fields)
   local i = 1
   local j = str:find(sep)

   local vals = {}
   for num = 1, fields do
	  table.insert(vals, str:sub(i, j - 1))
	  i = j + 1
	  j = str:find(sep, i)
	  if not j and num < fields then
		 error("Not enough values available for parsing")
	  end
   end
   return table.unpack(vals)
end

function flora.getprefixes()
   local qres = xsb_query("flora_query(\"prefix{?X, ?Y}.\", [\"?X\"=X,\"?Y\"=Y],_,_,_).", " ")
   local prefixes = {}
   for idx, str in ipairs(qres) do
	  local i = str:find(" ")
	  local prefix = str:sub(1, i - 1)
	  local uri = str:sub(i + 1, -1 + str:find(" ", i + 1))
	  prefixes[prefix] = _flora_iri_to_string(uri)
   end
   return prefixes
end

function flora.getproperties(objname)
   local query = string.format("%s[?X -> ?Y].", objname)
   local qres = xsb_query(string.format("flora_query(\"%s\", [\"?X\"=X,\"?Y\"=Y],_,_,_).", query), sep)
   local propvals = {}
   for idx, str in ipairs(qres) do
	  local prop, val = _parse_xsb_output(str, 2)
	  if propvals[prop] then
		 -- handle sets dynamically
		 if type(propvals[prop]) ~= "table" then
			local vals = {}
			table.insert(vals, propvals[prop])
			propvals[prop] = vals
		 end

		 table.insert(propvals[prop], val)
	  else
		 propvals[prop] = val
	  end
   end
   return propvals
end

function flora.classes(objname)
   -- we only consider IRIs as classes, not other values
   local query = string.format("%s:?X^^\\iri.", objname)
   local qres = xsb_query(string.format("flora_query(\"%s\", [\"?X\"=X],_,_,_).", query), sep)
   local classes = {}
   local _dupcheck = {}
   for idx, str in ipairs(qres) do
	  if not _dupcheck[str] then
		 table.insert(classes, _flora_iri_to_string(_parse_xsb_output(str, 1)))
		 _dupcheck[str] = 1
	  end
   end
   return classes
end

function flora.instances(classname)
   -- TODO generalize with classes() method
   local query = string.format("?X^^\\iri:%s.", classname)
   local qres = xsb_query(string.format("flora_query(\"%s\", [\"?X\"=X],_,_,_).", query), sep)
   local instances = {}
   -- we need to remove identical answers from the Flora query
   -- c.f. flranswer.P, flora_write_matches,
   -- flora_skip_identical_answers
   local _dupcheck = {}
   for idx, str in ipairs(qres) do
	  if not _dupcheck[str] then
		 table.insert(instances, _flora_iri_to_string(_parse_xsb_output(str, 1)))
		 _dupcheck[str] = 1
	  end
   end
   return instances
end

function flora.shell()
   xsb_command("flora_shell.")
end

return flora
