local flora = {}

-- Flora's internal prefix coding for IRIs
local function _flora_iri_to_string(iri)
   return iri:gsub("^i\x08", "")
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
   local qres = xsb_query(string.format("flora_query(\"%s\", [\"?X\"=X,\"?Y\"=Y],_,_,_).", query), "\x03")
   local propvals = {}
   for idx, str in ipairs(qres) do
	  local i = str:find("\x03")
	  local prop = str:sub(1, i - 1)
	  local val = str:sub(i + 1, -1 + str:find("\x03", i + 1))
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

return flora
