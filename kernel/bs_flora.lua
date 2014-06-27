local bs_flora = {}

-- Flora's internal prefix coding for IRIs
local function bs_flora_iri_to_string(iri)
   return iri:gsub("^i\x08", "")
end

function bs_flora.get_prefixes()
   local qres = xsb_query("flora_query(\"prefix{?X, ?Y}.\", [\"?X\"=X,\"?Y\"=Y],_,_,_).", " ")
   local prefixes = {}
   for idx, str in ipairs(qres) do
	  local i = str:find(" ")
	  local prefix = str:sub(1, i - 1)
	  local uri = str:sub(i + 1, -1 + str:find(" ", i + 1))
	  prefixes[prefix] = bs_flora_iri_to_string(uri)
   end
   return prefixes
end

return bs_flora
