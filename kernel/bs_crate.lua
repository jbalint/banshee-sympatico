local crate = {}

--local turtle = require("turtle")
local crate_turtle = require("crate_turtle")

function crate.toFloraIri(x)
   return string.format("i\b%s", x)
end

function crate.toFloraString(x)
   return string.format("s\b%s", x)
end

function crate.encodeString(str)
   return string.format("'%s'", crate.toFloraString(str:gsub("'", "''")))
end

crate._validFloraTypes = {["date"] = true,
   ["dateTime"] = true,
   ["duration"] = true}
-- get the flora representation for a date/time-type value.
function crate._getFloraRepresentation(str, typename)
   if str:find("Z$") then -- Flora doesn't like trailing Zs
	  str = str:gsub("Z$", "")
   end
   if not crate._validFloraTypes[typename] then
	  error("Cannot encode types of " .. typename)
   end
   local q = string.format("flrdatatype_parse:flora_parse_datatype(datatype('\\%s', \"%s\"), noIndex, ParsedDt, Status).", typename, str)
   local r = xsb_query(q, ".")
   if not r or #r ~= 1 or r[1]:find(".%[%]") ~= (#r[1] - 2) or #r[1] < 4 then
	  local msg = "<no message>"
	  if r and r[1] then
		 msg = r[1]
	  end
	  error(string.format("Cannot encode value: `%s' to %s: %s", str, typename, msg))
   end
   return string.format("'\\datatype'(%s, '\\%s')",
						r[1]:gsub(".%[%]", ""), typeName)
end

function crate._objectToString(o)
   local objectString
   if o.nodeType and o:nodeType() == "Collection" then
	  objectString = "["
	  for _idx, obj in ipairs(o) do
		 if objectString == "[" then
			objectString = objectString .. crate._objectToString(obj)
		 else
			objectString = objectString .. "," .. crate._objectToString(obj)
		 end
	  end
	  objectString = objectString .. "]"
   elseif type(o) == "string" and o:find("bnode_") ~= 1 then
	  objectString = "'" .. crate.toFloraIri(o) .. "'"
   elseif type(o) == "string" and o:find("bnode_") == 1 then
	  objectString = o
   elseif o.nodeType and o:nodeType() == "TypedString" then
	  if o.datatype.iri:find("#string") then
		 objectString = crate.encodeString(o.value)
	  elseif o.datatype.iri:find("#boolean") then
		 objectString = string.format("'\\%s'", o.value)
	  elseif o.datatype.iri:find("#integer") then
		 objectString = o.value
	  elseif o.datatype.iri:find("#double") then
		 if not o.value:find("%.") then
			objectString = o.value .. ".0" -- double-typed integer 1 -> 1.0
		 else
			objectString = o.value
		 end
	  elseif o.datatype.iri:find("#dateTime") then
		 -- TODO we have to canonicalize the representation BEFORE
		 -- this point so we can handle the comparison when the value
		 -- is inserted/deleted. Same for date, etc
		 objectString = crate._getFloraRepresentation(o.value, "dateTime")
		 o.floraString = objectString
	  elseif o.datatype.iri:find("#date") then
		 objectString = crate._getFloraRepresentation(o.value, "date")
		 o.floraString = objectString
	  elseif o.datatype.iri:find("#gYear") then
		 -- TODO what to do with this? Is there a real, practical
		 -- possibility of making it a proper Flora type that will be
		 -- useful?
		 objectString = o.value -- as integer for now
	  elseif o.datatype.iri:find("#duration") then
		 objectString = crate._getFloraRepresentation(o.value, "duration")
		 o.floraString = objectString
	  elseif o.datatype.iri:find("#anyURI") then
		 objectString = "'" .. crate.toFloraIri(o) .. "'"
	  else
		 error("Unknown datatype: " .. o.datatype.iri)
	  end
   elseif o.nodeType and o:nodeType() == "IriRef" then
	  objectString = "'" .. crate.toFloraIri(o) .. "'"
   else
	  _dump(o)
	  _dump(getmetatable(o))
	  error("UNKNOWN OBJECT TYPE")
   end
   if not objectString then
	  _dump(o)
	  error("Failed to encode")
   end
   return objectString
end

-- we generate a query like: (a[b->d])
-- flrstoragebase:flora_db_insert_base('_$_$_flora''fdbtrie''main', '_$_$_flora''mod''main''mvd'(a,b,d,_h0)).
function crate.assert(s, p, o)
   p = crate.toFloraIri(p)

   local storageName = "'_$_$_flora''fdbtrie''main'"
   local objectString = crate._objectToString(o)

   -- the actual term we will insert
   local term = string.format("'_$_$_flora''mod''main''mvd'('%s','%s',%s,_h0)",
							  s, p, objectString)
   -- the insert statement
   local addQuery = string.format("flrstoragebase:flora_db_insert_base(%s, %s).",
								  storageName, term)
   xsb_query(addQuery, "")
end

function crate.load(filename)
   local bnodeCount, spoCount = 0, 0
   local s = crate_turtle.parseFile("/home/jbalint/sw/banshee-sympatico/tmp/bs_stardog-export-20140711_16222_tbc.ttl")
   for name, bnode in pairs(s.bnodes) do
	  for predIri, pred in pairs(bnode) do
		 for _idx, obj in ipairs(pred.objects or {}) do
			crate.assert(name, predIri, obj)
			bnodeCount = bnodeCount + 1
		 end
	  end
   end
   for name, sub in pairs(s.spo) do
	  for predIri, pred in pairs(sub) do
		 for _idx, obj in ipairs(pred.objects or {}) do
			crate.assert(crate.toFloraIri(name), predIri, obj)
			spoCount = spoCount + 1
		 end
	  end
   end
   print(string.format("Loaded %d facts (bnode=%d, spo=%d)",
					   bnodeCount + spoCount, bnodeCount, spoCount))
end

function crate.insert()
   error("NOT IMPLEMENTED")
end

function crate.delete()
   error("NOT IMPLEMENTED")
end

return crate
