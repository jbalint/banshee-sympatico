-- convert turtle ontologies to F-Logic/Flora-2
local ttl2flr = {}

local turtleparse = require("turtleparse")

local __dump = require("pl.pretty").dump

local function __printPrefix(p)
   print(string.format(":- iriprefix{%s='%s'}.", p.name, p.uri))
end

-- convert a resource (UriRef, Qname) string value for Flora-2
local function __rsrc2str(r)
   if r.type == "UriRef" then
	  return string.format("\"%s\"^^\\iri", r.uri)
   elseif r.type == "Qname" then
	  local n = r.name
	  -- dcterms has "dcterms:ISO639-2"
	  if n:find("-") then -- TODO make this more robustly handle forbidden chars in F-atoms
		 n = "'" .. n .. "'"
	  end
	  return string.format("%s#%s", r.prefix, n)
   else
	  __dump(r)
	  error("Unknown resource")
   end
end

-- convert an object (resource or literal (TypedString)) to a string
-- value for Flora-2
local function __obj2str(o)
   -- TODO need proper string processing
   if type(o) == "string" then
	  return '"' .. o:sub('"', '\\"') .. '"'
   elseif r.type == "TypedString" then
	  local t
	  if r.datatype.type == "UriRef" then
		 t = r.datatype.uri:gsub("http://www.w3.org/2001/XMLSchema", "xsd")
	  elseif r.datatype.type == "Qname" then
		 t = string.format("%s#%s", r.datatype.prefix, r.datatype.name)
	  else
		 __dump(r)
		 error("Unknown datatype type")
	  end
	  return string.format("\"%s\"^^%s", r.value, t)
   else
	  return __rsrc2str(o)
   end
end

if true then
   -- run script
   --local f = io.open("/home/jbalint/Dropbox/important/org/rdf/other/rdf.ttl", "r")
   local f = io.open("/home/jbalint/Dropbox/important/org/rdf/other/vcard.ttl", "r")
   local content = f:read("*all")
   f:close()
   local s = turtleparse.parse(content)
   __dump(s)

   for idx, el in ipairs(s) do
	  if el.type == "Prefix" then
		 __printPrefix(el)
	  elseif el.subject then -- a statement
		 print("")
		 local sub = __rsrc2str(el.subject)
		 for idx2, pred in ipairs(el.preds) do
			local verb = pred.verb
			for idx3, obj in ipairs(pred.objects) do
			   if pred.verb == "a" then
				  print(string.format("%s:%s.", sub, __rsrc2str(obj)))
			   else
				  print(string.format("%s[%s -> %s].", sub, __rsrc2str(verb), __obj2str(obj)))
			   end
			end
			if pred.objects.preds then
			   error("NESTED OBJECTS")
			end
		 end
	  end
   end
end

return ttl2flr
