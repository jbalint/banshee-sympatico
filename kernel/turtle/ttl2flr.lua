-- convert turtle ontologies to F-Logic/Flora-2
local ttl2flr = {}

local turtleparse = require("turtleparse")

local __dump = require("pl.pretty").dump

-- no default prefix support in Flora-2, so we save it here and
-- substitute it upon encountering it
local __DEFAULT_PREFIX_URI

local function __printPrefix(p)
   if p.name == "" then
	  __DEFAULT_PREFIX_URI = p.uri
   else
	  print(string.format(":- iriprefix{%s='%s'}.", p.name, p.uri))
   end
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
	  if r.prefix == "" then
		 assert(__DEFAULT_PREFIX_URI, "Default prefix encountered, but none defined")
		 return string.format("\"%s%s\"^^\\iri", __DEFAULT_PREFIX_URI, n)
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
	  return '"' .. o:gsub('"', '\\"') .. '"'
   elseif o.type == "TypedString" then
	  local t
	  if o.datatype.type == "UriRef" then
		 t = o.datatype.uri:gsub("http://www.w3.org/2001/XMLSchema", "xsd")
	  elseif o.datatype.type == "Qname" then
		 t = string.format("%s#%s", o.datatype.prefix, o.datatype.name)
	  else
		 __dump(o)
		 error("Unknown datatype type")
	  end
	  return string.format("\"%s\"^^%s", o.value, t)
   elseif o.type == "Collection" then
	  local strval = "{"
	  for idx, v in ipairs(o.values) do
		 local strv = __obj2str(v)
		 if strval == "{" then
			strval = strval .. strv
		 else
			strval = strval .. ", " .. strv
		 end
	  end
	  return strval .. "}"
   else
	  return __rsrc2str(o)
   end
end

if true then
   -- run script
   --local f = io.open("/home/jbalint/Dropbox/important/org/rdf/other/rdf.ttl", "r")
   local f = io.open("/home/jbalint/Dropbox/important/org/rdf/nepomuk/nie.ttl", "r")
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
			   if verb == "a" then
				  print(string.format("%s:%s.", sub, __rsrc2str(obj)))
			   else
				  print(string.format("%s[%s -> %s].", sub, __rsrc2str(verb), __obj2str(obj)))
			   end
			end
			if pred.objects.preds then
			   -- A bit more complicated to represent nested objects
			   -- as we can't reference the skolem symbol from several
			   -- statements
			   local nested = "\\#"
			   local preds = "["
			   for idx3, pred2 in ipairs(pred.objects.preds) do
				  for idx4, obj in ipairs(pred2.objects) do
					 if pred2.verb == "a" then
						nested = nested .. ":" .. __rsrc2str(obj)
					 else
						local newpred = string.format("%s -> %s", __rsrc2str(pred2.verb), __obj2str(obj))
						if preds == "[" then
						   preds = preds .. newpred
						else
						   preds = preds .. ", " .. newpred
						end
					 end
				  end
			   end
			   print(string.format("%s[%s -> %s%s].", sub, __rsrc2str(verb), nested, preds .. "]"))
			end
		 end
	  end
   end
end

return ttl2flr
