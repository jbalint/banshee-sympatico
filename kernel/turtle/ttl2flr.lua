-- convert turtle ontologies to F-Logic/Flora-2
local ttl2flr = {}

local turtleparse = require("turtleparse")

local __dump = require("pl.pretty").dump

-- no default prefix support in Flora-2, so we save it here and
-- substitute it upon encountering it
local __DEFAULT_PREFIX_URI

-- prefix index necessary to expand Qnames with a prefix only and no
-- name
local __PREFIXES = {}

local print = print

local function __printPrefix(p)
   if p.name == "" then
	  __DEFAULT_PREFIX_URI = p.uri
   else
	  __PREFIXES[p.name] = p.uri
	  print(string.format(":- iriprefix{%s='%s'}.", p.name, p.uri))
   end
end

-- convert a resource (UriRef, Qname) string value for Flora-2
local function __rsrc2str(r)
   if r.type == "UriRef" then
	  return string.format("\"%s\"^^\\iri", r.uri)
   elseif r.type == "Qname" then
	  local n = r.name
	  -- prefix only and no name
	  if n == "" then
		 assert(__PREFIXES[r.prefix], "Prefix must be defined: " .. r.prefix)
		 return string.format("\"%s\"^^\\iri", __PREFIXES[r.prefix])
	  -- dcterms has "dcterms:ISO639-2"
	  elseif n:find("-") then -- TODO make this more robustly handle forbidden chars in F-atoms
		 n = "'" .. n .. "'"
	  end
	  if r.prefix == "" then
		 assert(__DEFAULT_PREFIX_URI, "Default prefix encountered, but none defined")
		 return string.format("\"%s%s\"^^\\iri", __DEFAULT_PREFIX_URI, n)
	  end
	  return string.format("%s#%s", r.prefix, n)
   elseif r.type == "Blank" then
	  -- TODO change this
	  -- just use them as oids for now
	  -- note, this is currently acceptable because we are translating
	  -- one stardog export with unique bnode ids
	  --return string.format("\\#[bnodeId->%s]", r.nodeID)
	  return r.nodeID
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
	  -- we should *ONLY* emit "string" objects, not charlist
	  return '"' .. o:gsub('"', '\\"') .. '"^^\\string'
   elseif o.type == "TypedString" then
	  local t
	  local v = o.value:gsub('"', '\\"')
	  if o.datatype.type == "UriRef" then
		 t = o.datatype.uri:gsub("http://www.w3.org/2001/XMLSchema", "xsd")
	  elseif o.datatype.type == "Qname" then
		 t = string.format("%s#%s", o.datatype.prefix, o.datatype.name)
	  else
		 __dump(o)
		 error("Unknown datatype type")
	  end
	  -- Flora doesn't like Z at the end of dates
	  if t:match("#date$") then
		 v = v:gsub("Z$", "")
	  end
	  return string.format("\"%s\"^^%s", v, t)
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

if not pcall(getfenv, 4) then
   -- run script
   local infile = arg[1]
   local outfile = arg[2]
   if not arg[1] or not arg[2] then
	  print("Turtle to Flora translator")
	  print("Argument 1: input file")
	  print("Argument 2: output file")
	  return
   end
   local f = io.open(infile, "r")
   local content = f:read("*all")
   f:close()

   local out = io.open(outfile, "w")
   print = function (x)
	  out:write(x)
	  out:write("\n")
   end

   local s = turtleparse.parse(content)

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
				  -- represent class membership via an RDF triple,
				  -- handle the Flora class membership via rules
				  print(string.format("%s[\"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\"^^\\iri -> %s].",
									  sub, __rsrc2str(obj)))
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
