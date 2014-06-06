local sparkleObj = {
   sparqlEndpointUrl = "<unknown>",
   -- index of namespaces by prefix
   ns = {
	  -- an inverse index of namespaces by URI
	  __byUri = {}
   }
}

local namespace = {
   classname = "sparkleObj.namespace"
}

local literal = {
   classname = "sparkleObj.literal"
}

local objInstance = {
   classname = "sparkleObj.instance"
}

local objReference = {
   classname = "sparkleObj.reference"
}

local valueSet = {
   classname = "sparkleObj.valueset"
}

local sparqlclient = require("sparqlclient")
local turtleparse = require("turtleparse")

local dump = require('pl.pretty').dump

------------------------------
-- internal/support methods --
------------------------------

local __debug = print
local __warn = print

local __rdf_type = nil

--- (INTERNAL) Convert a URI ref to a Qname using the namespace index
-- @param uriRef The UriRef struct
local function __uriRefToQName(uriRef)
   local hash = uriRef.uri:find("#")
   if hash then
	  local baseUri = uriRef.uri:sub(1, hash)
	  local name = uriRef.uri:sub(hash + 1)
	  local ns = sparkleObj.ns.__byUri[baseUri]
	  assert(ns, "uriRef must match prefix")
	  assert(#name > 0, "Name cannot be empty")
	  -- similar format as in turtleparse, but with ns added
	  return {type="Qname",
			  prefix=sparkleObj.namespacePrefix(ns),
			  name=name,
			  ns=ns}
   else
	  dump(uriRef)
	  error("NO HASH")
   end
end

local function __parseValueSet(objects)
   local vset = {}
   local obj
   for idx, robj in ipairs(objects) do
	  if type(robj) == "string" then
		 -- TODO THIS SHOULD BE AN UNTYPED LITERAL
		 obj = literal.create(robj, sparkleObj.ns.xsd.string)
	  elseif robj.type == "UriRef" then
		 -- TODO handle non-hash endings?
		 if robj.uri:find("#") == #robj.uri then
			-- no "name" means we have a prefix/namespace object
			obj = sparkleObj.ns.__byUri[robj.uri]
		 else
			-- normal object reference
			local q = __uriRefToQName(robj)
			obj = objReference.create(q.ns, q.name)
		 end
	  elseif robj.type == "TypedString" then
		 local type = __uriRefToQName(robj.datatype)
		 obj = literal.create(robj.value, type.ns[type.name])
	  else
		 print("-->")
		 print(type(robj))
		 dump(robj)
		 error("Unknown result object type")
	  end
	  table.insert(vset, obj)
   end
   return vset
end

--- (INTERNAL) 
-- TODO document the forms of calling __loadObject()
local function __loadObject(a, b)
   local objName
   if type(a) == "table" and getmetatable(a).classname == "sparkleObj.namespace" then
	  objName = string.format("%s:%s", sparkleObj.namespacePrefix(a), b)
   elseif type(a) == "string" then
	  if a:find(":") then
		 objName = string.format("%s:%s", a, b)
	  else
		 objName = a
	  end
   end
   if not objName then
	  error("Cannot load object without a name")
   end

   -- load object
   local q = [[construct { %s ?p ?o } 
               where { %s ?p ?o }]]
   q = string.format(q, objName, objName)
   __debug("__loadObject: " .. q)
   local s = turtleparse.parse(sparqlclient.query(sparkleObj.sparqlEndpointUrl, q))

   -- TODO for now, only handle one returned object
   assert(1 == #s)
   s = s[1]

   -- transform to instance
   -- Object is of the form:
   -- {subject=, preds=}
   --   subject: UriRef
   --   preds: [{verb=, objects=}]
   --     verb: UriRef
   --     objects: [UriRef | TypedString]
   --//
   local sub = __uriRefToQName(s.subject)
   local obj = objInstance.create(sub.ns, sub.name)
   for idx, pred in ipairs(s.preds) do
	  local verb
	  if pred.verb == "a" then
		 verb = {type="Qname",
				 prefix="rdf",
				 name="type",
				 ns=sparkleObj.ns.rdf}
	  else
		 verb = __uriRefToQName(pred.verb)
	  end
	  local verbName = string.format("%s:%s", verb.prefix, verb.name)
	  local vset = valueSet.create(__parseValueSet(pred.objects))
	  if false then -- DEBUG
		 print(string.format("%s.%s[%s] = %s",
							 sparkleObj.namespacePrefix(sub.ns), sub.name,
							 verbName, tostring(vset)))
	  end
	  rawset(obj, verbName, vset)
   end
   return obj
end

--- (INTERNAL) Initialize the set of namespaces from the database.
-- This function initializes the sparkleObj.ns structure.
local function __initializeNamespaces()
   if #sparkleObj.ns > 1 then
	  error("Namespaces already initialized")
   end

   local q = [[construct { ?s bscode:prefix ?p ; bscode:uri ?u }
               where { ?s a bscode:OntologyPrefix ;
                       bscode:prefix ?p ;
                       bscode:uri ?u ;
               }]]
   local res = sparqlclient.query(sparkleObj.sparqlEndpointUrl, q)
   local s = turtleparse.parse(res)
   for idx, x in ipairs(s) do
	  local prefix, uri
	  for idx2, pred in ipairs(x.preds) do
		 if pred.verb.uri == "http://banshee-sympatico/code#prefix" then
			prefix = pred.objects[1].value
		 elseif pred.verb.uri == "http://banshee-sympatico/code#uri" then
			uri = pred.objects[1].value
		 end
	  end
	  local ns = namespace.create(prefix, uri)
	  sparkleObj.ns[prefix] = ns
	  sparkleObj.ns.__byUri[uri] = ns
   end
end

--- (INTERNAL) Assert that an object is an instance of (one of) the
--- given class(es).
-- An error will be signaled if the object is not an instance.
-- @param obj The object whose class is to be checked.
-- @param classes A class name string, or table with keys as class name strings.
local function __assertClass(obj, classes)
   assert(obj)
   local cl = getmetatable(obj).classname
   if type(classes) == "string" then
	  assert(cl == classes,
			 string.format("Expected class '%s', but found class '%s'", classes, cl))
   else
	  assert(classes[cl],
			 string.format("Expected one of '%s', but found class '%s'", classes, cl))
   end
end

------------------------
-- Sparkle Object API --
------------------------
function sparkleObj.init(props)
   sparkleObj.sparqlEndpointUrl = props.sparqlEndpointUrl
   __initializeNamespaces()
   --__rdfType = sparkleObj.ns.rdf.type
   -- TODO namespace checking/validation
end

function sparkleObj.namespacePrefix(ns)
   __assertClass(ns, "sparkleObj.namespace")
   return rawget(ns, "__sparkle_prefix")
end

function sparkleObj.namespaceUri(ns)
   __assertClass(ns, "sparkleObj.namespace")
   return rawget(ns, "__sparkle_uri")
end

function sparkleObj.objectName(obj)
   __assertClass(obj, {["sparkleObj.instance"]=1,
					   ["sparkleObj.reference"]=1})
   return rawget(obj, "__sparkle_name")
end

function sparkleObj.createObject(ns, name, classes)
   -- TODO
end

function sparkleObj.deleteObject(obj)
   -- TODO
end

function sparkleObj.typeof(obj)
   local mt = getmetatable(obj)
   assert(mt, "Must have a metatable")
   local cl = mt.classname
   return (cl:gsub("sparkleObj%.", ""))
end

-------------------------------------
-- datatype literal implementation --
-------------------------------------
function literal.create(stringValue, type)
   local self = {}
   assert(stringValue, "String value must be provided")
   -- TODO further testing that this is an xsd: TYPE
   assert(type)
   assert(tostring(type):find("xsd:"), "Type must be an XSD type")
   self.stringValue = stringValue
   self.value = stringValue
   setmetatable(self, literal)
   return self
end

function literal:__tostring()
   return rawget(self, "stringValue")
end

function literal:__newindex(k, v)
   -- TODO
end

------------------------------
-- value set implementation --
------------------------------
function valueSet.create(values)
   -- TODO
   local self = {}
   self.values = values
   setmetatable(self, valueSet)
   return self
end

function valueSet:value()
   if #self.values ~= 1 then
	  error("Value set has more than one value")
   end
   return self.values[1]
end

function valueSet:values()
   -- TODO
end

function valueSet:__tostring()
   if #self.values == 1 then
	  return tostring(self.values[1])
   else
   -- TODO
	  return "<>>>>"
   end
end

------------------------------
-- instance implementation --
------------------------------
function objInstance.create(ns, name)
   local self = objReference.create(ns, name)
   setmetatable(self, objInstance)
   return self
end

function objInstance:__tostring()
   return objReference.__tostring(self)
end

function objInstance:__index(k)
   -- TODO necessary?
   return rawget(self, tostring(k))
end

function objInstance:__newindex(k, v)
   -- TODO
end

------------------------------
-- reference implementation --
------------------------------
function objReference.create(ns, name)
   local self = {}
   __assertClass(ns, "sparkleObj.namespace")
   self.__sparkle_ns = ns
   self.__sparkle_name = name
   setmetatable(self, objReference)
   return self
end

function objReference:__tostring()
   return string.format("%s:%s",
						sparkleObj.namespacePrefix(rawget(self, "__sparkle_ns")),
						rawget(self, "__sparkle_name"))
end

function objReference:__index(k)
   error("Object reference cannot be indexed")
end

function objReference:__call()
   -- TODO reification
   return __loadObject(tostring(self))
end

------------------------------
-- namespace implementation --
------------------------------
function namespace.create(prefix, uri)
   local self = {}
   setmetatable(self, namespace)
   self.__sparkle_prefix = prefix
   self.__sparkle_uri = uri
   return self
end

function namespace:__index(k)
   return __loadObject(self, k)
end

------------------------------
-- END
------------------------------
return sparkleObj
