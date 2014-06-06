local sparkleObj = {
   sparqlEndpointUrl = "<unknown>",
   ns = {}
}

local namespace = {
   classname = "sparkleObj.namespace"
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
-- TODO document the forms of calling loadObject()
local function loadObject(a, b)
   local objName
   if type(a) == "table" and getmetatable(a).classname == "sparkleObj.namespace" then
	  objName = string.format("%s:%s", sparkleObj.namespacePrefix, b)
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
   local s = turtleparse.parse(sparqlclient.query(sparkleObj.sparqlEndpointUrl, q))
   -- TODO parse `s'

   -- transform to instance
end

local function loadNamespaces()
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
	  dump(ns)
   end
end

------------------------
-- Sparkle Object API --
------------------------
function sparkleObj.init(props)
   sparkleObj.sparqlEndpointUrl = props.sparqlEndpointUrl
   loadNamespaces()
   -- TODO namespace checking/validation
end

function sparkleObj.namespacePrefix(ns)
   assert(getmetatable(ns).classname == "sparkleObj.namespace")
   return rawget(ns, "__sparkle_prefix")
end

function sparkleObj.namespaceUri(ns)
   assert(getmetatable(ns).classname == "sparkleObj.namespace")
   return rawget(ns, "__sparkle_uri")
end

------------------------------
-- value set implementation --
------------------------------
function valueSet.create()
end

------------------------------
-- instance implementation --
------------------------------
function objInstance.create()
end

------------------------------
-- reference implementation --
------------------------------
function objReference.create(ns, name)
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
   return loadObject(self, k)
end

------------------------------
-- END
------------------------------
return sparkleObj
