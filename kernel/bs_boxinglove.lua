local boxinglove = {
   ns = {
	  _byuri = {}
   }
}

local flora = require('bs_flora')

local _dump = require('pl.pretty').dump

local Date = require('pl.Date')

-- Wrapper for custom index metamethod. We pass all requests for keys
-- that don't start with "_" to the custom index method. If it does
-- begin with underscore, it implies that it's an internal
-- key. Internal keys are first check as raw entries in the table, and
-- second as raw entries in the table's metatable.
local function _custom_index(t, k)
   if (type(k) == "string" and k:find("_") ~= 1)
   or type(k) ~= "string" then
	  return getmetatable(t)._index(t, k)
   end

   assert(type(t) == "table")

   -- instance value (if exists) takes precedence
   local inst = rawget(t, k)
   if inst then
	  return inst
   end

   -- fall back to class/metatable value
   local mt = rawget(getmetatable(t), k)
   if mt then
	  return mt
   end

   return nil
end

-----------------------
-- OBJECT REFERENCES --
-----------------------
local blreference = {
   classname = "boxinglove.reference",
   __index = _custom_index
}

function _blreference_new(ns, name)
   local self = {}
   -- TODO
   --_assertClass(ns, "boxinglove.namespace")
   self._boxinglove_ns = ns
   self._boxinglove_name = name
   setmetatable(self, blreference)
   return self
end

function blreference:__tostring()
   return string.format("%s:%s",
						self._boxinglove_ns._boxinglove_prefix,
						self._boxinglove_name)
end

function blreference:_touri()
   return self._boxinglove_ns:_qualify_name(self._boxinglove_name)
end

function blreference:_index(k)
   error("Object reference cannot be indexed")
end

function blreference:__call()
   return _blnamespace_loadobject(self._boxinglove_ns, self._boxinglove_name)
end

----------------------
-- OBJECT INSTANCES --
----------------------
local blinstance = {
   classname = "boxinglove.instance",
   __tostring = blreference.__tostring,
   __index = _custom_index
}

function _blparseuri(uri)
   -- tested with
   --uri = "http://banshee-sympatico/project#MysqlConnectorJ"
   --uri = "http://purl.org/dc/terms/title"
   assert(uri:find("http://") == 1)
   local hash = uri:find("#")
   if hash then
	  local base = uri:sub(1, hash)
	  local name = uri:sub(hash + 1)
	  return base, name
   end
   local _, _, base, name = uri:find("(.*/)(%w+)")
   return base, name
end

-- TODO this could probably be properly in the Flora module as long as
-- marshalling/unmarshalling is consistent
function _blparse(value)
   -- TODO audit / finish this
   -- flora type encoding
   local flenc_type = value:gmatch("%w\x08")()
   if flenc_type then
	  flenc_type = flenc_type:sub(1, 1)
	  if flenc_type == "i" then
		 value = value:sub(3)
		 if value:find("mailto:") == 1 then
			-- TODO find something to do with non object resources
			print("WARN: " .. "UNHANDLED EMAIL ADDRESS: " .. tostring(value))
		 else
			local base, name = _blparseuri(value)
			local ns = boxinglove.ns._byuri[base]
			return _blreference_new(ns, name)
		 end
	  elseif flenc_type == "s" then
		 return value:sub(3)
	  else
		 error("Invalid Flora encoding type: " .. tostring(flenc_type))
	  end
   end
   -- flora datatype
   if value:find("'\\datatype'") == 1 then
	  flenc_type = value:gsub("'\\datatype'%('\\(%w+)'.*", "%1")
	  if flenc_type == "long" then
		 local str = value:gsub("'\\datatype'%('\\long'%((%d+)%),'\\long'%)", "%1")
		 return tonumber(str)
	  elseif flenc_type == "date" then
		 local capture = value:gmatch("%d+")
		 capture() -- skip first one - date "sign"
		 local year = capture()
		 local month = capture()
		 local day = capture()
		 local d = Date{year = year, month = month, day = day}
		 d.xsdType = "date"
		 return d
	  elseif flenc_type == "dateTime" then
		 local capture = value:gmatch("%d+")
		 capture()
		 local year = capture()
		 local month = capture()
		 local day = capture()
		 local hour = capture()
		 local minute = capture()
		 local seconds = capture()
		 local d = Date{year = year,
						month = month,
						day = day,
						hour = hour,
						min = min,
						sec = sec}
		 d.xsdType = "dateTime"
		 return d
	  else
		 require("pl.pretty").dump(value)
		 error("Unknown Flora encoding for type: " .. tostring(flenc_type))
	  end
   end
   -- flora "raw" oid (only used as Bnodes)
   -- TODO eliminate this case
   print("UNKNOWN VALUE (" .. tostring(type(value)) .. "): " .. value)
   return value
end

function _blinstance_populate(self, data)
   for prop, val in pairs(data) do
	  --prop = _blparse(prop)
	  -- TODO not clean way to deal with prop IRI
	  prop = prop:sub(3)
	  if type(val) == "table" then
		 local vals = {}
		 setmetatable(vals, {classname = "boxinglove.valueset"})
		 rawset(self, prop, vals)
		 for idx, v in ipairs(val) do
			table.insert(vals, _blparse(v))
		 end
	  elseif type(val) == "string" then
		 rawset(self, prop, _blparse(val))
	  else
		 error("Invalid type for val: " .. type(val))
	  end
   end
end

function _blinstance_new(ns, name, data)
   local self = _blreference_new(ns, name)
   _blinstance_populate(self, data)
   setmetatable(self, blinstance)
   return self
end

function blinstance:_index(k)
   -- indexing by property reference/instance
   if type(k) == "table" and
	  (getmetatable(k).classname == "boxinglove.reference" or
	   getmetatable(k).classname == "boxinglove.instance") then
		 return self[k:_touri()]
   end
end

function blinstance.__newindex(k, v)
   -- TODO implement update/insert semantics
   error("newindex not supported")
end

function blinstance:_touri()
   return blreference._touri(self)
end

----------------
-- NAMESPACES --
----------------
local blnamespace = {
   classname = "boxinglove.namespace",
   __index = _custom_index
}

-- Internal function to create a new namespace
function _blnamespace_new(prefix, uri)
   local self = {}
   self._boxinglove_prefix = prefix
   self._boxinglove_uri = uri
   setmetatable(self, blnamespace)
   return self
end

function _blnamespace_loadobject(ns, name)
   -- TODO optimization: return instance if name begins with a capital
   -- letter, reference otherwise. this avoids populating property
   -- objects that will infrequently be used
   local fullname = string.format("%s#%s", ns._boxinglove_prefix, name)
   local data = flora.getproperties(fullname)
   if next(data) == nil then
	  -- TODO this *should* double-check because the object might be
	  -- "empty" but Flora object existence is wonky
	  error("Object doesn't exist: " .. fullname)
   end
   return _blinstance_new(ns, name, data)
end

function blnamespace:_qualify_name(name)
   return string.format("%s%s", self._boxinglove_uri, name)
end

function blnamespace:_index(k)
   return _blnamespace_loadobject(self, k)
end

---------
-- API --
---------
function boxinglove.init()
   -- Setup namespaces
   -- TODO convert flora to use bscode-configured prefixes
   local prefixes = flora.getprefixes()
   for prefix, uri in pairs(prefixes) do
	  if not uri then
		 uri = ""
	  end
	  local ns = _blnamespace_new(prefix, uri)
	  boxinglove.ns[prefix] = ns
	  boxinglove.ns._byuri[uri] = ns
	  -- TODO maybe this should happen external this module? (eases
	  -- testing, better design, etc)
	  -- we pollute the global environment with namespaces
	  _G[prefix] = ns
   end
end

function boxinglove.instances(class)
   -- TODO
end

return boxinglove
