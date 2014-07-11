-- first version of slender - implemented outside of monkeyhead
local slender = {}

--
-- loadfile("/home/jbalint/sw/banshee-sympatico/kernel/slender.lua")

local boxinglove = require("bs_boxinglove")
local flora = require("bs_flora")
local Date = require("pl.Date")

local _dump = require("pl.pretty").dump

-- parse a uri to a qname pair (ns:boxinglove.namespace, name:string)
function _uri_to_qname(input)
   for prefix, ns in pairs(boxinglove.ns) do
	  if prefix:sub(1, 1) ~= "_" then
		 local uri = ns._boxinglove_uri
		 local len = #uri
		 if input:sub(1, len) == uri then
			local name = input:sub(#ns._boxinglove_uri + 1)
			return ns, name
		 end
	  end
   end
   return nil, nil
end

function _lisp_quote_string(str)
   return string.format('"%s"', str:gsub("\\", "\\\\"):gsub('"', '\\"'))
end

function _value_to_lisp(v)
   if type(v) == "string" then
	  return _lisp_quote_string(v)
   elseif type(v) == "table" then
	  local class = (getmetatable(v) or {}).classname
	  if class == "boxinglove.reference" or
	  class == "boxinglove.instance" then
		 return string.format(":%s-%s", v._boxinglove_ns._boxinglove_prefix, v._boxinglove_name)
	  elseif class == "boxinglove.valueset" then
		 local valstr = "("
		 --print("VALUE SET")
		 for _, entry in pairs(v) do
			valstr = string.format("%s\n     %s", valstr, _value_to_lisp(entry))
		 end
		 return valstr .. ")"
	  end

	  if getmetatable(v) == Date then
		 local s
		 if v.xsdType == "date" then
			s = Date.Format("yyyy-mm-dd"):tostring(v)
		 elseif v.xsdType == "dateTime" then
			s = Date.Format("yyyy-mm-ddTHH:MM:SS"):tostring(v)
		 end
		 assert(s)
		 return string.format('(:%s "%s")', v.xsdType, s)
	  end

	  print("TABLE")
	  require("pl.pretty").dump(v)
	  print("META TABLE")
	  require("pl.pretty").dump(getmetatable(v))
	  error("unknown table type")
   else
	  require("pl.pretty").dump(v)
	  error("Unknown value type: " .. type(v))
   end
end

-- bl object to lisp
function blo_to_lisp(o)
   local lisp = ""
   local pvs = {} -- prop val pairs
   for prop, val in pairs(o) do
	  if prop:sub(1, 1) ~= "_" then
		 local pns, pname = _uri_to_qname(prop)
		 local propstr = string.format("%s-%s", pns._boxinglove_prefix, pname)
		 --print(string.format("%s = %s ===========> ", propstr, val))
		 local valstr = _value_to_lisp(val)
		 table.insert(pvs, string.format("  (:%s %s)", propstr, valstr))
	  end
   end
   --_dump(pvs)
   lisp = "("
   for _, pv in ipairs(pvs) do
	  lisp = string.format("%s\n  %s", lisp, pv)
   end
   return lisp .. ")"
end

-- get the list of task containers
function get_containers()
   -- TODO flora.instances() *WILL* go away once the APIs are more
   -- complete in boxinglove
   local containers = flora.instances("tmo#TaskContainer")

   -- TODO need a more efficient approach to this
   -- and move it into boxinglove module
   for idx, strobj in ipairs(containers) do
	  local ns, name = _uri_to_qname(strobj)
	  assert(ns, "Unable to load object: " .. tostring(strobj))
	  -- load the objects
	  containers[idx] = ns[name]
	  assert(containers[idx])
   end

   return containers
end

-- get the list of container objects as a sexp
function get_containers_lisp()
   local lisp = "("
   for _, container in ipairs(get_containers()) do
	  lisp = string.format("%s\n(:%s-%s %s)",
						   lisp,
						   container._boxinglove_ns._boxinglove_prefix,
						   container._boxinglove_name,
						   blo_to_lisp(container))
   end
   return lisp .. ")"
end

--print(get_containers_lisp())

function _is_instance(o, class)
   local classes = flora.classes(string.format("%s#%s",
											   o._boxinglove_ns._boxinglove_prefix,
											   o._boxinglove_name))
   return classes[class:_touri()] ~= nil
end

function _print_task(task)
   print(string.format("** %s#%s",
					   task._boxinglove_ns._boxinglove_prefix,
					   task._boxinglov_name))
   print(string.format("Completion: %d", task[tmo.actualCompletion] / task[tmo.targetCompletion]))
   if _is_instance(task, bsoracle.RbReviewTask) then
	  print(string.format("RB#%d", task[bsoracle.rbEntry]()[bsoracle.rbNumber]))
   end
end

function slender.shell()
   local containers = get_containers()
   local active_container
   local active_task

   local last_numbered_result

   while true do
	  io.stdout:write(">- ")
	  local l = io.stdin:read("*l")
	  if not l then
		 return
	  end
	  if l == "l" then
		 if active_task then
			_print_task(active_task)
			last_numbered_result = nil
		 elseif active_container then
			print("** " .. tostring(active_container))
			for idx, t in ipairs(active_container[tmo.containsTask] or {}) do
			   print(string.format("[%d] %s", idx, t))
			end
			last_numbered_result = active_container[tmo.containsTask]
		 else
			l = "lc"
		 end
	  end

	  if l == "lc" then
		 for idx, c in ipairs(containers) do
			local taskcount = 0
			if c[tmo.containsTask] then
			   taskcount = #c[tmo.containsTask]
			end
			local status = " "
			if active_container == c then
			   status = "<"
			end
			local summary = string.format("[%s]%s %s (%d)",
										  idx,
										  status,
										  c[rdfs.label],
										  taskcount)
			print(summary)
		 end
		 last_numbered_result = containers
	  elseif tonumber(l) and last_numbered_result then
		 local o = last_numbered_result[tonumber(l)]
		 print(o)
		 if _is_instance(o, tmo.TaskContainer) then
			active_container = o
		 elseif _is_instance(o, tmo.Task) then
			active_task = o()
		 else
			print("whatever.")
		 end
		 last_numbered_result = nil
	  else
		 -- print(tonumber(l))
		 -- print(last_numbered_result)
		 -- print("confused.")
		 -- last_numbered_result = nil
	  end
   end
end

--shell()

return slender
