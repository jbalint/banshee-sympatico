#!/bin/env lua

-- Proof of concept to broadcast wmii events to DBus

local l2dbus = require('l2dbus')
local l2dbus_service = require('l2dbus.service')

local dump = require("pl.pretty").dump

-- This enables the passed message types
l2dbus.Trace.setFlags(l2dbus.Trace.ERROR, l2dbus.Trace.WARN)
--l2dbus.Trace.setFlags(l2dbus.Trace.ALL)

-- Our DBus interface
local interface_name = "com.bs.wmii_event_broadcast"
local wmii_event_interface = {
   signals = {
	  {name = "Key",
	   args = {{sig = "s", name = "key"}}
	  },
	  {name = "ClientFocus",
	   args = {{sig = "s", name = "client_id"}}
	  },
	  {name = "AreaFocus",
	   args = {{sig = "s", name = "area"}}
	  },
	  {name = "ColumnFocus",
	   args = {{sig = "s", name = "column"}}
	  },
	  {name = "FocusTag",
	   args = {{sig = "s", name = "tag"}}
	  },
	  {name = "UnfocusTag",
	   args = {{sig = "s", name = "tag"}}
	  },
	  {name = "FocusFloating"}
   }
}

-- Using the service module, we create a new service and object.
-- Since we are only emitting signals, we don't need any event
-- handler(s)
local svc = l2dbus_service.new("/com/bs/wmii_event_broadcast", false, nil)
svc:addInterface(interface_name, wmii_event_interface)

-- create the event loop, dispatcher, connection
local mainLoop = require("l2dbus_glib").MainLoop.new()
local dispatcher = l2dbus.Dispatcher.new(mainLoop)
assert( nil ~= dispatcher )
local conn = l2dbus.Connection.openStandard(dispatcher, l2dbus.Dbus.BUS_SESSION)
assert( nil ~= conn )

-- attach the service to the connection
svc:attach(conn)

-- callback to handle wmii events by broadcasting them as DBus signals
function on_wmii_event(watch, evTable, fd)
   if evTable.READ then
	  local event, arg
	  local event_text = fd:read()
	  for word in event_text:gmatch("[%w-]+") do
		 if event then
			arg = word
		 elseif arg then
			error("Bad event text: " .. event_text)
		 else
			event = word
		 end
	  end
	  svc:emit(conn, interface_name, event, arg)
   end
end

-- open the stream of wmii events
local fd = io.popen("wmiir read /event", "r")

-- add the stream to the fd set being watched
local watch = l2dbus.Watch.new(dispatcher, fd, l2dbus.Watch.READ, on_wmii_event, fd)
watch:setEnable(true)

-- start event loop
dispatcher:run(l2dbus.Dispatcher.DISPATCH_WAIT)
