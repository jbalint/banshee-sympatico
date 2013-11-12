-------------------------------------------------
-- event handling
--
-- events are read from the event stream and
-- routed based on the prefix of the event line.
-- multi-line events are unsupported
-------------------------------------------------
local eventHandlers = {}

-- GLOBAL
function eventRegisterHandler(handler)
   eventHandlers[handler.prefix] = handler
end

function eventHandle(eventType, eventData)
   if eventHandlers[eventType] then
	  eventHandlers[eventType]:event(eventData)
   else
	  print(string.format("Unhandled event. type=(%s), data = (%s)",
						  eventType, eventData))
   end
end

return nil
