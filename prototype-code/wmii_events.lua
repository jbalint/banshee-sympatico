-------------------------------------------------
-- wmii handler
-------------------------------------------------
local wmiiHandler = {}
wmiiHandler.prefix = "wmii"
function wmiiHandler:event(ev)
end

eventRegisterHandler(wmiiHandler)

return wmiiHandler
