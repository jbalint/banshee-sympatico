-- X11 module
require("pl.stringx").import()

if not X11 then X11 = {} end

function X11:getFocusWindow()
   return self.focusWindow
end

function X11:setFocusWindow(windowId)
   self.focusWindow = windowId
end

function X11:getAllWindowIds()
   local stream = io.popen("xprop -root")
   local clientLine
   for line in stream:lines() do
	  if line:match("_NET_CLIENT_LIST") then
		 clientLine = line
		 break
	  end
   end
   stream:close()
   local windowIds = {}
   return clientLine:gsub(".*# ", ""):split(", ")
end

function X11:activate(windowId)
   os.execute("xdotool windowactivate " .. windowId)
end

function X11:props(windowId)
   -- TODO fix escaping: label = "bs:0:emacs - \\\"rook\\\"",
   local stream = io.popen("xprop -notype -id " .. windowId .. " 8s '$0' WM_NAME")
   local line = stream:lines()()
   stream:close()
   local winLabel = line:gsub("WM_NAME", ""):gsub('^"(.*)"$', "%1")
   stream = io.popen("xprop -notype -id " .. windowId .. " 8s '$0' WM_CLASS")
   line = stream:lines()()
   stream:close()
   local winClass = line:gsub("WM_CLASS", ""):gsub('^"(.*)"$', "%1")
   stream = io.popen("xprop -notype -id " .. windowId .. " 8s '$1' WM_CLASS")
   line = stream:lines()()
   stream:close()
   local winName = line:gsub("WM_CLASS", ""):gsub('^"(.*)"$', "%1")
   local result = {["label"] = winLabel,
				   ["class"] = winClass,
				   ["name"] = winName}
   return result
end

if false and os.getenv("EMACS") == "t" then
   require("pl.pretty").dump(X11:getAllWindowIds())
   require("pl.pretty").dump(X11:props(X11:getAllWindowIds()[1]))
end

return X11
