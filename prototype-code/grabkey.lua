local posix = require("posix")
require("io")
require("fdopen")

if not GrabKey then GrabKey = {} end
GrabKey.mt = { __index = GrabKey, classname = "GrabKey" }

function GrabKey.new()
   local obj = {}
   setmetatable(obj, GrabKey.mt)
   local r, w = posix.pipe()
   local pid = posix.fork()
   if pid == 0 then -- child
	  posix.close(r)
	  posix.dup2(w, posix.STDOUT_FILENO)

	  -- http://unix.stackexchange.com/questions/23164/manipulating-x-key-and-pointer-grabs-on-the-command-line
	  os.execute("xdotool key XF86LogGrabInfo")

	  posix.exec("./grabkey")
   else
	  posix.close(w)
	  obj.keyStream = fdopen(r, "r")
	  obj.childPid = pid
   end
   return obj
end

function GrabKey:lines()
   return self.keyStream:lines()
end

function GrabKey:kill()
   posix.kill(self.childPid)
end

local function test()
   local gk = GrabKey.new()
   require('pl.pretty').dump(gk)
   for x in gk:lines() do
	  if x == "g" then
		 gk:kill()
	  end
   end
   print("Done")
end

return GrabKey
