local posix = require("posix")
require("io")
require("fdopen")

local grabkey = {}

function grabkey:start()
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
	  self.keyStream = fdopen(r, "r")
	  self.childPid = pid
   end
end

function grabkey:lines()
   return self.keyStream:lines()
end

function grabkey:kill()
   posix.kill(self.childPid)
end

local function test()
   grabkey:start()
   for x in grabkey:lines() do
	  if x == "g" then
		 grabkey:kill()
	  end
   end
   print("Done")
end

return grabkey
