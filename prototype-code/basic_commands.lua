local commands = {}

local keyStream = io.popen("./grabkey")

local currentCommand = nil


local dzen2CommandLine = "dzen2 -xs 1 -e '' -l 22 -fn '-*-inconsolata-*-*-*-*-*-*-*-*-*-*-*-*' -m"

local dzen2 = {}
function dzen2:start()
   -- TODO this must be write-enabled
   self.proc = io.popen(dzen2CommandLine, "w")
end
function dzen2:title(text)
   self.proc:write("^tw()^fg(#cb4b16)" .. text .. "\n")
   self.proc:flush()
end
function dzen2:stop()
   self.proc:close()
end

local commandXyz = {}
commandXyz.interactive = true
commandXyz.commandBuf = ""
function commandXyz:run()
   currentCommand = self
   dzen2:start()
   --self.
end
function commandXyz:input(key)
   if key == "C-g" then
   elseif key == "Return" then
	  print("Running '" .. self.commandBuf .. "'")
	  self.commandBuf = ""
	  currentCommand = nil
	  dzen2:stop()
   else
	  self.commandBuf = self.commandBuf .. key
	  dzen2:title(self.commandBuf)
   end
end

local commandTest = {}
commandTest.interactive = true
function commandTest:run()
   print("Running test")
end

commands["C-o"] = commandXyz
commands["C-n"] = commandTest

-- for x, y in pairs(_ENV) do
--    print(x, " .. ", y)
-- end
-- if true then
--    return 1
-- end
for x in keyStream:lines() do
   if currentCommand then
	  currentCommand:input(x)
   elseif commands[x] then
	  commands[x]:run()
   end
end
