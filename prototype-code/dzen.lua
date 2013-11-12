
local dzen = {}

local dzenCommandLine = "dzen2 -bg white -y 400 -x 400 -w 800 -xs 1 -e '' -l 22 -fn '-*-inconsolata-*-*-*-*-*-*-*-*-*-*-*-*' -m"

function dzen:start()
   self.proc = io.popen(dzenCommandLine, "w")
   self:text("^uncollapse()")
end
function dzen:title(text)
   --self.proc:write("^tw()^fg(#cb4b16)" .. text .. "\n")
   self.proc:write("^tw()" .. text .. "\n")
   self.proc:flush()
end
function dzen:text(text)
   self.proc:write(text)
   self.proc:write("\n")
   self.proc:flush()
end
function dzen:stop()
   self.proc:close()
end

-- function dzen:chooseOneOf(values)
-- end

return dzen
