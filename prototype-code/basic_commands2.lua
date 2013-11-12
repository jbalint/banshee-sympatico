-------------------------------------------------
-- dzen2
-------------------------------------------------
local dzen2CommandLine = "dzen2 -xs 1 -e '' -l 22 -fn '-*-inconsolata-*-*-*-*-*-*-*-*-*-*-*-*' -m"

local dzen2 = {}
function dzen2:start()
   self.proc = io.popen(dzen2CommandLine, "w")
end
function dzen2:title(text)
   --self.proc:write("^tw()^fg(#cb4b16)" .. text .. "\n")
   self.proc:write("^tw()" .. text .. "\n")
   self.proc:flush()
end
function dzen2:stop()
   self.proc:close()
end

-------------------------------------------------
-- wmii handler
-------------------------------------------------
local wmiiHandler = {}
wmiiHandler.prefix = "wmii"
function wmiiHandler:event(ev)
end

-------------------------------------------------
-------------------------------------------------
local commands = {}

local test = {}
test.interactive = true
test.arguments = {}

local test2 = {}
test2.interactive = true
test2.arguments = {}

-- refresh the list of commands by looking through the
-- module for objects with the `interactive' property
function refreshCommands()
   commands = {}
   local i = 1
   while true do
	  local n, v = debug.getlocal(2, i)
	  if not n then break end
	  if v.interactive then
		 commands[n] = v
	  end
	  i = i + 1
   end
end
refreshCommands()

local eventStream = io.popen("./events.sh")

function lineEditReplace(replaceString)
   return function(x)
	  return x .. replaceString
   end
end

local lineEditCommands = {}
lineEditCommands["BackSpace"] = function (text)
   return text:gsub(".$", "")
end
lineEditCommands["grave"] = lineEditReplace("`")
lineEditCommands["asciitilde"] = lineEditReplace("~")
lineEditCommands["exclam"] = lineEditReplace("!")
lineEditCommands["at"] = lineEditReplace("@")
lineEditCommands["numbersign"] = lineEditReplace("#")
lineEditCommands["dollar"] = lineEditReplace("$")
lineEditCommands["percent"] = lineEditReplace("%")
lineEditCommands["asciicircum"] = lineEditReplace("^")
lineEditCommands["ampersand"] = lineEditReplace("&")
lineEditCommands["asterisk"] = lineEditReplace("*")
lineEditCommands["parenleft"] = lineEditReplace("(")
lineEditCommands["parenright"] = lineEditReplace(")")
lineEditCommands["minus"] = lineEditReplace("-")
lineEditCommands["underscore"] = lineEditReplace("_")
lineEditCommands["equal"] = lineEditReplace("=")
lineEditCommands["plus"] = lineEditReplace("+")
lineEditCommands["backslash"] = lineEditReplace("\\")
lineEditCommands["bar"] = lineEditReplace("|")
lineEditCommands["slash"] = lineEditReplace("/")
lineEditCommands["period"] = lineEditReplace(".")
lineEditCommands["greater"] = lineEditReplace(">")
lineEditCommands["less"] = lineEditReplace("<")
lineEditCommands["comma"] = lineEditReplace(",")
lineEditCommands["semicolon"] = lineEditReplace(";")
lineEditCommands["colon"] = lineEditReplace(":")
lineEditCommands["bracketleft"] = lineEditReplace("[")
lineEditCommands["bracketright"] = lineEditReplace("]")
lineEditCommands["braceleft"] = lineEditReplace("{")
lineEditCommands["braceright"] = lineEditReplace("}")
lineEditCommands["space"] = lineEditReplace(" ")

for ev in eventStream:lines() do
   if ev == "wmii: Key Mod4-i" then
	  print("Grabbing key")
	  -- http://unix.stackexchange.com/questions/23164/manipulating-x-key-and-pointer-grabs-on-the-command-line
	  os.execute("xdotool key XF86LogGrabInfo")
	  local grabKey = io.popen("./grabkey")
	  dzen2:start()
	  local command = ""
	  for k in grabKey:lines() do
		 if k == "C-g" then
			grabKey:close()
			break
		 elseif lineEditCommands[k] then
			command = lineEditCommands[k](command)
		 elseif #k == 1 then
			command = command .. k
		 end
		 local titleText = command:gsub("(^)", "%1%1")
		 if titleText:match("(^^)$") then
			titleText = titleText:gsub("(^^)$", "^fg(#cb4b16)%1")
		 else
			titleText = titleText:gsub("(.)$", "^fg(#cb4b16)%1")
		 end
		 dzen2:title(titleText)
	  end
	  dzen2:stop()
   else
	  local a = ev:gmatch("([-%w]*):%s+([-%w%s]*)$")
	  print(a())
   end
end
