local tablex = require("pl.tablex")

local dzen = require("dzen")

local xdotool = require("xdotool")

local wmii = require("wmii")

local x11 = require("x11")

-------------------------------------------------
-- event handling
--
-- events are read from the event stream and
-- routed based on the prefix of the event line.
-- multi-line events are unsupported
-------------------------------------------------
local eventHandlers = {}
function eventRegisterHandler(handler)
   eventHandlers[handler.prefix] = handler
end

eventRegisterHandler(require("wmii_events"))

-------------------------------------------------
-------------------------------------------------
local commands = {}

local test = {}
test.interactive = true
test.arguments = {}
setmetatable(test, test)
function test:__call()
   print("Called test:__call")
end

local test2 = {}
test2.interactive = true
test2.arguments = {}
setmetatable(test2, test2)
function test2:__call()
   print("Called test2:__call")
end

-- refresh the list of commands by looking through the
-- module for objects with the `interactive' property
function refreshCommands()
   commands = {}
   local i = 1
   while true do
	  local n, v = debug.getlocal(2, i)
	  if not n then break end
	  if type(v) == "table" and v.interactive then
		 commands[n] = v
	  end
	  i = i + 1
   end
end
refreshCommands()

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

local eventStream = io.popen("./events.sh")

local keyBuf = ""

local keyBindings = {}
keyBindings["Mod4-i"] = function()
   -- http://unix.stackexchange.com/questions/23164/manipulating-x-key-and-pointer-grabs-on-the-command-line
   os.execute("xdotool key XF86LogGrabInfo")
   local grabKey = io.popen("./grabkey")
   dzen:start()
   local command = ""
   for k in grabKey:lines() do
	  if k == "Return" then
		 commands[command]()
		 grabKey:close()
		 break
	  elseif lineEditCommands[k] then
		 command = lineEditCommands[k](command)
	  elseif #k == 1 then
		 command = command .. k
	  end
	  local titleText1 = command:gsub("(^)", "%1%1")
	  dzen:text("^cs()")
	  for cmd in tablex.sort(commands) do
		 if command ~= "" and cmd:match(command) then
			local dispText = cmd:gsub("(^)", "^^")
			dispText = dispText:gsub("(" .. titleText1 .. ")", "^fg(#859900)%1^fg()")
			dzen:text(dispText)
		 end
	  end

	  local titleText2
	  if titleText1:match("(^^)$") then
		 titleText2 = titleText1:gsub("(^^)$", "^fg(#cb4b16)%1")
	  else
		 titleText2 = titleText1:gsub("(.)$", "^fg(#cb4b16)%1")
	  end
	  dzen:title(titleText2)
   end
   dzen:stop()
end

for ev in eventStream:lines() do
   if ev:match("wmii: Key ") then
	  local key = ev:gsub("wmii: Key ", "")
	  if keyBuf == "" then
		 keyBuf = key
	  else
		 keyBuf = keyBuf .. " " .. key
	  end
	  if #keyBuf > 20 then
		 print("Long key-sequence. cancelling " .. keyBuf)
		 keyBuf = ""
	  end
	  if keyBindings[keyBuf] then
		 keyBindings[keyBuf]()
		 keyBuf = ""
	  end
   else
	  -- TODO first match to ':' has to be non-greedy
	  local a = ev:gmatch("([-%w]*):%s+(.*)$")
	  local eventType, eventData = a()
	  if eventHandlers[eventType] then
		 eventHandlers[eventType]:event(eventData)
	  else
		 print(string.format("Unhandled event. type=(%s), data = (%s)",
							 eventType, eventData))
	  end
   end
end

eventStream:close()
