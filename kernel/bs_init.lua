local flora = require('bs_flora')

-- TODO code something here
-- x = (...)
-- print("X is ")
-- require('pl.pretty').dump(x)
-- core initialization
print("CORE")

local _dump = require('pl.pretty').dump

local boxinglove = require('bs_boxinglove')

boxinglove.init()

local bsbase = boxinglove.ns.bsbase
local bsoracle = boxinglove.ns.bsoracle
local pimo = boxinglove.ns.pimo
_dump(bsoracle)
print(bsbase.jess)

_dump(bsbase.jess)
_dump(bsoracle.MysqlBug73053LinuxSocketBug)
_dump(bsoracle.mysqlBugsDbNumber)

print("---------------------------")
--_dump(getmetatable(bsoracle.mysqlBugsDbNumber))
print("touri is : " .. tostring(bsoracle.mysqlBugsDbNumber:_touri()))
print("The bug is: " .. tostring(bsoracle.MysqlBug73053LinuxSocketBug))
print("The prop is: " .. tostring(bsoracle.mysqlBugsDbNumber))
print("The bug num is: " .. tostring(bsoracle.MysqlBug73053LinuxSocketBug[bsoracle.mysqlBugsDbNumber]))
print("Tag 1 is: ") _dump(bsoracle.MysqlBug73053LinuxSocketBug[pimo.hasTag][1])
print("Tag 1 is: ")
_dump(bsoracle.MysqlBug73053LinuxSocketBug[pimo.hasTag][1]())
--print("Tag 1 is: ") _dump(pimo.hasTag)
