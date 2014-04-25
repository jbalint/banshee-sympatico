-- Note: LuaJ code

-- Run:

local run_command = [[
SNARL_JARS=`find ~/sw/stardog-2.1.3/{server,client} -name '*.jar' | tr '\n' ':'`
LUAJ_JARS=~/sw/luaj-3.0-beta2/lib/luaj-jse-3.0-beta2.jar:
java -cp ${SNARL_JARS}${LUAJ_JARS}. lua lua_graph_orm_stardog.lua
]]

-- This is a prototype for creating a persistence of lua objects on
-- top of a triplestore/graphdb

-- This particular implementation uses stardog



-- imports
local java_imports = {
   "com.complexible.stardog.Stardog",
   "com.complexible.common.protocols.server.Server",
   "com.complexible.stardog.api.admin.AdminConnection",
   "com.complexible.stardog.api.admin.AdminConnectionConfiguration",
   "com.complexible.stardog.api.Connection",
   "com.complexible.stardog.api.ConnectionConfiguration",
   "com.complexible.stardog.protocols.snarl.SNARLProtocolConstants",

   "org.openrdf.model.impl.ValueFactoryBase",
   "org.openrdf.model.impl.ValueFactoryImpl",
   "org.openrdf.model.impl.StatementImpl",
}

-- process imports
function process_imports(imports)
   for idx, class_name in ipairs(imports) do
	  local unqual_name = class_name:gsub(".*%.", "")
	  _G[unqual_name] = luajava.bindClass(class_name)
   end
end
process_imports(java_imports)

print("Starting")

-- from ConnectionAPIExample
local aServer = Stardog:buildServer():bind(SNARLProtocolConstants.EMBEDDED_ADDRESS):start()

local aAdminConnection = AdminConnectionConfiguration:toEmbeddedServer():credentials("admin", "admin"):connect()
aAdminConnection:createMemory("myDb")
aAdminConnection:close()

local aConn = ConnectionConfiguration:to("myDb"):credentials("admin", "admin"):connect()
aConn:begin()

local v_fac = ValueFactoryImpl:getInstance()
local s = v_fac:createURI("http://jbalint/something")
local p = v_fac:createURI("http://jbalint/somethingElse")
local o = v_fac:createLiteral("the Object!")
local stmt = luajava.new(StatementImpl, s, p, o)
aConn:add():statement(stmt)
local o2 = v_fac:createLiteral("the (other) Object!")
local stmt2 = luajava.new(StatementImpl, s, p, o2)
aConn:add():statement(stmt2)

aConn:commit()

print("Done")
