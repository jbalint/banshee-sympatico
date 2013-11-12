#include <stdio.h>
#include <assert.h>

#include <lua.h>
#include <lauxlib.h>

/* from lua source, needed for reference to file handle metatable */
typedef luaL_Stream LStream;

static
int lua_fdclose(lua_State *L)
{
  LStream *lstream = (LStream *) luaL_checkudata(L, 1, LUA_FILEHANDLE);
  lua_pop(L, 1);
  fclose(lstream->f);
  return 0;
}

/*
 * Open a file descriptor to a Lua filehandle
 */
static
int lua_fdopen(lua_State *L)
{
  int fd = luaL_checkint(L, 1);
  const char *mode = luaL_checkstring(L, 2);
  LStream *lstream;
  lua_pop(L, 2);
  lstream = (LStream *) lua_newuserdata(L, sizeof(LStream));
  assert(lstream);
  lstream->f = fdopen(fd, mode);
  assert(lstream->f);
  luaL_setmetatable(L, LUA_FILEHANDLE);
  lstream->closef = lua_fdclose;
  return 1;
}

LUALIB_API
int luaopen_fdopen(lua_State *L)
{
  lua_register(L, "lua_fdopen", lua_fdopen);
  return 0;
}
