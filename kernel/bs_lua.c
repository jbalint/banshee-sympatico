#include <stdio.h>
#include <assert.h>

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

#include "bs_xsb.h"

/*
 * A single Lua state is used.
 */
/* allow to be reference externally in lua_repl.c */
/*static*/ lua_State *global_L;

static int bs_lua_xsb_query(lua_State *L)
{
  char *res;
  const char *query_string = luaL_checkstring(L, 1);
  const char *sep = luaL_checkstring(L, 2);
  int answer_num = 1;

  if (bs_xsb_query_begin(query_string, &res, sep))
  {
	fprintf(stderr, "Error during query");
	/* TODO lua error */
	lua_pushnil(L);
	return 1;
  }

  lua_newtable(L);

  while (res)
  {
	lua_pushstring(L, res);
	lua_rawseti(L, -2, answer_num++);

	/* TODO push any error to Lua - how to force an error here for testing? */
	bs_xsb_next(&res, sep);
  }

  /* TODO error handling */
  bs_xsb_query_end();

  return 1;
}

static int bs_lua_xsb_command(lua_State *L)
{
  const char *command = luaL_checkstring(L, 1);

  if (bs_xsb_command(command))
  {
	/* TODO lua error */
	fprintf(stderr, "Error during command");
	return 0;
  }

  return 0;
}

/* int lua_interface_error(lua_State *L, const char *format, ...) */
/* { */
/*   va_list ap; */
/*   char msg[500]; */
/*   va_start(ap, format); */
/*   vsprintf(msg, format, ap); */
/*   fflush(stdout); */
/*   va_end(ap); */
/*   lua_pushstring(L, msg); */
/*   traceback(L); */
/*   fprintf(stderr, "%s\n", msg); */
/*   return lua_error(L); */
/* } */

int bs_lua_init()
{
  int rc;
  global_L = luaL_newstate();
  assert(global_L);
  luaL_openlibs(global_L);
  /*
  lua_newtable(global_L);
  lua_pushcfunction(global_L, bs_lua_xsb_query);
  lua_setfield(global_L, -2, "xsb_query");
  */
  lua_register(global_L, "xsb_query", bs_lua_xsb_query);
  lua_register(global_L, "xsb_command", bs_lua_xsb_command);
  rc = luaL_dostring(global_L, "require('bs_init')");
  if (rc)
  {
	/* TODO better general error reporting during initialization */
	const char *msg = luaL_checkstring(global_L, 1);
	fprintf(stderr, "LUA ERROR: %d\n", rc);
	fprintf(stderr, "%s\n", msg);
  }
  assert(!rc);
  return 0;
}
