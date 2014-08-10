#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "bs_lua.h"
#include "bs_xsb.h"

void do_flora(const char *q)
{
  char *x;

  printf("\n\n\n***********************\n %s\n", q);
  bs_xsb_query_begin(q, &x, " ");
  do {
	printf("> %s\n", x);
	bs_xsb_next(&x, " ");
  } while (x);
  bs_xsb_query_end();
}

int main(int argc, char **argv)
{
  int rc;
  // TODO parameterize path - XSB_HOME
  if (bs_xsb_init("/home/jbalint/sw/xsb-src/XSB"))
  {
	exit(1);
  }
  // TODO parameterize path - FLORA_HOME
  rc = bs_xsb_command("asserta(library_directory('/home/jbalint/sw/flora-git/flora2')).");
  assert(!rc);
  rc = bs_xsb_command("[flora2].");
  assert(!rc);
  rc = bs_xsb_command("bootstrap_flora.");
  assert(!rc);
  // TODO parameterize path - BS_HOME
  rc = bs_xsb_command("'\\\\add'('/home/jbalint/sw/banshee-sympatico/kernel/bs_prefixes').");
  assert(!rc);
  rc = bs_xsb_command("'\\\\add'('/home/jbalint/sw/banshee-sympatico/kernel/bs_rdfs').");
  assert(!rc);

  // TODO parameterize path
  // load native component of CRATE storage
  rc = bs_xsb_command("asserta(library_directory('/home/jbalint/sw/banshee-sympatico/kernel')).");
  assert(!rc);
  rc = bs_xsb_command("[cratehooks].");
  assert(!rc);
  rc = bs_xsb_command("['crate.c'].");
  // TODO - even if this compilation fails, the program continues (rc=0?)
  assert(!rc);

  bs_lua_init();

  lua_repl_main(argc, argv);

  return 0;
}
