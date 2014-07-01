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
  // TODO parameterize path
  if (bs_xsb_init("/home/jbalint/sw/xsb-src/XSB"))
  {
	exit(1);
  }
  // TODO parameterize path
  rc = bs_xsb_command("asserta(library_directory('/home/jbalint/sw/flora-git/flora2')).");
  assert(!rc);
  rc = bs_xsb_command("[flora2].");
  assert(!rc);
  rc = bs_xsb_command("bootstrap_flora.");
  assert(!rc);
  // TODO parameterize path
  //rc = bs_xsb_command("'\\load'('/home/jbalint/sw/banshee-sympatico/kernel/base.flr').");
  //assert(!rc);
  rc = bs_xsb_command("'\\load'('/home/jbalint/sw/banshee-sympatico/kernel/turtle/export5_test').");
  assert(!rc);

  /* do_flora("flora_query(\"?X = \\\"abc\\\"^^\\string, ?Y = 1.\",[\"?X\"=X,\"?Y\"=Y],Res,_,_)."); */
  /* do_flora("flora_query(\"?X:?Y@\\@.\",[\"?X\"=X,\"?Y\"=Y],_,_,_)."); */
  /* do_flora("flora_query(\"prefix{?X,?Y}.\",[\"?X\"=X,\"?Y\"=Y],_,_,_)."); */
  /* do_flora("flora_query(\"jess#Me[?X->?Y].\",[\"?X\"=X,\"?Y\"=Y],_,_,_)."); */

  bs_lua_init();

  return 0;
}
