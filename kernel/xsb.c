#include <stdio.h>

#include "cinterf.h" /* XSB C interface */

void do_xsb()
{
  // -e startup.
  if (xsb_init_string("/home/jbalint/sw/xsb-src/XSB") == XSB_ERROR)
  {
	fprintf(stderr, "XSB Error");
	exit(1);
  }

  if (xsb_command_string("asserta(library_directory('/home/jbalint/sw/flora-git/flora2')).") == XSB_ERROR)
  {
	fprintf(stderr, "Failed to set flora dir");
	exit(1);
  }

  if (xsb_command_string("[flora2].") == XSB_ERROR)
  {
	fprintf(stderr, "Failed to load flora");
	exit(1);
  }

  if (xsb_command_string("flora_shell.") == XSB_ERROR)
  {
	fprintf(stderr, "Failed to run flora");
	exit(1);
  }
}
