#include <stdio.h>
#include <assert.h>
#include "cinterf.h" /* XSB C interface */

/*
 * Result string used in queries. This is shared across all
 * calls. Callers should not expect the string values returned to be
 * valid after the next call to a string-returning function.
 */
static XSB_StrDefine(res_str);

int bs_xsb_init(const char *xsb_path)
{
  if (xsb_init_string((char *) xsb_path) == XSB_ERROR)
  {
	/* TODO retrieve better error message from XSB */
	fprintf(stderr, "XSB Error");
	return 1;
  }
  return 0;
}

int bs_xsb_command(const char *command)
{
  if (xsb_command_string((char *) command) == XSB_ERROR)
  {
	/* TODO retrieve better error message from XSB */
	fprintf(stderr, "Failed command: %s: %s\n", command, xsb_get_error_message());
	return 1;
  }
  return 0;
}

int bs_xsb_query_begin(const char *query_string, char **str, const char *sep)
{
  int rc;
  assert(str);
  assert(sep);

  rc = xsb_query_string_string((char *) query_string, &res_str, (char *) sep);

  if (rc == XSB_FAILURE)
  {
	/* failure is a query with no results */
	*str = NULL;
	return 0;
  }
  else if (rc == XSB_ERROR)
  {
	/* TODO */
	fprintf(stderr, "Query error");
	return 1;
  }
  *str = res_str.string;
  return 0;
}

int bs_xsb_next(char **str, const char *sep)
{
  int rc;
  assert(str);
  assert(sep);
  rc = xsb_next_string(&res_str, (char *) sep);
  if (rc == XSB_SUCCESS)
  {
	*str = res_str.string;
	return 0;
  }
  else if (rc == XSB_ERROR)
  {
	/* TODO */
	fprintf(stderr, "xsb_next_string error: %s\n", xsb_get_error_message());
	*str = NULL;
	return 1;
  }
  else
  {
	/* no more items */
	*str = NULL;
	return 0;
  }
}

int bs_xsb_query_end()
{
  xsb_close_query();
  return 0;
}
