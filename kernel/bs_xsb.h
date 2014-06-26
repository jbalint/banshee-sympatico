
int bs_xsb_init(const char *xsb_path);

int bs_xsb_command(const char *command);

int bs_xsb_query_begin(const char *query_string, char **str, const char *sep);

int bs_xsb_next(char **str, const char *sep);

int bs_xsb_query_end();
