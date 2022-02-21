#include <weechat/weechat-plugin.h>

WEECHAT_PLUGIN_NAME("bs-weechat-playground");
WEECHAT_PLUGIN_DESCRIPTION("Banshee-Sympatico Weechat Playground");
WEECHAT_PLUGIN_AUTHOR("Jess Balint <jbalint@gmail.com>");
WEECHAT_PLUGIN_VERSION("0.1");
WEECHAT_PLUGIN_LICENSE("MIT");

struct t_weechat_plugin *weechat_plugin = NULL;

// int
// command_double_cb (const void *pointer, void *data,
//                    struct t_gui_buffer *buffer,
//                    int argc, char **argv, char **argv_eol)
// {
//     /* make C compiler happy */
//     (void) pointer;
//     (void) data;
//     (void) buffer;
//     (void) argv;

//     if (argc > 1)
//     {
//         weechat_command (NULL, argv_eol[1]);
//         weechat_command (NULL, argv_eol[1]);
//     }

//     return WEECHAT_RC_OK;
// }

extern "C"
int
weechat_plugin_init (struct t_weechat_plugin *plugin,
                     int argc, char *argv[])
{
    weechat_plugin = plugin;

    (void) argc;
    (void) argv;

    // weechat_hook_command ("double",
    //                       "Display two times a message "
    //                       "or execute two times a command",
    //                       "message | command",
    //                       "message: message to display two times\n"
    //                       "command: command to execute two times",
    //                       NULL,
    //                       &command_double_cb, NULL, NULL);

    return WEECHAT_RC_OK;
}

extern "C"
int
weechat_plugin_end (struct t_weechat_plugin *plugin)
{
    (void) plugin;

    return WEECHAT_RC_OK;
}
