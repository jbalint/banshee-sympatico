/*

gcc -Wall -ggdb3 -o grabkey grabkey.c -lX11

*/

#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

char *modkey_names[] = {
	"Shift",
	"",
	"Control",
	"Mod1",
	"Mod2",
	"Mod3",
	"Mod4",
	"Mod5",
	NULL
};

int main(int argc, char **argv)
{
  Display *d;
  XEvent ev;
  unsigned int mods;
  KeySym ks;
  char buf[20];
  const char *kss;
  char *bufptr;
  int ret;
  d = XOpenDisplay(NULL);
  ret = XGrabKeyboard(d, DefaultRootWindow(d), False, GrabModeAsync, GrabModeAsync, CurrentTime);
  fprintf(stderr, "XGrabKeyboard returns %d\n", ret);
  while (1)
  {
	assert(!XWindowEvent(d, DefaultRootWindow(d), KeyPressMask, &ev));
	XkbLookupKeySym(d, ev.xkey.keycode, ev.xkey.state, &mods, &ks);
	/* ignore the modifier keypresses */
	switch (ks) /* from keysymdef.h */
	{
case XK_Shift_L   :  /* Left shift */
case XK_Shift_R   :  /* Right shift */
case XK_Control_L :  /* Left control */
case XK_Control_R :  /* Right control */
case XK_Caps_Lock :  /* Caps lock */
case XK_Shift_Lock:  /* Shift lock */
case XK_Meta_L    :  /* Left meta */
case XK_Meta_R    :  /* Right meta */
case XK_Alt_L     :  /* Left alt */
case XK_Alt_R     :  /* Right alt */
case XK_Super_L   :  /* Left super */
case XK_Super_R   :  /* Right super */
case XK_Hyper_L   :  /* Left hyper */
case XK_Hyper_R   :  /* Right hyper */
 continue;
	}
	kss = XKeysymToString(ks);
	
	bufptr = buf;
	if (ev.xkey.state & ShiftMask) { }
	if (ev.xkey.state & ControlMask)
	  bufptr += sprintf(bufptr, "C-");
	if (ev.xkey.state & Mod1Mask)
	  bufptr += sprintf(bufptr, "M-");
	if (ev.xkey.state & Mod2Mask) { }
	if (ev.xkey.state & Mod3Mask) { }
	if (ev.xkey.state & Mod4Mask)
	  bufptr += sprintf(bufptr, "S-"); // S = super
	bufptr += sprintf(bufptr, kss);
	fprintf(stdout, "%s\n", buf);
	fflush(stdout);
	if (!strcmp(buf, "C-C"))
	  break;
	//printf("%s\n", buf);
	/* { */
	/*   Window cur; */
	/*   int revert_to_return; */
	/*   XGetInputFocus(d, &cur, &revert_to_return); */
	/*   XSendEvent(d, cur, False, KeyPressMask, &ev); */
	/* } */
  }
  XCloseDisplay(d);
}
