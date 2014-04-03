/*
  Example of how key events can be processed and intercepted with X11

  this currently causes a few problems with programs like Chromium and
  Pidgin that use their own grabs for menus:

 [2506281.596] (II) Printing all currently active device grabs:
 [2506281.596] Active grab 0x1200000 (core) on device 'Virtual core pointer' (2):
 [2506281.596]       client pid 11076 pidgin
 [2506281.596]       at 2506256680 (from passive grab) (implicit) (device thawed, state 1)
 [2506281.596]         core event mask 0x63807f
 [2506281.596]       passive grab type 4, detail 0x0, activating key 0
 [2506281.596]       owner-events false, kb 1 ptr 1, confine 0, cursor 0x0
 [2506281.596] Active grab 0x2400000 (core) on device 'Virtual core keyboard' (3):
 [2506281.596]       client pid 19562 ./replay_test
 [2506281.596]       at 2506280587 (from active grab) (device thawed, state 3)
 [2506281.596]         core event mask 0x3
 [2506281.596]       owner-events false, kb 0 ptr 1, confine 0, cursor 0x0
 [2506281.596] (II) End list of active device grabs

*/
#include <X11/Xlib.h>

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>

const char *type(XEvent *ev) {
  const char *type;
  if (ev->xkey.type == KeyPress)
	type = "KeyPress";
  else if (ev->xkey.type == KeyRelease)
	type = "KeyRelease";
  else
	type = "Unknown";
  return type;
}

int main(int argc, char **argv)
{
  Display *d;
  XEvent ev;
  int ret;
  d = XOpenDisplay(NULL);

  for (;;) {
	/* Initiate the SYNCHRONOUS grab. This will freeze keyboard event
	   processing */
	ret = XGrabKeyboard(d, DefaultRootWindow(d), False,
						GrabModeAsync,
						GrabModeSync,
						CurrentTime);
	assert(!ret);
	/* Allow the next event to be delivered */
	XAllowEvents(d, SyncKeyboard, CurrentTime);
	XAllowEvents(d, AsyncPointer, CurrentTime);
	/* Get the event */
	assert(!XWindowEvent(d, DefaultRootWindow(d), KeyPressMask|KeyReleaseMask, &ev));
	fprintf(stderr, "%s=%d\n", type(&ev), ev.xkey.keycode);
	fflush(stderr);
	/* (optionally) Replay the keyboard event and terminate the grab */
	if (ev.xkey.keycode != 25) { /* to test, ignore 'w' key presses */
	  (!XAllowEvents(d, ReplayKeyboard, CurrentTime));
	} else {
	  fprintf(stderr, "w\n");
	  (!XAllowEvents(d, AsyncKeyboard, CurrentTime));
	}
  }

  sleep(10);
  return 0;
}
