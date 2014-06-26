CFLAGS += -ggdb3
CFLAGS += -Wall

#CFLAGS += -DNDEBUG

# TODO parameterize all paths in this file
CFLAGS += -I/home/jbalint/sw/xsb-src/XSB/config/x86_64-unknown-linux-gnu
CFLAGS += -I/home/jbalint/sw/xsb-src/XSB/emu

# necessary to export ptoc_string from xsb.o to flora's shared objects
LDFLAGS += -Wl,-export-dynamic

# TODO parameterise xsb.o location
kernel: kernel.o bs_xsb.o /home/jbalint/sw/xsb-src/XSB/config/x86_64-unknown-linux-gnu/saved.o/xsb.o -lm -lpthread -ldl
	$(CC) $(LDFLAGS) -o bsk $^
