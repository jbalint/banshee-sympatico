CFLAGS += -ggdb3
CFLAGS += -Wall

#CFLAGS += -DNDEBUG

# TODO parameterize all paths in this file
CFLAGS += -I/home/jbalint/sw/xsb-src/XSB/config/x86_64-unknown-linux-gnu
CFLAGS += -I/home/jbalint/sw/xsb-src/XSB/emu
CFLAGS += -I/home/jbalint/sw/lua-5.2.2/include

# necessary to export ptoc_string from xsb.o to flora's shared objects
LDFLAGS += -Wl,-export-dynamic
LDFLAGS += -lm -lpthread -ldl
# TODO parameterise xsb.o location
kernel: kernel.o bs_xsb.o bs_lua.o \
	 /home/jbalint/sw/xsb-src/XSB/config/x86_64-unknown-linux-gnu/saved.o/xsb.o \
	/home/jbalint/sw/lua-5.2.2/lib/liblua.a
	$(CC) $(LDFLAGS) -o bsk $^

# Rule just to print a message if xsb.o isn't present
/home/jbalint/sw/xsb-src/XSB/config/x86_64-unknown-linux-gnu/saved.o/xsb.o:
	echo "Please install XSB properly"
