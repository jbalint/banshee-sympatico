
CFLAGS += -I$(LUA_HOME)
CFLAGS += -DLUA_USE_APICHECK
CFLAGS += -ggdb3
CFLAGS += -fPIC
CFLAGS += -Wall

fdopen.so: fdopen.o
	gcc -shared -Wl,-export-dynamic -o fdopen.so fdopen.o

fdopen.o: fdopen.c

clean:
	rm fdopen.o fdopen.so
