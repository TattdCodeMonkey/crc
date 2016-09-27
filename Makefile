# Variables to override
#
# CC            C compiler
# CROSSCOMPILE    crosscompiler prefix, if any
# CFLAGS    compiler flags for compiling all C files
# ERL_CFLAGS    additional compiler flags for files using Erlang header files
# ERL_EI_LIBDIR path to libei.a
# LDFLAGS    linker flags for linking all binaries
# ERL_LDFLAGS    additional linker flags for projects referencing Erlang libraries

# Look for the EI library and header files
# For crosscompiled builds, ERL_EI_INCLUDE_DIR and ERL_EI_LIBDIR must be
# passed into the Makefile.
ifeq ($(ERL_EI_INCLUDE_DIR),)
	ERL_ROOT_DIR = $(shell erl -eval "io:format(\"~s~n\", [code:root_dir()])" -s init stop -noshell)
ifeq ($(ERL_ROOT_DIR),)
	$(error Could not find the Erlang installation. Check to see that 'erl' is in your PATH)
endif
ERL_EI_INCLUDE_DIR = "$(ERL_ROOT_DIR)/usr/include"
ERL_EI_LIBDIR = "$(ERL_ROOT_DIR)/usr/lib"
endif

# Set Erlang-specific compile and linker flags
ERL_CFLAGS ?= -I$(ERL_EI_INCLUDE_DIR)
ERL_LDFLAGS ?= -L$(ERL_EI_LIBDIR)

LDFLAGS += -fPIC -shared  -dynamiclib
CFLAGS ?= -fPIC -O2 -Wall -Wextra -Wno-unused-parameter
CC ?= $(CROSSCOMPILER)gcc

ifeq ($(CROSSCOMPILE),)
	ifeq ($(shell uname),Darwin)
	LDFLAGS += -undefined dynamic_lookup
endif
endif

NIF_SRC=\
	src/crc_16.c\
	src/crc_16_ccitt.c\
	src/crc_16_modbus.c\
	src/crc_nif.c

.PHONY: all clean

all: crc

%.o: %.c
		$(CC) -c $(ERL_CFLAGS) $(CFLAGS) -o $@ $<

priv/crc_nif.so: $(NIF_SRC)
		@mkdir -p priv
		$(CC) $^ $(ERL_LDFLAGS) $(LDFLAGS) $(ERL_CFLAGS) -o $@

clean:
	$(MIX) clean
	rm -f priv/crc.so src/*.o
