MIX = mix
CFLAGS = -g -O3 -ansi -pedantic -Wall -Wextra -Wno-unused-parameter

ERLANG_PATH = $(shell erl -eval 'io:format("~s", [lists:concat([code:root_dir(), "/erts-", erlang:system_info(version), "/include"])])' -s init stop -noshell)
# Look for the EI library and header files
ERL_EI_INCLUDE_DIR = "/usr/local/lib/erlang/usr/include"
ERL_EI_LIBDIR = "/usr/local/lib/erlang/usr/lib"

ifeq ($(ERL_EI_INCLUDE_DIR),)
   $(error Could not find include directory for ei.h. Check that Erlang header files are available)
endif
ifeq ($(ERL_EI_LIBDIR),)
   $(error Could not find libei.a. Check your Erlang installation)
endif

# Set Erlang-specific compile and linker flags
ERL_CFLAGS ?= -I$(ERL_EI_INCLUDE_DIR)
ERL_LDFLAGS ?= -L$(ERL_EI_LIBDIR) -lei

LDFLAGS +=
CFLAGS += -I$(ERLANG_PATH)

ifneq ($(OS),Windows_NT)
	CFLAGS += -fPIC

	ifeq ($(shell uname),Darwin)
		LDFLAGS += -dynamiclib -undefined dynamic_lookup
	endif
endif

.PHONY: all crc clean

all: crc

crc:
	$(MIX) compile

%.o: %.c
	$(CC) -c $(ERL_CFLAGS) $(CFLAGS) -o $@ $<

priv/crc.so: src/crc_nif.o src/crc_16_ccitt.o
	@mkdir -p priv
	$(CC) $^ $(ERL_LDFLAGS) $(LDFLAGS) -o $@

clean:
	$(MIX) clean
	rm -f priv/crc.so src/*.o

realclean:
	rm -fr _build priv/crc.so src/*.o
