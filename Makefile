CFLAGS = -g -O3 -Wall

ERLANG_PATH = $(shell erl -eval 'io:format("~s", [lists:concat([code:root_dir(), "/erts-", erlang:system_info(version), "/include"])])' -s init stop -noshell)
CFLAGS += -I$(ERLANG_PATH)
CFLAGS += -Ic_src

ifneq ($(OS),Windows_NT)
	CFLAGS += -fPIC

	ifeq ($(shell uname),Darwin)
		LDFLAGS += -dynamiclib -undefined dynamic_lookup
	endif
endif

NIF_SRC=\
	src/crc_nif.c\
	src/crc_16_ccitt.c

all: crc

crc:
	$(MIX) compile

priv/crc_nif.so: $(NIF_SRC)
	$(CC) $(CFLAGS) -shared $(LDFLAGS) -o $@ $(NIF_SRC)

clean:
	$(MIX) clean
	rm -f priv/crc.so src/*.o

.PHONY: all crc clean
