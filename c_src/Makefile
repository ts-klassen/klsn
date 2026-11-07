CC ?= cc
CFLAGS ?= -O2

.PHONY: all clean

all: ../priv/klsn_bwrap_launcher

../priv/klsn_bwrap_launcher: klsn_bwrap_launcher.c
	mkdir -p ../priv
	$(CC) $(CPPFLAGS) $(CFLAGS) -std=c11 -Wall -Wextra -Werror \
		$(LDFLAGS) -o "$@" "$<"

clean:
	if test -f ../priv/klsn_bwrap_launcher; then \
		rm -- ../priv/klsn_bwrap_launcher; \
	fi
