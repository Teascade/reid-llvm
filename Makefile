SRC=foo.reid
BIN=$(SRC:.reid=.out)

REID=cargo run --example cli
LD=ld
LDFLAGS=-lSDL3

all: $(BIN)
clean:
	rm -f $(BIN) $(SRC:.reid=.o) $(SRC:.reid=.asm) $(SRC:.reid=.ll)

$(BIN): $(SRC:.reid=.o)
	$(LD) -dynamic-linker /lib64/ld-linux-x86-64.so.2 /usr/lib/crt1.o -lc $(LDFLAGS) $< -o$@

.SUFFIXES: .o .reid
.reid.o:
	$(REID) $<