BIN=arcadia
CFLAGS=-Wall -Ofast -c -DREAD_LINE=1
LDFLAGS=-s -lm -lreadline

$(BIN): arcadia.o arc.o
	$(CC) $(LDFLAGS) -o $(BIN) arcadia.o arc.o

no-read-line: LDFLAGS:=$(filter-out -lreadline,$(LDFLAGS))
no-read-line: CFLAGS+=-UREAD_LINE -DREAD_LINE=0
no-read-line: $(BIN)

arcadia.o: arcadia.c arc.h
	$(CC) $(CFLAGS) arcadia.c
arc.o: arc.c arc.h
	$(CC) $(CFLAGS) arc.c
run: $(BIN)
	./$(BIN)
clean:
	rm -f $(BIN) *.o
