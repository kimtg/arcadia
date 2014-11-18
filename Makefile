BIN=arcadia
CFLAGS=-Wall -O3 -c
LDFLAGS=-s -lm

$(BIN): arcadia.o arc.o
	$(CC) -o $(BIN) arcadia.o arc.o $(LDFLAGS)

readline: CFLAGS+=-DREADLINE
readline: LDFLAGS+=-lreadline
readline: $(BIN)

arcadia.o: arcadia.c arc.h
	$(CC) $(CFLAGS) arcadia.c
arc.o: arc.c arc.h
	$(CC) $(CFLAGS) arc.c
run: $(BIN)
	./$(BIN)
clean:
	rm -f $(BIN) *.o
tag:
	etags *.h *.c
