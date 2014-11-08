BIN=arcadia
CFLAGS=-s -Wall -O3
$(BIN): arcadia.o arc.o
	$(CC) $(CFLAGS) -static -o $(BIN) arcadia.o arc.o
arcadia.o: arcadia.c arc.h
	$(CC) $(CFLAGS) -c arcadia.c
arc.o: arc.c arc.h
	$(CC) $(CFLAGS) -c arc.c
run: $(BIN)
	./$(BIN)
clean:
	rm -f $(BIN) *.o
