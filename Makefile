BIN=arcadia

$(BIN): arcadia.c
	$(CC) -s -Wall -static -O3 -o $(BIN) arcadia.c
run: $(BIN)
	./$(BIN)
clean:
	rm -f $(BIN)
