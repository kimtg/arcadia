arcadia: arcadia.c
	gcc -s -Wall -static -O3 -o arcadia arcadia.c
run: arcadia
	./arcadia
clean:
	rm -f arcadia
