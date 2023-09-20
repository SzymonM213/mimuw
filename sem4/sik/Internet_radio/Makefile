CC = g++
CFLAGS = -Wall -Wextra -O2 -std=gnu++17 -pthread
.PHONY: all clean sikradio-receiver sikradio-sender send receive

all: sikradio-receiver sikradio-sender

sikradio-sender sikradio-receiver remix: 
	$(CC) $(CFLAGS) $@.cpp err.h utils.h -o $@

receive:
	./sikradio-receiver -b 30000 -n "popek" 2>chuj.txt | play -t raw -c 2 -r 44100 -b 16 -e signed-integer --buffer 32768 -

clean:
	rm -rf *.o sikradio-receiver sikradio-sender