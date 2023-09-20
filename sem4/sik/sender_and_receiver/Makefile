.PHONY: sikradio-sender sikradio-receiver clean wisniewski sluchaj

all:

sikradio-sender sikradio-receiver sender-test: 
	# gcc $@.c -g -Wall -Wextra -pthread -fsanitize=address -o $@
	gcc $@.c -g -Wall -Wextra -pthread -o $@

clean:
	rm -rf *.o
