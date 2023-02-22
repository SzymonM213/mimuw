CC       = gcc
CFLAGS   = -Wall -Wextra -Wno-implicit-fallthrough -O2 -std=c17

.PHONY: all clean

labyrinth: labyrinth.o input.o stack.o utils.o bfs.o
utils.o: utils.h
stack.o: stack.h utils.h input.h
input.o: utils.h input.h
bfs.o: stack.h utils.h bfs.h
labyrinth.o: utils.h stack.h input.h bfs.h

clean:
	rm -f *o labyrinth
