CFLAGS = -Wall -Wextra -Werror -std=gnu11
CDEBUG = -g -DDEBUG -O0
CRELEASE = -DNDEBUG -O2
CC = gcc

.c.o:
	$(CC) $(CFLAGS) -c -o $@ $<

parse.o: parse.c parse.h

trie.o: trie.c trie.h

dictionary.o: dictionary.c trie.h parse.h

dictionary: dictionary.o trie.o parse.o
	$(CC) $(CFLAGS) -o dictionary dictionary.o trie.o parse.o

dictionary.dbg: dictionary.o trie.o parse.o
	$(CC) $(CFLAGS) -o dictionary.dbg dictionary.o trie.o parse.o

.PHONY: all clean debug

all: CFLAGS += $(CRELEASE)
all: dictionary

debug: CFLAGS += $(CDEBUG)
debug: dictionary.dbg

clean:
	rm trie.o parse.o dictionary.o dictionary dictionary.dbg

