CFLAGS=-c -g -Wall
CC=gcc
CPP=g++

all: atom

atom: atom.o linenoise.o
	$(CC) -lstdc++ atom.o linenoise.o -o atom

atom.o: atom.h atom.cpp
	$(CPP) $(CFLAGS) atom.cpp

linenoise.o: linenoise.h linenoise.h
	$(CC) $(CFLAGS) linenoise.c

clean:
	rm -f *.o atom
