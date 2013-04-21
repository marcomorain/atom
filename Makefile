CCFLAGS = -std=c99 #-Wall

default: src/*.c src/*.h
	@mkdir -p bin
	cc ${CCFLAGS} -o bin/test src/atom.c src/atom_lib.c test/test.c
	cc ${CCFLAGS} -o bin/atom src/atom.c src/atom_lib.c src/linenoise/linenoise.c src/main.c
	@echo `cat src/atom.c src/atom_lib.c | tr -dc ';' | wc -c` 'lines of code.'
	bin/test > /dev/null

clean :
	rm -f bin/test bin/atom
