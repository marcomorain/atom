CCFLAGS = -std=c99 #-Wall

default: src/*.c src/*.h
	@mkdir -p bin
	cc ${CCFLAGS} -o bin/test src/atom.c test/test.c
	cc ${CCFLAGS} -o bin/atom src/atom.c src/linenoise/linenoise.c src/main.c
	@echo 'atom.c:' `cat src/atom.c | tr -dc ';' | wc -c` 'lines of code.'
	bin/test

clean :
	rm -f bin/test bin/atom
