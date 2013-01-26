default: src/*.c src/*.h
	mkdir -p bin
	cc -std=c99 -o bin/test src/atom.c test/test.c
	cc -std=c99 -o bin/atom src/atom.c src/linenoise.c src/main.c
	./bin/test
