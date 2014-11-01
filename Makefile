all: bin/rusty_scheme

run: bin/rusty_scheme
	./bin/rusty_scheme

bin/rusty_scheme: src/*.rs
	rustc --color always -o bin/rusty_scheme src/main.rs

test: bin/test
	./bin/test

bin/test: src/*.rs
	rustc --color always --test -o bin/test src/main.rs

watch: FORCE
	kicker -e "make test" -c -l 0.1 src/*.rs

clean:
	rm -f bin/*

FORCE:
