run: bin/rusty_scheme
	./bin/rusty_scheme

bin/rusty_scheme: *.rs
	rustc --color always -o bin/rusty_scheme main.rs

test: bin/test
	./bin/test

bin/test: *.rs
	rustc --color always --test -o bin/test main.rs

watch: FORCE
	kicker -e "make test" -c -l 0.1 *.rs

clean:
	rm -f bin/*

FORCE:
