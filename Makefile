.PHONY: build clean install setup

build: setup
	dune build
	cp -f _build/default/bin/main.exe bin/urme
	codesign -s - bin/urme 2>/dev/null || true

clean:
	dune clean
	rm -f bin/urme

setup:
	opam install yojson cohttp-lwt-unix lwt uuidm irmin-git -y

install:
	dune build
	cp -f _build/default/bin/main.exe bin/urme
	@echo "Binary installed at $(CURDIR)/bin/urme"
