.PHONY: build clean install setup

build: setup
	dune build
	cp -f _build/default/bin/main.exe bin/experience-agent
	codesign -s - bin/experience-agent 2>/dev/null || true

clean:
	dune clean
	rm -f bin/experience-agent

setup:
	opam install yojson cohttp-lwt-unix lwt uuidm irmin-git -y

install:
	dune build
	cp -f _build/default/bin/main.exe bin/experience-agent
	@echo "Binary installed at $(CURDIR)/bin/experience-agent"
