.PHONY: clean list-targets

# remove any compiled files, leaving source
clean:
	find . -name elpa -prune -o -name '*.elc' -delete

list-targets:
	@grep '^[a-zA-Z._-]\+:' Makefile | cut -d: -f1 | grep -v .PHONY
