.PHONY: clean list-targets

# remove any compiled files, leaving source
clean:
	find . -name elpa -prune -o -name '*.elc' -delete

smoke:
	emacs -Q --batch \
	--eval '(setq user-emacs-directory "/Users/adrianflanagan/.config/emacs/")' \
	--eval '(setq init-file-debug t)' \
	-L . -l init.el

list-targets:
	@grep '^[a-zA-Z._-]\+:' Makefile | cut -d: -f1 | grep -v .PHONY
