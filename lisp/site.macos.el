;;; site.macos.el -- Initialization specific to MacOS -*- lexical-binding: t; -*-

;;; Commentary:
;; this file should have site-specifc customizations, which may override the values set in init.el or custom.el.

;;; Code:

(setq exec-path
      '("/usr/bin"
        "/bin"
        "/usr/sbin"
        "/sbin"
        "~/.nvm/versions/node/v21.6.1/bin"
        "/opt/homebrew/Cellar/pyenv-virtualenv/1.2.1/shims"
        "~/.pyenv/shims"
        "~/bin"
        "/opt/homebrew/bin"
        "/opt/homebrew/sbin"
        "/usr/local/bin"
        "/usr/local/go/bin"
        "~/.cargo/bin"
        "~/.local/bin"
        "~/go/bin"))
(setopt
 projectile-project-search-path
 '("~/.emacs.d" "~/Devel/Personal" ("~/Devel/Mobelux" . 3) "~/Devel/HackRVA" "~/Devel/ThirdParty")
 elisp-autofmt-python-bin
 "/Users/adrianflanagan/.pyenv/versions/3.12.2/bin/python"
 python-interpreter
 "/Users/adrianflanagan/.pyenv/shims/python"
 python-shell-exec-path
 '("/Users/adrianflanagan/.pyenv/shims")
 python-shell-interpreter
 "/Users/adrianflanagan/.pyenv/shims/python")

(message "lisp/site.macos.el loaded")

(provide "site.macos")
;;; site.macos.el ends here
