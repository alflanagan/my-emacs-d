;; this file should have site-specifc customizations, which may override the values set in init.el or custom.el.

(setq projectile-project-search-path
      '("~/.emacs.d" "~/Devel/Personal" ("~/Devel/Mobelux" . 3) "~/Devel/HackRVA" "~/Devel/ThirdParty"))
(setq elisp-autofmt-python-bin "/Users/adrianflanagan/.pyenv/versions/3.12.2/bin/python")
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
        "~/go/bin"
        "/opt/homebrew/Cellar/emacs-plus@30/30.0.50/libexec/emacs/30.0.50/aarch64-apple-darwin23.2.0"))
(setq python-interpreter "/Users/adrianflanagan/.pyenv/shims/python")
(setq python-shell-exec-path '("/Users/adrianflanagan/.pyenv/shims"))
(setq python-shell-interpreter "/Users/adrianflanagan/.pyenv/shims/python")
