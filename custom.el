;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(casual-info-use-unicode-symbols t)
 '(casual-lib-use-unicode t)
 '(completion-auto-select 'second-tab)
 '(copilot-idle-delay 1)
 '(copilot-max-char-warning-disable t)
 '(create-lockfiles nil)
 '(css-indent-offset 2)
 '(cua-enable-cua-keys nil)
 '(custom-enabled-themes '(smart-mode-line-dark wombat))
 '(custom-safe-themes
   '("cbe7f2b12e2739b720225769cdc3a69dfb8a31544d5f86960a3fbdae4c58c0b8"
     "68a0201c7bb9dba9c9b6fd6662d1f3daf8865860ba8fc56d0201be859da535fc"
     "45631691477ddee3df12013e718689dafa607771e7fd37ebc6c6eb9529a8ede5"
     "b2981f490579960b489803a8b874e570cf293fdf9065014ee1aaa0e6b523e8ae"
     "95ee4d370f4b66ff2287d8075f8fe5f58c4a9b9c1e65d663b15174f1a8c57717"
     "36d4b9573ed57b3c53261cb517eef2353058b7cf95b957f691f5ad066933ae84"
     "17e0f989a947f8026eb7044c07c11a36c6c901ee370dd8ce58a1e08544c5cf9f"
     "9b21c848d09ba7df8af217438797336ac99cbbbc87a08dc879e9291673a6a631"
     "fc1275617f9c8d1c8351df9667d750a8e3da2658077cfdda2ca281a2ebc914e0"
     "31deed4ac5d0b65dc051a1da3611ef52411490b2b6e7c2058c13c7190f7e199b"
     "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d"
     "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223"
     "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
     "f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a"
     "87de2a48139167bfe19e314996ee0a8d081a6d8803954bafda08857684109b4e"
     "a04676d7b664d62cf8cd68eaddca902899f98985fff042d8d474a0d51e8c9236"
     "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279"
     "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(devdocs-window-select nil)
 '(display-fill-column-indicator t)
 '(dockerfile-use-buildkit t)
 '(flycheck-checkers
   '(ada-gnat asciidoctor asciidoc awk-gawk bazel-build-buildifier
              bazel-module-buildifier bazel-starlark-buildifier
              bazel-workspace-buildifier c/c++-clang c/c++-gcc c/c++-cppcheck
              cfengine coffee coffee-coffeelint css-csslint css-stylelint
              cuda-nvcc cwl d-dmd dockerfile-hadolint elixir-credo emacs-lisp
              emacs-lisp-checkdoc ember-template erlang-rebar3 erlang
              eruby-erubis eruby-ruumba fortran-gfortran go-gofmt go-vet
              go-build go-test go-errcheck go-unconvert go-staticcheck groovy
              haml haml-lint handlebars haskell-stack-ghc haskell-ghc
              haskell-hlint html-tidy javascript-eslint javascript-jshint
              javascript-standard json-jsonlint json-python-json json-jq jsonnet
              less less-stylelint llvm-llc lua-luacheck lua
              markdown-markdownlint-cli markdown-mdl markdown-pymarkdown nix
              nix-linter opam perl perl-perlcritic php php-phpmd php-phpcs
              php-phpcs-changed processing proselint protobuf-protoc
              protobuf-prototool pug puppet-parser puppet-lint python-flake8
              python-ruff python-pylint python-pycompile python-pyright
              python-mypy r-lintr racket rpm-rpmlint rst-sphinx rst ruby-rubocop
              ruby-chef-cookstyle ruby-standard ruby-reek ruby ruby-jruby
              rust-cargo rust rust-clippy scala scala-scalastyle scheme-chicken
              scss-lint sass-stylelint scss-stylelint sass/scss-sass-lint sass
              scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim
              slim-lint sql-sqlint statix systemd-analyze tcl-nagelfar terraform
              terraform-tflint tex-chktex tex-lacheck texinfo textlint
              typescript-tslint verilog-verilator vhdl-ghdl xml-xmlstarlet
              xml-xmllint yaml-yamllint yaml-actionlint yaml-jsyaml yaml-ruby))
 '(flycheck-markdown-markdownlint-cli-config
   '(".markdownlint.json" ".markdownlint.jsonc" ".markdownlint.yaml"
     ".markdownlint"))
 '(flycheck-rubocop-lint-only t)
 '(global-highlight-parentheses-mode t)
 '(global-tree-sitter-mode t)
 '(global-treesit-auto-modes
   '(yaml-mode yaml-ts-mode wgsl-mode wgsl-ts-mode wat-mode wat-ts-mode wat-mode
               wat-ts-wast-mode vue-mode vue-ts-mode vhdl-mode vhdl-ts-mode
               verilog-mode verilog-ts-mode typst-mode typst-ts-mode
               typescript-mode typescript-ts-mode typescript-tsx-mode
               tsx-ts-mode toml-mode conf-toml-mode toml-ts-mode surface-mode
               surface-ts-mode sql-mode sql-ts-mode scala-mode scala-ts-mode
               rust-mode rust-ts-mode ruby-mode ruby-ts-mode ess-mode r-ts-mode
               python-mode python-ts-mode protobuf-mode protobuf-ts-mode
               perl-mode perl-ts-mode org-mode org-ts-mode nushell-mode
               nushell-ts-mode nix-mode nix-ts-mode markdown-mode
               poly-markdown-mode makefile-mode makefile-ts-mode lua-mode
               lua-ts-mode kotlin-mode kotlin-ts-mode julia-mode julia-ts-mode
               js-json-mode json-ts-mode js2-mode javascript-mode js-mode
               js-ts-mode java-mode java-ts-mode sgml-mode mhtml-mode
               html-ts-mode heex-mode heex-ts-mode go-mod-mode go-mod-ts-mode
               go-mode go-ts-mode glsl-mode glsl-ts-mode elixir-mode
               elixir-ts-mode dockerfile-mode dockerfile-ts-mode dart-mode
               dart-ts-mode css-mode css-ts-mode c++-mode c++-ts-mode
               common-lisp-mode commonlisp-ts-mode cmake-mode cmake-ts-mode
               clojurec-mode clojurescript-mode clojure-mode clojure-ts-mode
               csharp-mode csharp-ts-mode c-mode c-ts-mode blueprint-mode
               blueprint-ts-mode bibtex-mode bibtex-ts-mode sh-mode bash-ts-mode
               awk-mode awk-ts-mode))
 '(highlight-parentheses-colors '("#7ec98f" "#e5c06d" "#a4b5e6" "#834c98" "#8ac6f2"))
 '(indent-tabs-mode nil nil nil "Customized with use-package emacs")
 '(initial-buffer-choice t)
 '(js-chain-indent t)
 '(js-enabled-frameworks '(javascript extjs))
 '(js-indent-level 2)
 '(kill-buffer-delete-auto-save-files t nil nil "Customized with use-package emacs")
 '(kill-do-not-save-duplicates t)
 '(kill-read-only-ok t)
 '(kill-ring-max 256)
 '(kill-whole-line nil)
 '(lsp-bash-allowed-shells '(sh bash zsh))
 '(lsp-javascript-format-enable nil)
 '(lsp-pylsp-plugins-isort-enabled t)
 '(lsp-pylsp-plugins-preload-modules [])
 '(lsp-pylsp-plugins-ruff-enabled t)
 '(lsp-pylsp-plugins-ruff-exclude [])
 '(lsp-pylsp-plugins-ruff-extend-ignore [])
 '(lsp-pylsp-plugins-ruff-format [])
 '(lsp-pylsp-plugins-ruff-ignore [])
 '(lsp-pylsp-plugins-ruff-select [])
 '(lsp-pylsp-server-command '("uv run pylsp"))
 '(lsp-rubocop-use-bundler t)
 '(lsp-ruby-lsp-use-bundler t)
 '(lsp-typescript-format-enable nil)
 '(lsp-typescript-tsserver-trace "messages")
 '(major-mode-remap-alist
   '((css-mode . css-ts-mode) (js-json-mode . json-ts-mode) (go-mode . go-ts-mode)
     (c-mode . c-ts-mode) (c++-mode . c++-ts-mode) (rust . rust-ts-mode)
     (cmake-mode . cmake-ts-mode) (python-mode . python-ts-mode)
     (ruby-mode . ruby-ts-mode)))
 '(markdown-asymmetric-header t)
 '(markdown-enable-highlighting-syntax t)
 '(markdown-hide-urls t)
 '(markdown-special-ctrl-a/e t)
 '(mode-require-final-newline 'visit-save)
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus ol-info ol-irc ol-mhe
             ol-rmail ol-w3m))
 '(package-selected-packages
   '(amx elisp-autofmt elisp-lint elpy flycheck gptel-agent gptel-fn-complete
         ido-completing-read+ jinja2-mode kirigami lsp-treemacs markdown-ts-mode
         ob-ts-node org-beautify-theme ox-gfm page-break-lines prettier
         rainbow-delimiters shfmt smart-mode-line-powerline-theme terraform-mode
         treemacs-icons-dired treemacs-magit treesit-auto treesit-fold vterm vui
         vulpea vulpea-journal vulpea-ui web-mode which-key
         whitespace-cleanup-mode winpulse xkcd))
 '(safe-local-variable-values
   '((org-todo-keywords quote
                        ((sequence "TODO" "IN PROGRESS" "DEFERRED" "ON HOLD"
                                   "NEEDS INPUT" "|" "DONE" "CANCELED"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'custom)
;;; custom.el ends here
