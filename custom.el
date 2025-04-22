(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(casual-info-use-unicode-symbols t)
 '(casual-lib-use-unicode t)
 '(completion-auto-select t)
 '(copilot-idle-delay 1)
 '(copilot-max-char-warning-disable t)
 '(create-lockfiles nil)
 '(cua-enable-cua-keys nil)
 '(custom-enabled-themes '(smart-mode-line-dark wombat))
 '(custom-safe-themes
   '("36d4b9573ed57b3c53261cb517eef2353058b7cf95b957f691f5ad066933ae84"
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
 '(display-line-numbers nil)
 '(display-line-numbers-major-tick 0)
 '(display-line-numbers-minor-tick 0)
 '(dockerfile-use-buildkit t)
 '(ede-project-directories nil)
 '(elisp-autofmt-format-quoted nil)
 '(elisp-autofmt-use-default-override-defs t)
 '(elpy-disable-backend-error-display nil)
 '(elpy-formatter 'black)
 '(elpy-modules
   '(elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-folding elpy-module-pyvenv
                         elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-django
                         elpy-module-sane-defaults))
 '(fill-column 120)
 '(flycheck-checkers
   '(ada-gnat asciidoctor asciidoc awk-gawk bazel-build-buildifier bazel-module-buildifier bazel-starlark-buildifier
              bazel-workspace-buildifier c/c++-clang c/c++-gcc c/c++-cppcheck cfengine coffee coffee-coffeelint
              css-csslint css-stylelint cuda-nvcc cwl d-dmd dockerfile-hadolint elixir-credo emacs-lisp
              emacs-lisp-checkdoc ember-template erlang-rebar3 erlang eruby-erubis eruby-ruumba fortran-gfortran
              go-gofmt go-vet go-build go-test go-errcheck go-unconvert go-staticcheck groovy haml haml-lint handlebars
              haskell-stack-ghc haskell-ghc haskell-hlint html-tidy javascript-eslint javascript-jshint
              javascript-standard json-jsonlint json-python-json json-jq jsonnet less less-stylelint llvm-llc
              lua-luacheck lua markdown-markdownlint-cli markdown-mdl markdown-pymarkdown nix nix-linter opam perl
              perl-perlcritic php php-phpmd php-phpcs php-phpcs-changed processing proselint protobuf-protoc
              protobuf-prototool pug puppet-parser puppet-lint python-flake8 python-ruff python-pylint python-pycompile
              python-pyright python-mypy r-lintr racket rpm-rpmlint rst-sphinx rst ruby-rubocop ruby-chef-cookstyle
              ruby-standard ruby-reek ruby ruby-jruby rust-cargo rust rust-clippy scala scala-scalastyle scheme-chicken
              scss-lint sass-stylelint scss-stylelint sass/scss-sass-lint sass scss sh-bash sh-posix-dash sh-posix-bash
              sh-zsh sh-shellcheck slim slim-lint sql-sqlint statix systemd-analyze tcl-nagelfar terraform
              terraform-tflint tex-chktex tex-lacheck texinfo textlint typescript-tslint verilog-verilator vhdl-ghdl
              xml-xmlstarlet xml-xmllint yaml-yamllint yaml-actionlint yaml-jsyaml yaml-ruby))
 '(flycheck-markdown-markdownlint-cli-config
   '(".markdownlint.json" ".markdownlint.jsonc" ".markdownlint.yaml" ".markdownlint"))
 '(flycheck-rubocop-lint-only t)
 '(global-highlight-parentheses-mode t)
 '(global-prettify-symbols-mode t)
 '(global-tree-sitter-mode t)
 '(highlight-parentheses-colors '("#7ec98f" "#e5c06d" "#a4b5e6" "#834c98" "#8ac6f2"))
 '(ido-big-directories '("node_modules" "\\.?venv"))
 '(ido-cannot-complete-command 'ido-completion-help)
 '(ido-enable-flex-matching nil)
 '(ido-ignore-directories '("\\`CVS/" "\\`\\.\\./" "\\`\\./" "\\`__pycache__/"))
 '(ido-use-filename-at-point nil)
 '(ido-use-url-at-point t)
 '(ido-use-virtual-buffers 'auto)
 '(indent-tabs-mode nil)
 '(initial-buffer-choice t)
 '(js-chain-indent t)
 '(js-enabled-frameworks '(javascript extjs))
 '(js-indent-level 2)
 '(kill-do-not-save-duplicates t)
 '(kill-read-only-ok t)
 '(kill-ring-max 256)
 '(kill-whole-line nil)
 '(lsp-javascript-format-enable nil)
 '(lsp-pylsp-plugins-isort-enabled t)
 '(lsp-pylsp-plugins-preload-modules [])
 '(lsp-pylsp-plugins-ruff-enabled t)
 '(lsp-pylsp-plugins-ruff-exclude [])
 '(lsp-pylsp-plugins-ruff-extend-ignore [])
 '(lsp-pylsp-plugins-ruff-format [])
 '(lsp-pylsp-plugins-ruff-ignore [])
 '(lsp-pylsp-plugins-ruff-select [])
 '(lsp-rubocop-use-bundler t)
 '(lsp-ruby-lsp-use-bundler t)
 '(lsp-typescript-format-enable nil)
 '(lsp-typescript-tsserver-trace "messages")
 '(major-mode-remap-alist
   '((css-mode . css-ts-mode) (js-json-mode . json-ts-mode) (go-mode . go-ts-mode) (c-mode . c-ts-mode)
     (c++-mode . c++-ts-mode) (rust . rust-ts-mode) (cmake-mode . cmake-ts-mode) (python-mode . python-ts-mode)
     (ruby-mode . ruby-ts-mode)))
 '(markdown-asymmetric-header t)
 '(markdown-enable-highlighting-syntax t)
 '(markdown-hide-urls t)
 '(markdown-special-ctrl-a/e t)
 '(mode-require-final-newline 'visit-save)
 '(org-agenda-files
   '("~/org/personal/job_search/conversations.org" "/Users/adrianflanagan/org/personal/job_search/search_log.org"
     "/Users/adrianflanagan/org/index.org" "/Users/adrianflanagan/org/elisp.org"
     "/Users/adrianflanagan/org/emacs_tips.org" "/Users/adrianflanagan/org/org-todo.org"
     "/Users/adrianflanagan/org/quotes.org" "/Users/adrianflanagan/org/personal/appointments.org"
     "/Users/adrianflanagan/org/personal/archery.org" "/Users/adrianflanagan/org/personal/contacts.org"
     "/Users/adrianflanagan/org/personal/how_to_report_bugs.org" "/Users/adrianflanagan/org/personal/rules_of_thumb.org"
     "/Users/adrianflanagan/org/personal/todo-daily.org" "/Users/adrianflanagan/org/personal/todo-emacs.org"
     "/Users/adrianflanagan/org/personal/todo-house.org" "/Users/adrianflanagan/org/personal/todo-main.org"
     "/Users/adrianflanagan/org/personal/todo-periodic.org" "/Users/adrianflanagan/org/personal/todo-weekly.org"
     "/Users/adrianflanagan/org/personal/job_search/LinkedIn_Connections.org"
     "/Users/adrianflanagan/org/personal/job_search/online-sites.org"
     "/Users/adrianflanagan/org/personal/job_search/todo-jobs.org"
     "/Users/adrianflanagan/org/personal/job_search/unemp_ins.org"))
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-w3m org-mac-iCal))
 '(org-todo-keywords '((sequence "TODO" "IN PROGRESS" "ON HOLD" "DONE(!)")))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/") ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(all-the-icons amx casual-info chatgpt-shell chatu company-terraform company-web copilot csv-mode cyberpunk-theme
                   dashboard-hackernews devdocs djangonaut dockerfile-mode dogears easysession eldoc-box elisp-autofmt
                   elisp-lint elpy emacsql emacsql-pg emmet-mode fira-code-mode flycheck haki-theme helpful
                   highlight-parentheses iceberg-theme ivy jetbrains-darcula-theme kaolin-themes leuven-theme
                   lisp-extra-font-lock lsp-origami lsp-sourcekit lsp-treemacs lsp-ui magit-todos ng2-mode nice-org-html
                   nodejs-repl nushell-ts-mode org-beautify-theme org-chef org-contacts org-elisp-help org-mac-link
                   org-node org-recur org-special-block-extras org-superstar osx-lib ox-gfm page-break-lines prettier
                   pyenv-mode rainbow-delimiters smart-mode-line-powerline-theme spider-man-theme sql-indent swift-mode
                   swift-ts-mode terraform-doc tree-sitter treemacs-magit treemacs-projectile treesit-auto
                   unspecified-theme uv-mode web-mode which-key whitespace-cleanup-mode xkcd))
 '(prettify-symbols-unprettify-at-point 'right-edge)
 '(projectile-globally-ignored-directories
   '("^\\.idea$" "^\\.vscode$" "^\\.ensime_cache$" "^\\.eunit$" "^\\.git$" "^\\.hg$" "^\\.fslckout$" "^_FOSSIL_$"
     "^\\.bzr$" "^_darcs$" "^\\.pijul$" "^\\.tox$" "^\\.svn$" "^\\.stack-work$" "^\\.ccls-cache$" "^\\.cache$"
     "^\\.clangd$" "^\\.sl$" "^\\.jj$" "*^node-modules$" "*^\\.venv$"))
 '(projectile-mode t nil (projectile))
 '(projectile-project-search-path '("~/Devel" "~/org"))
 '(reddigg-subs '(emacs rust))
 '(ruby-flymake-use-rubocop-if-available t)
 '(safe-local-variable-directories
   '("/Users/adrianflanagan/Devel/personal/people/" "~/org/projects/Springbok/"))
 '(safe-local-variable-values
   '((elisp-lint-ignored-validators 'package-lint) (web-mode-indent-style . 2) (web-mode-block-padding . 2)
     (web-mode-script-padding . 2) (web-mode-style-padding . 2) (pyenv-workon . myogram) (hcl-indent-level . 2)
     (lsp-typescript-sdk . "/Users/adrianflanagan/Devel/mobelux/MMS/gatsby-source-mms/node_modules/typescript/bin")
     (org-todo-keywords quote ((sequence "TODO" "IN PROGRESS" "DEFERRED" "CLIENT" "|" "DONE" "CANCELED")))
     (org-todo-keywords quote ((sequence "TODO" "IN PROGRESS" "DEFERRED" "ON HOLD" "NEEDS INPUT" "|" "DONE" "CANCELED")))
     (org-todo-keywords quote ((sequence "TODO" "IN PROGRESS" "DEFFERED" "|" "DONE" "CANCELED")))
     (checkdoc-package-keywords-flag) (eglot-inlay-hints-mode)
     (org-todo-keywords quote ((sequence "TODO" "IN PROGRESS" "DEFERRED" "|" "DONE" "CANCELED")))
     (lexical-binding . true)))
 '(show-trailing-whitespace nil)
 '(sml/theme 'dark)
 '(sql-product 'postgres)
 '(tab-always-indent 'complete)
 '(track-eol t)
 '(tree-sitter-major-mode-language-alist
   '((actionscript-mode . actionscript) (ada-mode . ada) (agda-mode . agda) (agda2-mode . agda) (arduino-mode . arduino)
     (astro-mode . astro) (fish-mode . fish) (asm-mode . asm) (fasm-mode . asm) (masm-mode . asm) (nasm-mode . asm)
     (gas-mode . asm) (sh-mode . bash) (beancount-mode . beancount) (bibtex-mode . bibtex) (c-mode . c)
     (caml-mode . ocaml) (clojure-mode . clojure) (lisp-mode . commonlisp) (lisp-interaction-mode . commonlisp)
     (csharp-mode . c-sharp) (c++-mode . cpp) (cmake-mode . cmake) (d-mode . d) (dart-mode . dart)
     (dockerfile-mode . dockerfile) (css-mode . css) (csv-mode . csv) (elm-mode . elm) (elixir-mode . elixir)
     (emacs-lisp-mode . elisp) (erlang-mode . erlang) (ess-r-mode . r) (fennel-mode . fennel) (f90-mode . fortran)
     (fortran-mode . fortran) (gdscript-mode . gdscript) (git-commit-mode . gitcommit) (git-rebase-mode . git-rebase)
     (gitattributes-mode . gitattributes) (gitignore-mode . gitignore) (gleam-mode . gleam) (glsl-mode . glsl)
     (go-mode . go) (groovy-mode . groovy) (jenkinsfile-mode . groovy) (haskell-mode . haskell) (hcl-mode . hcl)
     (terraform-mode . hcl) (heex-mode . heex) (hlsl-mode . hlsl) (html-mode . html) (markdown-mode . markdown)
     (mhtml-mode . html) (nix-mode . nix) (jai-mode . jai) (java-mode . java) (javascript-mode . javascript)
     (js-mode . javascript) (js2-mode . javascript) (js3-mode . javascript) (json-mode . json) (jsonc-mode . json)
     (jsonnet-mode . jsonnet) (julia-mode . julia) (kotlin-mode . kotlin) (latex-mode . latex) (llvm-mode . llvm)
     (llvm-mir-mode . llvm-mir) (lua-mode . lua) (magik-mode . magik) (makefile-mode . make)
     (makefile-automake-mode . make) (makefile-gmake-mode . make) (makefile-makepp-mode . make)
     (makefile-bsdmake-mode . make) (makefile-imake-mode . make) (matlab-mode . matlab) (mermaid-mode . mermaid)
     (meson-mode . meson) (typescript-ts-mode . typescript) (ninja-mode . ninja) (noir-mode . noir) (ocaml-mode . ocaml)
     (org-mode . org) (pascal-mode . pascal) (perl-mode . perl) (php-mode . php) (qss-mode . css) (prisma-mode . prisma)
     (python-mode . python) (pygn-mode . pgn) (racket-mode . racket) (rjsx-mode . javascript) (rst-mode . rst)
     (ruby-mode . ruby) (rust-mode . rust) (rustic-mode . rust) (scala-mode . scala) (scheme-mode . scheme)
     (solidity-mode . solidity) (smithy-mode . smithy) (sql-mode . sql) (swift-mode . swift) (tablegen-mode . tablegen)
     (toml-mode . toml) (conf-toml-mode . toml) (tcl-mode . tcl) (tuareg-mode . ocaml) (twig-mode . twig)
     (tsx-ts-mode . tsx) (typst-mode . typst) (verilog-mode . verilog) (vhdl-mode . vhdl) (nxml-mode . xml)
     (yaml-mode . yaml) (k8s-mode . yaml) (zig-mode . zig)))
 '(treesit-auto-langs
   '(awk bash bibtex blueprint c c-sharp clojure cmake commonlisp cpp css dart dockerfile elixir glsl go gomod heex html
         java javascript json julia kotlin lua make markdown nix nu org perl proto python r ruby rust scala sql surface
         toml tsx typescript typst verilog vhdl vue wast wat wgsl yaml))
 '(typescript-enabled-frameworks '(typescript))
 '(typescript-indent-level 2)
 '(undo-limit 524288)
 '(undo-no-redo t)
 '(undo-outer-limit 262144)
 '(undo-strong-limit 655360)
 '(warning-minimum-level :error)
 '(which-key-mode t)
 '(whitespace-style
   '(face trailing tabs spaces lines newline missing-newline-at-eof empty indentation space-after-tab space-before-tab
          space-mark tab-mark newline-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(enh-ruby-string-delimiter-face ((t (:foreground "DarkOrchid3"))))
 '(erm-syn-errline ((t (:box (:line-width (1 . 1) :color "red")))))
 '(fixed-pitch ((t nil)))
 '(line-number-major-tick ((t (:weight bold))))
 '(line-number-minor-tick ((t (:slant italic :weight medium)))))

(provide 'custom)
;;; custom.el ends here
