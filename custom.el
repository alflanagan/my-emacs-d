(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(create-lockfiles nil)
 '(custom-safe-themes
   '("b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d"
     "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223"
     "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
     "f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a"
     "87de2a48139167bfe19e314996ee0a8d081a6d8803954bafda08857684109b4e"
     "a04676d7b664d62cf8cd68eaddca902899f98985fff042d8d474a0d51e8c9236"
     "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279"
     "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(display-fill-column-indicator t)
 '(ede-project-directories nil)
 '(fill-column 120)
 '(global-prettify-symbols-mode t)
 '(global-tree-sitter-mode t)
 '(indent-tabs-mode nil)
 '(initial-buffer-choice t)
 '(js-chain-indent t)
 '(js-indent-level 2)
 '(kill-ring-max 5000)
 '(major-mode-remap-alist
   '((css-mode . css-ts-mode) (js-json-mode . json-ts-mode) (go-mode . go-ts-mode) (c-mode . c-ts-mode)
     (c++-mode . c++-ts-mode) (rust . rust-ts-mode) (cmake-mode . cmake-ts-mode) (python-mode . python-ts-mode)))
 '(markdown-code-lang-modes
   '(("ocaml" . tuareg-mode) ("elisp" . emacs-lisp-mode) ("ditaa" . artist-mode) ("asymptote" . asy-mode)
     ("dot" . fundamental-mode) ("sqlite" . sql-mode) ("calc" . fundamental-mode) ("C" . c-mode) ("cpp" . c++-mode)
     ("C++" . c++-mode) ("screen" . shell-script-mode) ("shell" . sh-mode) ("bash" . sh-mode) ("sh" . sh-mode)))
 '(mode-require-final-newline 'visit-save)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/") ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(async auto-header auto-rename-tag better-defaults blacken cargo-mode cmake-mode company company-jedi company-math
           company-shell company-terraform company-web counsel-projectile css-eldoc devdocs django-snippets
           docker-compose-mode dockerfile-mode dumb-jump editorconfig editorconfig-generate elisp-autofmt elisp-def
           elisp-lint elisp-refs emmet-mode eslint-disable-rule eslint-fix flycheck-aspell flycheck-bashate
           flycheck-cask flycheck-clang-tidy flycheck-eglot flycheck-golangci-lint flycheck-jest flycheck-kotlin
           flycheck-mypy flycheck-package flycheck-pycheckers flycheck-relint flycheck-rust flylisp focus-autosave-mode
           form-feed-st forth-mode gameoflife git-modes gnu-elpa-keyring-update go go-autocomplete go-projectile
           go-scratch guru-mode highlight-parentheses hl-todo ibuffer-projectile ietf-docs immaterial-theme js2-mode
           kotlin-ts-mode lispy lsp-origami magit markdown-toc material-theme morlock mwim ng2-mode nov org-contrib
           org-modern origami parrot projectile-codesearch projectile-speedbar pyenv-mode rainbow-delimiters reddigg
           rust-mode slime smart-mode-line-powerline-theme super-save term-projectile tide tree-sitter-indent
           treesit-auto typescript-mode w3m web-beautify web-mode weyland-yutani-theme ws-butler))
 '(prettify-symbols-unprettify-at-point 'right-edge)
 '(projectile-mode t nil (projectile))
 '(safe-local-variable-values
   '((python-shell-exec-path . "/Users/adrianflanagan/.pyenv/shims")
     (org-todo-keywords quote ((sequence "TODO" "IN PROGRESS" "DEFFERED" "|" "DONE" "CANCELED")))
     (python-interpreter . "/Users/adrianflanagan/.pyenv/versions/silverpoint-web/bin/python")
     (python-shell-interpreter . "/Users/adrianflanagan/.pyenv/versions/silverpoint-web/bin/python")))
 '(show-trailing-whitespace t)
 '(sql-product 'postgres)
 '(tab-width 4)
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
     (meson-mode . meson) (ng2-mode . typescript) (ninja-mode . ninja) (noir-mode . noir) (ocaml-mode . ocaml)
     (org-mode . org) (pascal-mode . pascal) (perl-mode . perl) (php-mode . php) (qss-mode . css) (prisma-mode . prisma)
     (python-mode . python) (pygn-mode . pgn) (racket-mode . racket) (rjsx-mode . javascript) (rst-mode . rst)
     (ruby-mode . ruby) (rust-mode . rust) (rustic-mode . rust) (scala-mode . scala) (scheme-mode . scheme)
     (solidity-mode . solidity) (smithy-mode . smithy) (sql-mode . sql) (swift-mode . swift) (tablegen-mode . tablegen)
     (toml-mode . toml) (conf-toml-mode . toml) (tcl-mode . tcl) (tuareg-mode . ocaml) (twig-mode . twig)
     (typescript-ts-mode . typescript) (tsx-ts-mode . tsx) (typst-mode . typst) (verilog-mode . verilog)
     (vhdl-mode . vhdl) (nxml-mode . xml) (yaml-mode . yaml) (k8s-mode . yaml) (zig-mode . zig)))
 '(typescript-enabled-frameworks '(typescript prototype dojo))
 '(warning-minimum-level :error)
 '(whitespace-style
   '(face trailing tabs spaces lines newline missing-newline-at-eof empty indentation space-after-tab space-before-tab
          space-mark tab-mark newline-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 170 :width normal :foundry "nil" :family "Fira Code")))))
