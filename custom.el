(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(casual-info-use-unicode-symbols t)
 '(casual-lib-use-unicode t)
 '(create-lockfiles nil)
 '(cua-enable-cua-keys nil)
 '(custom-safe-themes
   '("31deed4ac5d0b65dc051a1da3611ef52411490b2b6e7c2058c13c7190f7e199b"
     "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d"
     "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223"
     "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
     "f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a"
     "87de2a48139167bfe19e314996ee0a8d081a6d8803954bafda08857684109b4e"
     "a04676d7b664d62cf8cd68eaddca902899f98985fff042d8d474a0d51e8c9236"
     "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279"
     "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e"
     default))
 '(devdocs-window-select nil)
 '(display-fill-column-indicator t)
 '(display-line-numbers nil)
 '(display-line-numbers-major-tick 0)
 '(display-line-numbers-minor-tick 0)
 '(dockerfile-use-buildkit t)
 '(ede-project-directories nil)
 '(elpy-disable-backend-error-display nil)
 '(elpy-formatter 'black)
 '(elpy-modules
   '(elpy-module-company
     elpy-module-eldoc
     elpy-module-flymake
     elpy-module-folding
     elpy-module-pyvenv
     elpy-module-highlight-indentation
     elpy-module-yasnippet
     elpy-module-django
     elpy-module-sane-defaults))
 '(enh-ruby-add-encoding-comment-on-save t)
 '(fill-column 120)
 '(global-highlight-parentheses-mode t)
 '(global-prettify-symbols-mode t)
 '(global-tree-sitter-mode t)
 '(highlight-parentheses-colors '("firebrick1" "dark blue" "green4" "DeepSkyBlue2"))
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
 '(lsp-typescript-format-enable nil)
 '(lsp-typescript-tsserver-trace "messages")
 '(major-mode-remap-alist
   '((css-mode . css-ts-mode)
     (js-json-mode . json-ts-mode)
     (go-mode . go-ts-mode)
     (c-mode . c-ts-mode)
     (c++-mode . c++-ts-mode)
     (rust . rust-ts-mode)
     (cmake-mode . cmake-ts-mode)
     (python-mode . python-ts-mode)))
 '(markdown-code-lang-modes
   '(("ocaml" . tuareg-mode)
     ("elisp" . emacs-lisp-mode)
     ("ditaa" . artist-mode)
     ("asymptote" . asy-mode)
     ("dot" . fundamental-mode)
     ("sqlite" . sql-mode)
     ("calc" . fundamental-mode)
     ("C" . c-mode)
     ("cpp" . c++-mode)
     ("C++" . c++-mode)
     ("screen" . shell-script-mode)
     ("shell" . sh-mode)
     ("bash" . sh-mode)
     ("sh" . sh-mode)))
 '(markdown-command "~/.pyenv/shims/markdown_py")
 '(mode-require-final-newline 'visit-save)
 '(org-agenda-files
   '("~/org/org-todo.org"
     "~/org/projects/Springbok/todo.org"
     "~/org/mobelux/todo.org"
     "~/org/personal/todo-weekly.org"
     "~/org/personal/appointments.org"
     "~/org/projects/VentureRichmond/todo.org"
     "~/org/projects/Montpelier/todo.org"
     "~/org/projects/Livio/sdl-web/todo.org"
     "~/org/personal/todo-house.org"
     "~/org/personal/todo-emacs.org"
     "~/org/personal/todo-daily.org"
     "~/org/personal/todo-main.org"))
 '(org-modules '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-w3m org-mac-iCal))
 '(org-todo-keywords '((sequence "TODO" "IN PROGRESS" "ON HOLD" "DONE(!)")))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(all-the-icons
     bbdb
     casual-info
     coffee-mode
     company
     company-terraform
     company-web
     dashboard
     dashboard-hackernews
     devdocs
     dockerfile-mode
     dogears
     eldoc
     eldoc-box
     elisp-autofmt
     elpy
     emacsql
     emacsql-pg
     emmet-mode
     enh-ruby-mode
     erblint
     fira-code-mode
     flycheck
     flymake-ruby
     form-feed-st
     graphql
     graphql-ts-mode
     highlight-parentheses
     inf-ruby
     ivy
     lsp-mode
     lsp-origami
     magit
     magit-todos
     ng2-mode
     nodejs-repl
     org
     org-beautify-theme
     org-chef
     org-contacts
     org-elisp-help
     org-gcal
     org-mac-link
     org-recur
     org-shoplist
     org-special-block-extras
     org-superstar
     origami
     osx-lib
     page-break-lines
     poly-erb
     poly-ruby
     prettier
     projectile
     projectile-speedbar
     robe
     smart-mode-line
     smart-mode-line-powerline-theme
     sql-indent
     terraform-doc
     terraform-mode
     tree-sitter
     weyland-yutani-theme
     whitespace-cleanup-mode
     xkcd))
 '(prettify-symbols-unprettify-at-point 'right-edge)
 '(projectile-globally-ignored-directories
   '("^\\.idea$"
     "^\\.vscode$"
     "^\\.ensime_cache$"
     "^\\.eunit$"
     "^\\.git$"
     "^\\.hg$"
     "^\\.fslckout$"
     "^_FOSSIL_$"
     "^\\.bzr$"
     "^_darcs$"
     "^\\.pijul$"
     "^\\.tox$"
     "^\\.svn$"
     "^\\.stack-work$"
     "^\\.ccls-cache$"
     "^\\.cache$"
     "^\\.clangd$"
     "^\\.sl$"
     "^\\.jj$"
     "*^node-modules$"
     "*^\\.venv$"))
 '(projectile-mode t nil (projectile))
 '(projectile-project-search-path '("~/Devel" "~/org"))
 '(reddigg-subs '(emacs rust))
 '(safe-local-variable-directories '("~/org/projects/Springbok/"))
 '(safe-local-variable-values
   '((hcl-indent-level . 2)
     (lsp-typescript-sdk . "/Users/adrianflanagan/Devel/mobelux/MMS/gatsby-source-mms/node_modules/typescript/bin")
     (org-todo-keywords quote ((sequence "TODO" "IN PROGRESS" "DEFERRED" "CLIENT" "|" "DONE" "CANCELED")))
     (org-todo-keywords
      quote ((sequence "TODO" "IN PROGRESS" "DEFERRED" "ON HOLD" "NEEDS INPUT" "|" "DONE" "CANCELED")))
     (org-todo-keywords quote ((sequence "TODO" "IN PROGRESS" "DEFFERED" "|" "DONE" "CANCELED")))
     (checkdoc-package-keywords-flag)
     (eglot-inlay-hints-mode)
     (org-todo-keywords quote ((sequence "TODO" "IN PROGRESS" "DEFERRED" "|" "DONE" "CANCELED")))
     (lexical-binding . true)))
 '(show-trailing-whitespace nil)
 '(sml/theme 'dark)
 '(sql-product 'postgres)
 '(track-eol t)
 '(tree-sitter-major-mode-language-alist
   '((actionscript-mode . actionscript)
     (ada-mode . ada)
     (agda-mode . agda)
     (agda2-mode . agda)
     (arduino-mode . arduino)
     (astro-mode . astro)
     (fish-mode . fish)
     (asm-mode . asm)
     (fasm-mode . asm)
     (masm-mode . asm)
     (nasm-mode . asm)
     (gas-mode . asm)
     (sh-mode . bash)
     (beancount-mode . beancount)
     (bibtex-mode . bibtex)
     (c-mode . c)
     (caml-mode . ocaml)
     (clojure-mode . clojure)
     (lisp-mode . commonlisp)
     (lisp-interaction-mode . commonlisp)
     (csharp-mode . c-sharp)
     (c++-mode . cpp)
     (cmake-mode . cmake)
     (d-mode . d)
     (dart-mode . dart)
     (dockerfile-mode . dockerfile)
     (css-mode . css)
     (csv-mode . csv)
     (elm-mode . elm)
     (elixir-mode . elixir)
     (emacs-lisp-mode . elisp)
     (erlang-mode . erlang)
     (ess-r-mode . r)
     (fennel-mode . fennel)
     (f90-mode . fortran)
     (fortran-mode . fortran)
     (gdscript-mode . gdscript)
     (git-commit-mode . gitcommit)
     (git-rebase-mode . git-rebase)
     (gitattributes-mode . gitattributes)
     (gitignore-mode . gitignore)
     (gleam-mode . gleam)
     (glsl-mode . glsl)
     (go-mode . go)
     (groovy-mode . groovy)
     (jenkinsfile-mode . groovy)
     (haskell-mode . haskell)
     (hcl-mode . hcl)
     (terraform-mode . hcl)
     (heex-mode . heex)
     (hlsl-mode . hlsl)
     (html-mode . html)
     (markdown-mode . markdown)
     (mhtml-mode . html)
     (nix-mode . nix)
     (jai-mode . jai)
     (java-mode . java)
     (javascript-mode . javascript)
     (js-mode . javascript)
     (js2-mode . javascript)
     (js3-mode . javascript)
     (json-mode . json)
     (jsonc-mode . json)
     (jsonnet-mode . jsonnet)
     (julia-mode . julia)
     (kotlin-mode . kotlin)
     (latex-mode . latex)
     (llvm-mode . llvm)
     (llvm-mir-mode . llvm-mir)
     (lua-mode . lua)
     (magik-mode . magik)
     (makefile-mode . make)
     (makefile-automake-mode . make)
     (makefile-gmake-mode . make)
     (makefile-makepp-mode . make)
     (makefile-bsdmake-mode . make)
     (makefile-imake-mode . make)
     (matlab-mode . matlab)
     (mermaid-mode . mermaid)
     (meson-mode . meson)
     (typescript-ts-mode . typescript)
     (ninja-mode . ninja)
     (noir-mode . noir)
     (ocaml-mode . ocaml)
     (org-mode . org)
     (pascal-mode . pascal)
     (perl-mode . perl)
     (php-mode . php)
     (qss-mode . css)
     (prisma-mode . prisma)
     (python-mode . python)
     (pygn-mode . pgn)
     (racket-mode . racket)
     (rjsx-mode . javascript)
     (rst-mode . rst)
     (ruby-mode . ruby)
     (rust-mode . rust)
     (rustic-mode . rust)
     (scala-mode . scala)
     (scheme-mode . scheme)
     (solidity-mode . solidity)
     (smithy-mode . smithy)
     (sql-mode . sql)
     (swift-mode . swift)
     (tablegen-mode . tablegen)
     (toml-mode . toml)
     (conf-toml-mode . toml)
     (tcl-mode . tcl)
     (tuareg-mode . ocaml)
     (twig-mode . twig)
     (tsx-ts-mode . tsx)
     (typst-mode . typst)
     (verilog-mode . verilog)
     (vhdl-mode . vhdl)
     (nxml-mode . xml)
     (yaml-mode . yaml)
     (k8s-mode . yaml)
     (zig-mode . zig)))
 '(typescript-enabled-frameworks '(typescript))
 '(typescript-indent-level 2)
 '(undo-limit 524288)
 '(undo-no-redo t)
 '(undo-outer-limit 262144)
 '(undo-strong-limit 655360)
 '(warning-minimum-level :error)
 '(whitespace-style
   '(face
     trailing
     tabs
     spaces
     lines
     newline
     missing-newline-at-eof
     empty
     indentation
     space-after-tab
     space-before-tab
     space-mark
     tab-mark
     newline-mark)))
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
