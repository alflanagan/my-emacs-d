;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(amx-backend 'ido)
 '(completion-auto-select 'second-tab)
 '(create-lockfiles nil)
 '(cua-enable-cua-keys nil)
 '(custom-safe-themes
   '("b0cedf3c6d8fbbf65934e2045dddacff0a031992f2f389215adcb0ca741347c3"
     "95cda51cb6a3fdf667a7710cf85cd67726440e556b91a316ebc5197f077903bb"
     "967c23e9ba179b80560774419f081df22e7674aac23c5c550b817e4a1ce7d058"
     "cbe7f2b12e2739b720225769cdc3a69dfb8a31544d5f86960a3fbdae4c58c0b8"
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
 '(display-fill-column-indicator t)
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
 '(initial-buffer-choice t)
 '(kill-read-only-ok t)
 '(kill-ring-max 256)
 '(kill-whole-line nil)
 '(markdown-ts-inline-images t)
 '(mode-require-final-newline 'visit-save)
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus ol-info ol-irc ol-mhe
             ol-rmail ol-w3m))
 '(package-selected-packages
   '(amx batppuccin cmake-ide cmake-mode cmake-project elisp-autofmt elisp-lint
         flycheck-pos-tip htmlize ido-completing-read+ lsp-treemacs
         org-auto-tangle org-autoexport org-modern ox-gfm prettier
         rainbow-delimiters shfmt terraform-mode treesit-auto treesit-fold
         uv-mode web-mode whitespace-cleanup-mode winpulse xkcd))
 '(safe-local-variable-values
   '((dockerfile-image-name . "backend") (web-mode-indent-style . 2)
     (web-mode-block-padding . 2) (web-mode-script-padding . 2)
     (web-mode-style-padding . 2)
     (eval add-hook 'after-save-hook #'org-babel-tangle nil t)
     (org-todo-keywords quote
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
