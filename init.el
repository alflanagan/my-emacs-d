;;;  -*- lexical-binding: t; indent-tabs-mode: nil -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(display-fill-column-indicator t)
 '(fill-column 120)
 '(global-whitespace-mode nil)
 '(global-whitespace-newline-mode nil)
 '(initial-buffer-choice t)
 '(package-selected-packages
   '(cmake-mode elisp-def elisp-lint elisp-refs paradox lispy ## async auto-header dockerfile-mode editorconfig eldoc fold-dwim forth-mode git-modes gnu-elpa-keyring-update go-mode guru-mode highlight-parentheses js2-mode kotlin-ts-mode markdown-mode markdown-toc org parrot rust-mode slime smart-mode-line smart-mode-line-powerline-theme tree-sitter tree-sitter-indent tree-sitter-langs ws-butler yaml-mode))
 '(paradox-github-token t)
 '(whitespace-action '(auto-cleanup))
 '(whitespace-global-modes nil)
 '(whitespace-line-column nil)
 '(whitespace-style
   '(face trailing tabs spaces lines newline missing-newline-at-eof empty indentation space-after-tab space-before-tab space-mark tab-mark newline-mark)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "White" :foreground "Black" :inverse-video nil
                :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular
                :height 120 :width normal :foundry "nil" :family "\12Fira Code Retina")))))


(let ((lispdir (expand-file-name "~/.emacs.d/lisp")))
  (unless (member lispdir load-path)
    (push lispdir load-path)))

(use-package cmake-mode)

(use-package package
  :init (progn
          (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
  :config (progn
            (package-initialize)
            (paradox-enable)))

(require 'parrot)
(sml/setup)
(sml/apply-theme 'powerline)

(keymap-global-set "C-x C-b" 'ibuffer-list-buffers)
;; because I often hit this key by accident and use "C-x C-c" instead anyway
(keymap-global-unset "s-q" nil)
(keymap-global-set "C-x C-p" '(project-list-buffers t))


(require 'highlight-parentheses)

(require 'server) ;; load early so we can start (??)
(unless (server-running-p)
  (server-start))

(when (equal system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (when (member "Fira Code" (font-family-list))
  (add-to-list 'initial-frame-alist '(font . "Fira Code-14"))
  (add-to-list 'default-frame-alist '(font . "Fira Code-14")))
  (set-fontset-font t 'symbol (font-spec :family "Apple Symbols") nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

(set-frame-size nil 120 45)
