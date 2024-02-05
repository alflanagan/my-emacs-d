
;;; -*- lexical-binding: t; indent-tabs-mode: nil; coding:utf-8 -*-
;;; init.el -- Non-site-specific initialization
;; Copyright © 2024, A. Lloyd Flanagan

;; Author: A. Lloyd Flanagan <a.lloyd.flanagan@gmail.com>
;; Maintainer: A. Lloyd Flanagan <a.lloyd.flanagan@gmail.com>
;; Created: 2014

;; This file is not part of GNU Emacs.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(display-fill-column-indicator t)
 '(ede-project-directories nil)
 '(fill-column 120)
 '(global-whitespace-mode nil)
 '(global-whitespace-newline-mode nil)
 '(initial-buffer-choice t)
 '(major-mode-remap-alist
   '((css-mode . css-ts-mode)
     (js-json-mode . json-ts-mode)
     (go-mode . go-ts-mode)
     (c-mode . c-ts-mode)
     (c++-mode . c++-ts-mode)
     (rust . rust-ts-mode)
     (cmake-mode . cmake-ts-mode)
     (markdown-mode . markdown-ts-mode)))
 '(package-selected-packages
   '(mwim go go-autocomplete go-eldoc go-projectile go-scratch ibuffer-projectile projectile projectile-codesearch projectile-speedbar promise pyenv-mode rainbow-delimiters reddigg blacken ipretty ace-jump-mode docker-compose-mode flycheck flycheck-aspell flycheck-bashate flycheck-cask flycheck-clang-tidy flycheck-eglot flycheck-golangci-lint flycheck-jest flycheck-kotlin flycheck-mypy flycheck-package flycheck-pycheckers flycheck-relint flycheck-rust flylisp focus-autosave-mode cmake-mode elisp-def elisp-lint elisp-refs paradox lispy ## async auto-header dockerfile-mode editorconfig eldoc fold-dwim forth-mode git-modes gnu-elpa-keyring-update go-mode guru-mode highlight-parentheses js2-mode kotlin-ts-mode markdown-mode markdown-toc org parrot rust-mode slime smart-mode-line smart-mode-line-powerline-theme tree-sitter tree-sitter-indent tree-sitter-langs ws-butler yaml-mode))
 '(paradox-execute-asynchronously nil)
 '(paradox-github-token t)
 '(projectile-project-search-path
   '("~/.emacs.d"
     ("~/Devel/personal" . 1)
     ("~/Devel/mobelux" . 1)
     "~/Devel/Hackrva" "~/Devel/thirdparty"))
 '(whitespace-action '(auto-cleanup))
 '(whitespace-global-modes nil)
 '(whitespace-line-column nil)
 '(whitespace-style
   '(face trailing tabs spaces lines newline missing-newline-at-eof empty indentation space-after-tab space-before-tab space-mark tab-mark newline-mark)))

;; tree-sitter: to install a language use treesit-install-language-grammar. If the language is part of the tree-sitter
;; project it will fill in the URL for you, just accept the defaults.

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 120 :width normal :foundry "nil" :family "\12Fira Code Retina")))))

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

(use-package ace-jump-mode
  :config (ace-jump-mode-enable-mark-sync)
  )
(keymap-global-set "C-c SPC" 'ace-jump-mode)
(keymap-global-set  "C-x SPC" 'ace-jump-mode-pop-mark)

(use-package parrot)
(sml/setup)
(sml/apply-theme 'powerline)

(keymap-global-set "C-x C-b" 'ibuffer-list-buffers)
;; because I often hit this key by accident and use "C-x C-c" instead anyway
(keymap-global-unset "s-q" nil)
(keymap-global-set "C-x C-p" #'(project-list-buffers t))


(use-package highlight-parentheses)

(use-package server)
(unless (default-value 'server-mode)
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

(set-frame-size nil 130 45)

;; Mac keybindings that conflict with Emacs defaults
;; M-% launches screen capture
;; C-M-k brings up calender in MacOS topbar
;; C-M-q locks the screen
(keymap-global-set "C-%" 'query-replace)
(keymap-global-set "C-c C-s" 'kill-sexp)
(keymap-global-set "C-c C-q" 'indent-pp-sexp)

;; steve yegge's replacements for using Meta
(keymap-global-set "C-x C-m" 'execute-extended-command)
(keymap-global-set "C-c C-m" 'execute-extended-command)

(use-package flylisp)
(use-package flycheck-rust)
(use-package flycheck-relint)
(use-package flycheck-pycheckers)
(use-package flycheck-package)
(use-package flycheck-mypy)
(use-package flycheck-kotlin)
(use-package flycheck-jest)
(use-package flycheck-golangci-lint)
(use-package flycheck-eglot)
(use-package flycheck-clang-tidy)
(use-package flycheck-cask)
(use-package flycheck-bashate)
(use-package flycheck-aspell)
(use-package flycheck)

(use-package projectile
  :ensure t
  :pin melpa
  :init
    (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)))

(use-package mwim
  :init (progn
          (keymap-global-set "C-a" 'mwim-beginning)
          (keymap-global-set "C-e" 'mwim-end)))
;; should we use :bind above? if so, what's the exact syntax?
