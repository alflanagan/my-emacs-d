;; -*- lexical-binding: t; indent-tabs-mode: nil; -*-

;;; init.el -- Non-site-specific initialization
;; Copyright © 2024, A. Lloyd Flanagan

;; Author: A. Lloyd Flanagan <a.lloyd.flanagan@gmail.com>
;; Maintainer: A. Lloyd Flanagan <a.lloyd.flanagan@gmail.com>
;; Created: 2014

;; This file is not part of GNU Emacs.


;;; custom lisp directory

(let ((lispdir (expand-file-name "~/.emacs.d/lisp")))
  (unless (member lispdir load-path)
    (push lispdir load-path)))

;;; NOTE: customizations are in custom.el -- better-defaults changes the destination 

;;; Packages
;;; Eventual goal is to remove from customization entirely, and use use-package for all.

(package-initialize)
(paradox-enable)

;; KEEP THIS SORTED!
; except for this, it kind of needs to be first
(use-package
 better-defaults
 :ensure t
 :config
 (progn
   ;; the one "better" default I don't like
   (setq visible-bell nil)

   ;; better-defaults set custom-file to custom.el
   (load custom-file)))

;; (use-package async :ensure t)
;; (use-package auto-header :ensure t)
;; (use-package blacken :ensure t)
;; (use-package cmake-mode :ensure t)
;; (use-package counsel :ensure t)
;; (use-package counsel-projectile :ensure t)
;; (use-package devdocs :ensure t)
;; (use-package docker-compose-mode :ensure t)
;; (use-package dockerfile-mode :ensure t)
;; (use-package dumb-jump :ensure t :config (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))
;; (use-package editorconfig :ensure t :config (editorconfig-mode 1))
;; (use-package editorconfig-generate :ensure t)
;; (use-package eldoc :ensure t)
;; (use-package elisp-autofmt :ensure t :bind (:map emacs-lisp-mode-map (("C-c f" . elisp-autofmt-buffer))))
;; (use-package elisp-def :ensure t)
;; (use-package elisp-lint :ensure t)
;; (use-package elisp-refs :ensure t)
;; (use-package eslint-disable-rule :ensure t)
;; (use-package eslint-fix :ensure t)
;; (use-package flycheck :ensure t)
;; (use-package flycheck-aspell :ensure t)
;; (use-package flycheck-bashate :ensure t)
;; (use-package flycheck-cask :ensure t)
;; (use-package flycheck-clang-tidy :ensure t)
;; (use-package flycheck-eglot :ensure t)
;; (use-package flycheck-golangci-lint :ensure t)
;; (use-package flycheck-jest :ensure t)
;; (use-package flycheck-kotlin :ensure t)
;; (use-package flycheck-mypy :ensure t)
;; (use-package flycheck-package :ensure t)
;; (use-package flycheck-pycheckers :ensure t)
;; (use-package flycheck-relint :ensure t)
;; (use-package flycheck-rust :ensure t)
;; (use-package flylisp :ensure t)
;; (use-package focus-autosave-mode :ensure t)
;; (use-package fold-dwim :ensure t)
;; (use-package form-feed-st :ensure t :config (add-hook 'emacs-lisp-mode-hook 'form-feed-st-mode))
;; (use-package forth-mode :ensure t)
;; (use-package git-modes :ensure t)
;; (use-package gnu-elpa-keyring-update :ensure t)
;; (use-package go :ensure t)
;; (use-package go-autocomplete :ensure t)
;; (use-package go-eldoc :ensure t)
;; (use-package go-mode :ensure t)
;; (use-package go-projectile :ensure t)
;; (use-package go-scratch :ensure t)
;; (use-package guru-mode :ensure t)
;; (use-package highlight-parentheses :ensure t)

(use-package ibuffer :ensure t :bind (("C-x C-b" . ibuffer-list-buffers)))
(use-package ivy :ensure t :config (ivy-mode 1))
(use-package mwim :ensure t :bind (("C-a" . mwim-beginning) ("C-e" . mwim-end)))

(use-package
 projectile
 :ensure t
 :config (projectile-mode +1)
 :bind (:map projectile-mode-map ("s-p" . projectile-command-map)))

(use-package
 smart-mode-line
 :ensure t
 :config
 (progn
   (sml/setup)
   (sml/apply-theme 'powerline)))

(use-package
 immaterial-theme
 :ensure t
 :config
 (progn
   (load-theme 'immaterial-dark t)
   (load-theme 'immaterial-light t)))


;;; Everything Else

(unless (default-value 'server-mode)
  (server-start t))

;; because I often hit this key by accident and use "C-x C-c" instead anyway
(keymap-global-unset "s-q" nil)
(keymap-global-set "C-x C-p" #'(project-list-buffers t))

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
;; enable these for all environments so I don't have to remember on non-Macs
(bind-keys ("C-c C-q" . indent-pp-sexp) ("C-c C-s" . kill-sexp) ("C-%" . query-replace))

;; steve yegge's replacements for using Meta
(bind-keys ("C-x C-m" . execute-extended-command) ("C-c C-m" . execute-extended-command))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (setq-local compile-command
               (concat
                "emacs -batch -f batch-byte-compile "
                (if buffer-file-name
                    (shell-quote-argument buffer-file-name))))))

;;; init.el ends here :-)
