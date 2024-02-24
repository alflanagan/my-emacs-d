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
(setq my-paradox-github-token "") ;; so it has a value. should get real value from secrets.el.
(load "./secrets")

;; this is set in custom.el -- but we haven't loaded it yet
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(use-package
 paradox
 :ensure t
 :config
 (progn
   (paradox-enable)
   (setq
    paradox-column-width-package 28
    paradox-column-width-version 14
    paradox-execute-asynchronously t
    paradox-github-token my-paradox-github-token
    paradox-display-download-count t
    paradox-lines-per-entry 2)))


;; TODO: set up and maintain blacklist of packages that look useful, but don't work for me
;; (use-package go :ensure t) ;; the game, not the language -- causes crash in ivy?
;; so very out of date...
;; (use-package angular-mode :ensure t)
;; (use-package ng2-mode :ensure t)
;; this is a) the wrong kind of go (the game, not the language)
;; and b) crashes ivy in spectacular fashion. DO NOT WANT.
;; (use-package go :ensure t)
;; currently, djangonaut commands are failing
;; (use-package djangonaut :ensure t) ;; TODO: link to my rework of the damn package


;;; Use Packages

;; KEEP THIS SORTED!
;; except for this, it kind of needs to be first
(use-package
 better-defaults
 :ensure t
 :config
 (progn
   ;; visible-bell is very nice on Linux and very obnoxious on a Mac
   (setq visible-bell (not (equal system-type 'darwin)))

   ;; better-defaults set custom-file to custom.el
   (load custom-file)))

(use-package async :ensure t)
(use-package auto-header :ensure t)
(use-package blacken :ensure t)
(use-package cmake-mode :ensure t)
(use-package counsel :ensure t)
(use-package counsel-projectile :ensure t)
(use-package devdocs :ensure t)
(use-package django-snippets :ensure t)
(use-package docker-compose-mode :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package dumb-jump :ensure t :config (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))
(use-package editorconfig :ensure t :config (editorconfig-mode 1))
(use-package eldoc :ensure t)
(use-package
 elisp-autofmt
 :ensure t
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook (emacs-lisp-mode . elisp-autofmt-mode)
 :bind (:map emacs-lisp-mode-map (("C-c f" . elisp-autofmt-buffer))))
(use-package elisp-def :ensure t)
(use-package elisp-lint :ensure t)
(use-package elisp-refs :ensure t)
(use-package eslint-disable-rule :ensure t)
(use-package eslint-fix :ensure t)
(use-package django-snippets :ensure t)
(use-package flycheck :ensure t)
(use-package flycheck-aspell :ensure t)
(use-package flycheck-bashate :ensure t)
(use-package flycheck-cask :ensure t)
(use-package flycheck-clang-tidy :ensure t)
(use-package flycheck-eglot :ensure t)
(use-package flycheck-golangci-lint :ensure t)
(use-package flycheck-jest :ensure t)
(use-package flycheck-kotlin :ensure t)
(use-package flycheck-mypy :ensure t)
(use-package flycheck-package :ensure t)
(use-package flycheck-pycheckers :ensure t)
(use-package flycheck-relint :ensure t)
(use-package flycheck-rust :ensure t)
(use-package flylisp :ensure t)
(use-package focus-autosave-mode :ensure t)
(use-package fold-dwim :ensure t)
(use-package form-feed-st :ensure t :config (add-hook 'emacs-lisp-mode-hook 'form-feed-st-mode))
(use-package forth-mode :ensure t)
(use-package git-modes :ensure t)
(use-package go-autocomplete :ensure t)
(use-package go-eldoc :ensure t)
(use-package go-mode :ensure t)
(use-package go-projectile :ensure t)
(use-package go-scratch :ensure t)
(use-package guru-mode :ensure t)
(use-package highlight-parentheses :ensure t)

(use-package ibuffer :ensure t :bind (("C-x C-b" . ibuffer-list-buffers)))
(use-package ivy :ensure t :config (ivy-mode 1))
(use-package lispy :ensure t)
;; bind can happen even if the package install fails??
(use-package mwim :ensure t :bind (("C-a" . mwim-beginning) ("C-e" . mwim-end)))
(use-package paradox :ensure t :config (paradox-enable))
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

(use-package django-snippets :ensure t)

(use-package
 immaterial-theme
 :ensure t
 :config
 (progn
   (load-theme 'immaterial-dark t)
   (load-theme 'immaterial-light t)))
(use-package ivy :ensure t :config (ivy-mode 1))
(use-package morlock :ensure t :config (global-morlock-mode 1)) ;; additional syntax highlighting for ELisp
;; bind can happen even if the package install fails??
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
(use-package yasnippet :ensure t :pin melpa)

;;; Everything Else

;; this file is read by the emacs system service, so we need to start the server
(if (not (boundp 'server-process))
(server-start))

;; because I often hit this key by accident and use "C-x C-c" instead anyway
(keymap-global-unset "s-q" nil)
(keymap-global-set "C-x C-p" #'(project-list-buffers t))

;; to work with emacsclient, commands that affect the frame need to be in server-after-make-frame-hook
(defun setup-frame-for-mac ()
  "sets up graphical elements for new frames specific to macs"
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (when (member "Fira Code" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Fira Code-16"))
    (add-to-list 'default-frame-alist '(font . "Fira Code-16"))))

(when (equal system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (add-hook 'server-after-make-frame-hook #'setup-frame-for-mac)
  (set-fontset-font t 'symbol (font-spec :family "Apple Symbols") nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
  ;; really this is specific to my macbook only
  (setq elisp-autofmt-python-bin "/Users/adrianflanagan/.pyenv/versions/3.12.1/bin/python")
  (setq visible-bell nil)
  (setq ring-bell-function
        (lambda ()
          "do nothing and do it well"
          ())))

(defun set-def-frame-size ()
  (set-frame-size nil 180 60))
(add-hook 'server-after-make-frame-hook #'set-def-frame-size)
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


;; TypeScript setup

(defun setup-tide-mode ()
  "Set up `tide-mode', an IDE for typescript.

Should only be run in a directory or project with a tsconfig file."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package
 tide
 :ensure t
 :config
 (progn
   ;; aligns annotation to the right hand side
   (setq company-tooltip-align-annotations t)

   ;; formats the buffer before saving, only if tide-mode is active
   (add-hook 'before-save-hook 'tide-format-before-save)

   (add-hook 'typescript-ts-mode-hook #'setup-tide-mode)
   (add-hook 'tsx-ts-mode-hook #'setup-tide-mode)
   (add-hook 'angular-html-mode-hook #'setup-tide-mode)))


;; Org-mode setup

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;;; init.el ends here :-)
