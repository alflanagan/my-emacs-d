;;; init.el -- Non-site-specific initialization -*- lexical-binding: t; -*-

;; Copyright © 2024 A. Lloyd Flanagan
;;
;; Author: A. Lloyd Flanagan <lloyd.flanagan@proton.me>
;; Maintainer: A. Lloyd Flanagan <lloyd.flanagan@proton.me>
;; Created: 2014
;; Version: ??

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; (require 'hideshowvis "hideshowvis")

;;; Code:

;;; custom lisp directory
(let ((default-directory (directory-file-name (concat (expand-file-name user-emacs-directory) "lisp"))))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

(require 'alf-alists "alists")
(load "./secrets")

;; various settings gleaned from package better-defaults

(ido-mode t)
(setq ido-enable-flex-matching t)

;; these nodes are discouraged because they use mouse -- keep hands on the keyboard!
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(require 'uniquify)

(save-place-mode t)
(show-paren-mode 1)
(global-auto-revert-mode 1)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(setq-default indent-tabs-mode nil)
(setq
 apropos-do-all t
 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
 column-number-mode t
 custom-file (concat user-emacs-directory "custom.el")
 default-frame-alist '((horizontal-scroll-bars) (vertical-scroll-bars) (width . 180) (height . 70)) ;; see also early-init.el
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 save-place-file (concat user-emacs-directory "places")
 select-enable-clipboard t
 select-enable-primary t
 sentence-end-double-space nil
 uniquify-buffer-name-style 'post-forward-angle-brackets)

(load custom-file)

;;; NOTE: customizations are in custom.el

;;; Packages
;;; Eventual goal is to remove from customization entirely, and use use-package for all.
;; ... except in practice it's easier to add from list-packages and thus it sets variable
;; package-select-packages anyway -- should we advise use-package to do that also??
;; heck, why not rewrite list-packages so insert adds a use-package declaration to a
;; section of this file?

;; first we add melpa archive (use with caution!)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; set up use-package
;; so I don't have to specify :ensure t on every call
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(package-initialize)

;; nifty interface for execute-extended-command (M-x)
(require 'smex "smex/smex")
;; (smex-initialize) ;; not required, might make first use faster


;; Blacklisted Packages
;; this is reminder (to myself) not to use these packages, and why

;; (use-package go) ;; the game, not the language -- causes crash in ivy?
;; (use-package angular-mode) ;; so very out of date...
;; (use-package djangonaut) ;; currently, djangonaut commands are failing
;; (use-package paradox)  problems like putting all its output into minibuffer and freezing emacs :-(
;; (use-package fold-dwim :bind (("C-+" . fold-dwim-toggle)("C-=" . fold-dwim-toggle)))
;; I have no idea what fold-dwim's problem is, but it almost never Does What I Mean.
;; eglot -- built-in, fine as far as I know, superceded by lsp-mode

;;; Use Packages

;; KEEP THIS SORTED!

;; (use-package async)
;; (use-package auto-header)
;; (use-package auto-rename-tag)
(use-package bbdb)
;; (use-package blacken)
;; (use-package cargo-mode :pin "melpa" :hook 'rust-mode-hook)
;; (use-package cmake-mode)
;; (use-package code-archive)
;; don't load company until a source file has loaded (check: startup load of org file doesn't load it)
(use-package company :after prog-mode :config (add-hook 'prog-mode-hook 'company-mode))

;; (use-package company-jedi)
;; (use-package company-math)
;; (use-package company-shell)
;; (use-package company-terraform)
(use-package company-web)
;; (use-package counsel)
;; (use-package counsel-projectile)
;; (use-package css-eldoc)
(use-package devdocs)
;; (use-package django-snippets)
;; (use-package docker-compose-mode)
(use-package dockerfile-mode)
(use-package
 dogears
 :config (dogears-mode 1)
 :bind
 (:map
  global-map
  ("M-g d" . dogears-go)
  ("M-g M-b" . dogears-back)
  ("M-g M-f" . dogears-forward)
  ("M-g M-d" . dogears-list)
  ("M-g M-D" . dogears-sidebar)))
;; (use-package dumb-jump :config (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))
;; (use-package editorconfig :config (editorconfig-mode 1))
(use-package eldoc :defer t)
(use-package eldoc-box :defer t :after eldoc)
(use-package
 elisp-autofmt
 :defer t
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook ((emacs-lisp-mode . elisp-autofmt-mode) (lisp-data-mode . elisp-autofmt-mode))
 :bind (:map lisp-mode-shared-map (("C-c f" . elisp-autofmt-buffer))))
;; (use-package elisp-def)
;; (use-package elisp-lint)
;; (use-package elisp-refs)
(use-package elpy :defer t :init (advice-add 'python-mode :before 'elpy-enable))
;; (use-package eslint-disable-rule)
;; (use-package eslint-fix)
(use-package emmet-mode :hook ((html-mode . emmet-mode)))
;; (use-package fira-code-mode
;;   ;; == and === come out as set operators on linux?
;;   :custom (fira-code-mode-disabled-ligatures '("[]" "===" "==" ":" ))  ; ligatures you don't want
;;   :hook prog-mode)                                         ; mode to enable fira-code-mode in


;; Flycheck
(use-package flycheck :config (add-hook 'after-init-hook #'global-flycheck-mode) :pin nongnu)
;; (use-package flycheck-aspell)
;; (use-package flycheck-bashate)
;; (use-package flycheck-cask)
;; (use-package flycheck-clang-tidy)
;; (use-package flycheck-golangci-lint)
;; (use-package flycheck-jest)
;; (use-package flycheck-kotlin)
;; (use-package flycheck-mypy)
;; (use-package flycheck-package)
;; (use-package flycheck-pycheckers)
;; (use-package flycheck-relint)
;; (use-package flycheck-rust)
;; (use-package flylisp)

;; (use-package focus-autosave-mode)
(use-package form-feed-st :config (add-hook 'emacs-lisp-mode-hook 'form-feed-st-mode))
;; (use-package forth-mode)
;; (use-package git-modes)
;; (use-package gnu-elpa-keyring-update)
;; (use-package go-autocomplete)
;; (use-package go-eldoc)
;; (use-package go-mode)
;; (use-package go-projectile)
;; (use-package go-scratch)
;; (use-package guru-mode)
(use-package highlight-parentheses)
;; (use-package hl-todo)
;; (use-package ibuffer-projectile)
;; (use-package ietf-docs)
;; (use-package
;;  immaterial-theme
;;  :config
;;  (progn
;;    (load-theme 'immaterial-dark t)
;;    (load-theme 'immaterial-light t)))
(use-package ivy :config (ivy-mode 1))
;; (use-package kotlin-ts-mode)
;; (use-package lispy)

(use-package lsp-mode :defer t :commands lsp)
(use-package lsp-origami :defer t :config (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable))
(use-package magit)
(use-package magit-todos)
;; (use-package markdown-toc)
;; (use-package morlock :config (global-morlock-mode 1)) ;; additional syntax highlighting for ELisp
;; (use-package nov) ;; epub reader


;; org-mode packages
(use-package
 org
 :defer t
 :pin gnu
 :bind (("C-c l" . org-store-link) ("C-c a" . org-agenda) ("C-c c" . org-capture))
 :config
 (progn
   (setq org-adapt-indentation 'headline-data)
   (setq org-ctrl-k-protect-subtree t)
   (setq org-special-ctrl-a/e t)))


(use-package
 org-contacts
 :defer t
 :config
 (push org-capture-templates
       '("c" "Contacts" entry (file "~/org/contacts.org")
         "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:PHONE:
:ALIAS:
:NICKNAME:
:IGNORE:
:ICON:
:NOTE:
:ADDRESS:
:BIRTHDAY:
:END:")))

(use-package org-chef)
(use-package org-elisp-help)

;; (use-package org-contrib)
(use-package org-modern :after org :config (global-org-modern-mode +1))
;; (use-package org-ai)
;; (use-package org-msg)
;; (use-package org-ql)
;; (use-package org-recur)
;; http://alhassy.com/org-special-block-extras/ -- define your own Org blocks
(use-package org-special-block-extras)
;; (use-package org-web-tools)

(use-package
 origami
 :defer t
 :config (global-origami-mode)
 :bind (("C-+" . origami-forward-toggle-node) ("C-=" . origami-forward-toggle-node)))

;; (use-package parrot)

;; intriguing, but doesn't seem to be working correctly (may just need more config)
;; https://github.com/kcyarn/pretty-speedbar
;; (use-package pretty-speedbar :defer t :after projectile-speedbar :config
;;   (setq pretty-speedbar-font "Font Awesome 6 Free Solid"))

;; attempt to set up equivalent keys on Mac and my PC.
(if (equal system-type 'darwin)
    (use-package
     projectile
     :init (keymap-global-unset "s-p")
     :config (projectile-mode +1)
     :bind (:map projectile-mode-map ("s-p" . projectile-command-map)))
  (use-package
   projectile
   :init (keymap-global-unset "M-p")
   :config (projectile-mode +1)
   :bind (:map projectile-mode-map ("M-p" . projectile-command-map))))

(use-package projectile-speedbar :after projectile :defer t)
;; (use-package projectile-codesearch)
;; (use-package rainbow-delimiters)
;; (use-package reddigg)
;; (use-package rust-mode :pin "melpa" :config (add-hook 'rust-mode-hook #'cargo-minor-mode))
;; (use-package slime)
(use-package smart-mode-line :config (sml/setup))
(use-package smart-mode-line-powerline-theme :config (sml/apply-theme 'light-powerline))
(use-package sql-indent :defer t)

;; (use-package super-save)
;; (use-package term-projectile)
(use-package tree-sitter)
;; (use-package tree-sitter-indent)

(defun mp-setup-install-grammars ()
  "Install Tree-sitter grammars if they are absent."
  (interactive)
  ;; it's not clear there's much advantage to specifying the version here
  (dolist (grammar
           '((bash "https://github.com/tree-sitter/tree-sitter-bash")
             (cmake "https://github.com/uyha/tree-sitter-cmake")
             (css . ("https://github.com/tree-sitter/tree-sitter-css"))
             (elisp "https://github.com/Wilfred/tree-sitter-elisp")
             (go "https://github.com/tree-sitter/tree-sitter-go")
             (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
             (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
             (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
             (make "https://github.com/alemuller/tree-sitter-make")
             (markdown "https://github.com/ikatyang/tree-sitter-markdown")
             (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
             (rust "https://github.com/tree-sitter/tree-sitter-rust")
             (toml "https://github.com/tree-sitter/tree-sitter-toml")
             (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
             (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
             (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

    (add-to-list 'treesit-language-source-alist grammar)
    ;; Only install `grammar' if we don't already have it
    ;; installed. However, if you want to *update* a grammar then
    ;; this obviously prevents that from happening.
    (unless (treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar)))))

;; (use-package treesit-auto)


;; TypeScript setup

;; tree-sitter nodes don't have load event?
;; (with-eval-after-load 'typescript-ts-mode
;;  "sets up typescript-ts-mode-hook with more goodies"
(use-package
 typescript-ts-mode
 :defer t
 :hook
 ((typescript-ts-mode . lsp-deferred)
  (typescript-ts-mode . display-line-numbers-mode)
  (typescript-ts-mode . (lambda () (setq flycheck-check-syntax-automatically '(save mode-enabled))))
  (typescript-ts-mode . flycheck-mode)
  (typescript-ts-mode . eldoc-mode)
  (typescript-ts-mode . eldoc-box-hover-mode)
  (tsx-ts-mode . lsp-deferred)
  (typescript-ts-mode . company-mode)))


;; (use-package w3m)
;; (use-package web-beautify)
;; (use-package web-mode)
;; (use-package weyland-yutani-theme)

(use-package whitespace-cleanup-mode)
(use-package xkcd :defer t)

;; (use-package yasnippet :pin melpa)

;; ;; should have a separate section for Elisp libraries
;; (use-package dash) ;; list functions
;; (use-package s) ;; string functions


;;; Everything Else

;; visible-bell is very nice on Linux and very obnoxious on a Mac
(setq visible-bell (not (equal system-type 'darwin)))
(setq
 fill-column 120
 indent-tabs-mode nil)
;; surely there's a better function for this?
;; and alist-key-add-or-replace doesn't work properly
;; these aren't defined in auto-mode-alist and this works
(push '("\\.ts\\'" . typescript-ts-mode) auto-mode-alist)
(push '("\\.tsx\\'" . tsx-ts-mode) auto-mode-alist)
(push '("\\.js[mx]?\\'" . js-ts-mode) auto-mode-alist)
(push '("\\.har\\'" . js-ts-mode) auto-mode-alist)

;; not having a lot of luck setting up emacs as a brew service, so far
(if (or (not (boundp 'server-process)) (null server-process))
    (server-start))

;; because I often hit this key by accident and use "C-x C-c" instead anyway
(keymap-global-unset "s-q" nil)
;; (keymap-global-set "C-x C-p" (lambda (x) (interactive) (project-list-buffers t)))


;; MAC-specific setup

(when (equal system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (set-fontset-font t 'symbol (font-spec :family "Apple Symbols") nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
  (setq ring-bell-function
        (lambda ()
          "do nothing and do it well"
          ())))


;; Linux-specific setup

;; (when (equal system-type 'gnu/linux))


;; Mac keybindings that conflict with Emacs defaults
;; M-% launches screen capture
;; C-M-k brings up calender in MacOS topbar
;; C-M-q locks the screen
;; enable these for all environments so I don't have to remember on non-Macs
(bind-keys ("C-c C-q" . indent-pp-sexp) ("C-c C-s" . kill-sexp) ("C-%" . query-replace))

(keymap-set global-map "C-x M-r" #'remember)
(keymap-set global-map "C-x M-R" #'remember-region)

;; Tell emacs lisp mode to do the right thing on build.
(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (setq-local compile-command
               (concat
                "emacs -batch -f batch-byte-compile "
                (if buffer-file-name
                    (shell-quote-argument buffer-file-name))))))


;;; buffer(s) opened on startup

(find-file (expand-file-name "~/org/personal/todo-main.org"))


;; system locations

;; this may be _too_ clever
;; (setq global-node-executable (s-chomp (shell-command-to-string ". ~/.zshrc 2> /dev/null && nvm which default")))


;; enabled "risky" commands

(put 'downcase-region 'disabled nil)


(message "%s" "init.el completed")

;;; init.el ends here :-)
