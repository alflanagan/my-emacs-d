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

(let ((lispdir (directory-file-name (concat (expand-file-name user-emacs-directory) "lisp"))))
  (unless (member lispdir load-path)
    (push lispdir load-path)))

(let ((ng2dir (directory-file-name (concat (expand-file-name user-emacs-directory) "lisp/ng2-mode"))))
  (push ng2dir load-path))

(require 'alf-alists "alists")
(load "./secrets")
(require 'better-defaults "better-defaults/better-defaults")

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)



(global-auto-revert-mode 1)

;;; NOTE: customizations are in custom.el

;;; Packages
;;; Eventual goal is to remove from customization entirely, and use use-package for all.

;; first we add melpa archive (use with caution!)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; set up use-package

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(package-initialize)


;; TODO: set up and maintain blacklist of packages that look useful, but don't work for me
;; (use-package go :ensure t) ;; the game, not the language -- causes crash in ivy?
;; so very out of date...
;; (use-package angular-mode :ensure t)
;; this is a) the wrong kind of go (the game, not the language)
;; and b) crashes ivy in spectacular fashion. DO NOT WANT.
;; (use-package go :ensure t)
;; currently, djangonaut commands are failing
;; (use-package djangonaut :ensure t) ;; TODO: link to my rework of the damn package
;; paradox is having problems like putting all its output into minibuffer and freezing emacs :-(
;; (use-package fold-dwim :ensure t :bind (("C-+" . fold-dwim-toggle)("C-=" . fold-dwim-toggle)))
;; I have no idea what fold-dwim's problem is, but it almost never Does What I Mean.
;; eglot -- built-in, fine as far as I know, superceded by lsp-mode

;;; Use Packages

;; KEEP THIS SORTED!

(require 'smex "smex/smex")

;; (smex-initialize) ;; not required, might make first use faster

;; (use-package async :ensure t)
;; (use-package auto-header :ensure t)
;; (use-package auto-rename-tag :ensure t)
;; (use-package blacken :ensure t)
;; (use-package cargo-mode :ensure t :pin "melpa" :hook 'rust-mode-hook)
;; (use-package cmake-mode :ensure t)
;; (use-package code-archive :ensure t)
;; (use-package company :ensure t)
;; (use-package company-jedi :ensure t)
;; (use-package company-math :ensure t)
;; (use-package company-shell :ensure t)
;; (use-package company-terraform :ensure t)
;; (use-package company-web :ensure t)
;; (use-package counsel :ensure t)
;; (use-package counsel-projectile :ensure t)
;; (use-package css-eldoc :ensure t)
(use-package devdocs :ensure t)
;; (use-package django-snippets :ensure t)
;; (use-package docker-compose-mode :ensure t)
;; (use-package dockerfile-mode :ensure t)
;; (use-package dumb-jump :ensure t :config (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))
;; (use-package editorconfig :ensure t :config (editorconfig-mode 1))
(use-package eldoc :defer t)
(use-package
 elisp-autofmt
 :defer t
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook ((emacs-lisp-mode . elisp-autofmt-mode) (lisp-data-mode . elisp-autofmt-mode))
 :bind (:map lisp-mode-shared-map (("C-c f" . elisp-autofmt-buffer))))
;; (use-package elisp-def :ensure t)
;; (use-package elisp-lint :ensure t)
;; (use-package elisp-refs :ensure t)
;; (use-package eslint-disable-rule :ensure t)
;; (use-package eslint-fix :ensure t)
;; (use-package emmet-mode :ensure t)
;; 
;; Flycheck
(use-package flycheck :config (add-hook 'after-init-hook #'global-flycheck-mode) :pin nongnu :ensure t)
;; (use-package flycheck-aspell :ensure t)
;; (use-package flycheck-bashate :ensure t)
;; (use-package flycheck-cask :ensure t)
;; (use-package flycheck-clang-tidy :ensure t)
;; (use-package flycheck-golangci-lint :ensure t)
;; (use-package flycheck-jest :ensure t)
;; (use-package flycheck-kotlin :ensure t)
;; (use-package flycheck-mypy :ensure t)
;; (use-package flycheck-package :ensure t)
;; (use-package flycheck-pycheckers :ensure t)
;; (use-package flycheck-relint :ensure t)
;; (use-package flycheck-rust :ensure t)
;; (use-package flylisp :ensure t)
;; 
;; (use-package focus-autosave-mode :ensure t)
(use-package form-feed-st :config (add-hook 'emacs-lisp-mode-hook 'form-feed-st-mode))
;; (use-package forth-mode :ensure t)
;; (use-package git-modes :ensure t)
;; (use-package gnu-elpa-keyring-update :ensure t)
;; (use-package go-autocomplete :ensure t)
;; (use-package go-eldoc :ensure t)
;; (use-package go-mode :ensure t)
;; (use-package go-projectile :ensure t)
;; (use-package go-scratch :ensure t)
;; (use-package guru-mode :ensure t)
(use-package highlight-parentheses)
;; (use-package hl-todo :ensure t)
;; (use-package ibuffer-projectile :ensure t)
;; (use-package ietf-docs :ensure t)
;; (use-package
;;  immaterial-theme
;;  :ensure t
;;  :config
;;  (progn
;;    (load-theme 'immaterial-dark t)
;;    (load-theme 'immaterial-light t)))
;; (use-package ivy :ensure t :config (ivy-mode 1))
;; (use-package kotlin-ts-mode :ensure t)
;; (use-package lispy :ensure t)

(use-package lsp-mode :defer t :ensure t :commands lsp)
;; (use-package magit :ensure t)
;; (use-package markdown-toc :ensure t)
;; (use-package morlock :ensure t :config (global-morlock-mode 1)) ;; additional syntax highlighting for ELisp
;; (use-package nov :ensure t) ;; epub reader


;; ;; org-mode packages
;; (use-package org-contrib :ensure t)
(use-package org-modern :ensure t :defer t)
;; (use-package org-ai :ensure t)
;; (use-package org-msg :ensure t)
;; (use-package org-ql :ensure t)
;; (use-package org-recur :ensure t)
;; (use-package org-special-block-extras :ensure t)
;; (use-package org-tidy :ensure t)
;; (use-package org-web-tools :ensure t)

(use-package
 origami
 :defer t
 :config (global-origami-mode)
 :bind (("C-+" . origami-forward-toggle-node) ("C-=" . origami-forward-toggle-node)))
(use-package lsp-origami :defer t :config (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable))

;; (use-package parrot :ensure t)

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

;; (use-package projectile-codesearch :ensure t)
;; (use-package pyenv-mode :ensure t)
;; (use-package rainbow-delimiters :ensure t)
;; (use-package reddigg :ensure t)
;; (use-package rust-mode :ensure t :pin "melpa" :config (add-hook 'rust-mode-hook #'cargo-minor-mode))
;; (use-package slime :ensure t)
(use-package smart-mode-line :ensure t :config (sml/setup))
(use-package smart-mode-line-powerline-theme :ensure t :config (sml/apply-theme 'light-powerline))
;; (use-package super-save :ensure t)
;; (use-package term-projectile :ensure t)
;; (use-package tree-sitter-indent :ensure t)

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

;; (use-package treesit-auto :ensure t)
;; (use-package w3m :ensure t)
;; (use-package web-beautify :ensure t)
;; (use-package web-mode :ensure t)
;; (use-package weyland-yutani-theme :ensure t)

(use-package whitespace-cleanup-mode :ensure t)
(use-package xkcd :defer t)

;; (use-package yasnippet :ensure t :pin melpa)

;; ;; should have a separate section for Elisp libraries
;; (use-package dash :ensure t) ;; list functions
;; (use-package s :ensure t) ;; string functions


;;; Everything Else

;; visible-bell is very nice on Linux and very obnoxious on a Mac
(setq visible-bell (not (equal system-type 'darwin)))
(setq
 fill-column 120
 indent-tabs-mode nil)
;; surely there's a better function for this?
(setq auto-mode-alist (alist-key-add-or-replace "\\.ts\\'" 'typescript-ts-mode auto-mode-alist))
(setq auto-mode-alist (alist-key-add-or-replace "\\.tsx\\'" 'tsx-ts-mode auto-mode-alist))

(setq column-number-mode t)
(setq sentence-end-double-space nil)

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
  (add-hook 'server-after-make-frame-hook #'setup-frame-for-mac)
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

;; Tell emacs lisp mode to do the right thing on build.
(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (setq-local compile-command
               (concat
                "emacs -batch -f batch-byte-compile "
                (if buffer-file-name
                    (shell-quote-argument buffer-file-name))))))


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
  (tsx-ts-mode . lsp-deferred)
  ;;   (typescript-mode . company-mode)))
  ))


;; system locations

;; this may be _too_ clever
;; (setq global-node-executable (s-chomp (shell-command-to-string ". ~/.zshrc 2> /dev/null && nvm which default")))


;; Org-mode setup

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
;; (setq global-org-modern-mode t)

(message "%s" "init.el completed")
;;; init.el ends here :-)
