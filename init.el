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

;; TODO set up custom variable for path to my emacs customization files (obviously would have to do something tricky to
;; implement it, since the custom.el file is in the directory it would point to). Easiest solution is probably to use a
;; global variable with a default, then user can set it before loading this file.

;; custom lisp directory
(let ((default-directory (directory-file-name (concat (expand-file-name user-emacs-directory) "my_emacs/lisp"))))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;; this needs to go first so it can affect rest of the file. It also needs some setup?
;; see my_emacs/lisp/no-littering/migrate.org
;; (require 'no-littering)

(require 'alf-alists "alists")
(load "./secrets")

;; various settings gleaned from package better-defaults

(ido-mode t)
(setq ido-enable-flex-matching t)

;; these modes are discouraged because they use mouse -- keep hands on the keyboard!
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(require 'uniquify)

(save-place-mode t)
(show-paren-mode 1)
(global-auto-revert-mode 1)
(global-display-line-numbers-mode 1)

(keymap-set global-map "M-/" 'hippie-expand)
(keymap-set global-map "C-x C-b" 'ibuffer)
(keymap-set global-map "C-s" 'isearch-forward-regexp)
(keymap-set global-map "C-r" 'isearch-backward-regexp)
(keymap-set global-map "C-M-s" 'isearch-forward)
(keymap-set global-map "C-M-r" 'isearch-backward)

(setq-default indent-tabs-mode nil)
(setopt
 apropos-do-all
 t
 backup-directory-alist
 `(("." . ,(concat user-emacs-directory "backups")))
 column-number-mode
 t
 custom-file
 (concat user-emacs-directory "my_emacs/custom.el")
 default-frame-alist
 '((horizontal-scroll-bars) (vertical-scroll-bars) (width . 180) (height . 70)) ;; see also early-init.el
 mouse-yank-at-point
 t
 save-interprogram-paste-before-kill
 t
 save-place-file
 (concat user-emacs-directory "places")
 select-enable-clipboard
 t
 select-enable-primary
 t
 sentence-end-double-space
 nil
 uniquify-buffer-name-style
 'post-forward-angle-brackets
 user-email-address
 "lloyd.flanagan@proton.me"
 selection-coding-system
 'utf-8)


(load custom-file)

;; Fix directories
;; get HOME directory
;; check entries in org-agenda-files to see if they are in subdirs of HOME
;; if not, substitute actual home dir for home in variable (tricky-ish)
;; and this needs to be added to use-package so org-agenda-files exists
;; (let ((home-dir (getenv "HOME")))(dolist (agenda-file org-agenda-files))
;;      (if (not (or (void agenda-file) (string-prefix-p home-dir agenda-file)))
;;          (print agenda-file)))


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

;; angular-mode -- so very out of date...
;; djangonaut -- currently, djangonaut commands are failing
;; eglot -- built-in, fine as far as I know, superceded by lsp-mode
;; fold-dwim -- it almost never Does What I Mean.
;; go -- the game, not the language -- causes crash in ivy?
;; org-modern --  makes org mode look really nice, but it makes editing much harder
;; paradox -- nice, but has problems like putting all its output into minibuffer and freezing emacs :-(
;; speedar -- OK, but treemacs is better (for file lists anyway)

;;; Use Packages

;; KEEP THIS SORTED!

(use-package all-the-icons :if (display-graphic-p))
;; (use-package async)
;; (use-package auto-header)
;; (use-package auto-rename-tag)
(use-package bbdb :defer t)
;; (use-package blacken)
;; (use-package cargo-mode :pin "melpa" :hook 'rust-mode-hook)
(use-package
 casual-info
 :defer t
 :bind
 (:map
  Info-mode-map ("C-o" . casual-info-tmenu) ("M-[" . Info-history-back)
  ;; Use web-browser history navigation bindings
  ("M-]" . Info-history-forward)
  ;; Bind p and n to paragraph navigation
  ("p" . casual-info-browse-backward-paragraph) ("n" . casual-info-browse-forward-paragraph)
  ;; Bind h and l to navigate to previous and next nodes
  ;; Bind j and k to navigate to next and previous references
  ("h" . Info-prev) ("j" . Info-next-reference) ("k" . Info-prev-reference) ("l" . Info-next)
  ;; Bind / to search
  ("/" . Info-search)
  ;; Set Bookmark
  ("B" . bookmark-set))
 :hook ((Info-mode . hl-line-mode) (Info-mode . scroll-lock-mode)))

;; (use-package cmake-mode)
;; (use-package code-archive)
(use-package coffee-mode :defer t)
;; don't load company until a source file has loaded (check: startup load of org file doesn't load it)
(use-package company :hook prog-mode)

;; (use-package company-jedi)
;; (use-package company-math)
;; (use-package company-shell)
(use-package company-terraform :defer t)
(use-package company-web :defer t)
;; (use-package counsel)
;; (use-package counsel-projectile)
;; (use-package css-eldoc)
(use-package dashboard-hackernews)
(use-package
 dashboard
 :after
 dashboard-hackernews
 page-break-lines
 :custom
 (dashboard-items '((recents . 5) (bookmarks . 5) (projects . 10) (agenda . 5)))
 (dashboard-startupify-list
  '(dashboard-insert-banner
    dashboard-insert-newline
    dashboard-insert-banner-title
    dashboard-insert-newline
    dashboard-insert-init-info
    dashboard-insert-items
    dashboard-insert-newline
    (lambda () (dashboard-hackernews-insert 5))
    dashboard-insert-newline
    dashboard-insert-footer))
 (dashboard-banner-logo-title "My Dashboard")
 (dashboard-startup-banner 'logo)
 (dashboard-set-init-info t)
 ;; dashboard-set-heading-icons t
 (dashboard-set-file-icons t)
 ;; somehow this was getting set to nil?
 (dashboard-heading-icons
  '((recents . "history")
    (bookmarks . "bookmark")
    (agenda . "calendar")
    (projects . "rocket")
    (hackernews . "hacker-news")))
 ;;    (hackernews . (all-the-icons-faicon "hacker-news")))
 (dashboard-icon-type 'all-the-icons)
 (dashboard-projects-backend 'projectile)
 (dashboard-center-content t)
 (dashboard-week-agenda t)
 :config (dashboard-setup-startup-hook))

(use-package devdocs :defer t)
;; (use-package django-snippets)
;; (use-package docker-compose-mode)
(use-package dockerfile-mode :defer t)
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
  ("M-g M-D" . dogears-sidebar)
  ("M-g M-r" . dogears-remember)))
;; (use-package dumb-jump :config (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))
(use-package editorconfig :config (editorconfig-mode 1))
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
(use-package emacsql :defer t)
(use-package emacsql-pg :defer t :after 'emacsql)
;; (use-package eslint-disable-rule)
;; (use-package eslint-fix)
(use-package emmet-mode :hook html-mode)
(use-package enh-ruby-mode :hook ruby-mode)
(use-package erblint)

;; also check out package 'ligature'
(use-package
 fira-code-mode
 ;; == and === come out as set operators on linux?
 :custom (fira-code-mode-disabled-ligatures '("[]" "===" "==" ":")) ; ligatures you don't want
 :hook prog-mode) ; mode to enable fira-code-mode in


;; Flycheck
(use-package flycheck :hook ((after-init . global-flycheck-mode)) :pin "nongnu")
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
(use-package form-feed-st :hook (emacs-lisp-mode lisp-data-mode))
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
(use-package inf-ruby :defer t)
(use-package ivy :config (ivy-mode 1))
;; (use-package kotlin-ts-mode)
;; (use-package lispy)

(use-package lsp-mode :defer t :commands lsp)
(use-package lsp-origami :hook ((lsp-after-open . lsp-origami-try-enable)))
(use-package magit :defer t)
(use-package magit-todos :defer t)
;; (use-package markdown-toc)
;; (use-package morlock :config (global-morlock-mode 1)) ;; additional syntax highlighting for ELisp
(use-package ng2-mode :defer t)
(use-package nodejs-repl)
;; (use-package nov) ;; epub reader


;; org-mode packages

(use-package
 org
 :pin "gnu"
 :bind (("C-c l" . org-store-link) ("C-c a" . org-agenda) ("C-c c" . org-capture))
 :custom
 (org-adapt-indentation 'headline-data)
 (org-ctrl-k-protect-subtree t)
 (org-special-ctrl-a/e t)
 (org-return-follows-link t))

(use-package org-beautify-theme)
(use-package
 org-contacts
 :defer t
 ;;  :config
 ;;  (push org-capture-templates
 ;;        '("c" "Contacts" entry (file "~/org/contacts.org")
 ;;          "* %(org-contacts-template-name)
 ;; :PROPERTIES:
 ;; :EMAIL: %(org-contacts-template-email)
 ;; :PHONE:
 ;; :ALIAS:
 ;; :NICKNAME:
 ;; :IGNORE:
 ;; :ICON:
 ;; :NOTE:
 ;; :ADDRESS:
 ;; :BIRTHDAY:
 ;; :END:"))
 )

(use-package org-chef :defer t)
(use-package org-elisp-help :defer t)
;; org-gcal requires setup to contact google calendar
;; org-gcal: must set ‘org-gcal-client-id’ and ‘org-gcal-client-secret’ for this package to work. Please run ‘org-gcal-reload-client-id-secret’ after setting these variables.
;; (use-package org-gcal)

;; (use-package org-contrib)
;; (use-package org-ai)
;; (use-package org-msg)
;; (use-package org-ql)
(use-package
 org-recur
 :hook ((org-mode . org-recur-mode) (org-agenda-mode . org-recur-agenda-mode))
 :demand t
 :bind
 (:map
  org-recur-mode-map ("C-c d" . org-recur-finish)
  ;; Rebind the 'd' key in org-agenda (default: `org-agenda-day-view').
  :map org-recur-agenda-mode-map ("d" . org-recur-finish) ("C-c d" . org-recur-finish))
 :custom (org-recur-finish-done t) (org-recur-finish-archive t))

(use-package org-shoplist :defer t)
(use-package org-superstar :defer t)
;; http://alhassy.com/org-special-block-extras/ -- define your own Org blocks
(use-package org-special-block-extras :defer t)
;; (use-package org-web-tools)

(use-package
 origami
 :defer t
 ;; don't want global origami mode -- it activates in org buffers, etc. where it shouldn't
 :bind (("C-+" . origami-forward-toggle-node) ("C-=" . origami-forward-toggle-node)))

(use-package page-break-lines)
(use-package poly-erb :defer t)
;; (use-package parrot)

(use-package prettier :hook (html-mode ng2-html-mode))
(use-package robe :defer t)

;; attempt to set up equivalent keys on Mac and my PC.
(if (equal system-type 'darwin)
    (progn
      (use-package
       projectile
       :init (keymap-global-unset "s-p")
       :config (projectile-mode +1)
       :bind (:map projectile-mode-map ("s-p" . projectile-command-map))))
  ;; on GNOME desktop, "s-p" opens desktop menu
  (use-package
   projectile
   :init (keymap-global-unset "M-p")
   :config (projectile-mode +1)
   :bind (:map projectile-mode-map ("M-p" . projectile-command-map))))

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
(use-package terraform-doc :defer t)
(use-package terraform-mode :defer t)

;; Treemacs https://github.com/Alexander-Miller/treemacs?tab=readme-ov-file#installation
(use-package
 treemacs
 :config (line-number-mode 0)
 :custom
 (treemacs-is-never-other-window t)
 (treemacs-sorting 'alphabetic-case-insensitive-asc)
 (treemacs-width-is-initially-locked t)
 :bind
 ;; some of these replace functions from tab-bar, which I don't use anyway
 (:map
  global-map
  ("M-0" . treemacs-select-window)
  ("C-x t 1" . treemacs-delete-other-windows)
  ("C-x t t" . treemacs)
  ("C-x t d" . treemacs-select-directory)
  ("C-x t B" . treemacs-bookmark)
  ("C-x t C-t" . treemacs-find-file)
  ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile :after (treemacs projectile))

(use-package treemacs-magit :after (treemacs magit))

;; see git page above for other related package setups
(treemacs-start-on-boot)


(use-package
 tree-sitter
 :hook
 ((python-ts-mode . lsp-deferred)
  (python-ts-mode . eldoc-mode)
  (python-ts-mode . eldoc-box-hover-mode)
  (python-ts-mode . company-mode)))
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
             (html . ("https://github.com/tree-sitter/tree-sitter-html"))
             (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
             (json . ("https://github.com/tree-sitter/tree-sitter-json"))
             (make "https://github.com/alemuller/tree-sitter-make")
             (markdown "https://github.com/ikatyang/tree-sitter-markdown")
             (python . ("https://github.com/tree-sitter/tree-sitter-python"))
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

;;
;; (with-eval-after-load 'typescript-ts-mode
;;  "sets up typescript-ts-mode-hook with more goodies"
(use-package
 typescript-ts-mode
 :defer t
 :ensure nil ;; built-in mode
 ;; :hook  ;; this is not working at all
 ;; ((typescript-ts-mode . lsp-deferred)
 ;;  (typescript-ts-mode . display-line-numbers-mode)
 ;;  (typescript-ts-mode . (lambda () (setq flycheck-check-syntax-automatically '(save mode-enabled))))
 ;;  (typescript-ts-mode . flycheck-mode)
 ;;  (typescript-ts-mode . eldoc-mode)
 ;;  (typescript-ts-mode . eldoc-box-hover-mode)
 ;;  (tsx-ts-mode . lsp-deferred)
 ;;  (typescript-ts-mode . company-mode))
 :config
 (progn
   (add-hook 'typescript-ts-mode-hook #'lsp-deferred)
   (add-hook 'typescript-ts-mode-hook #'eldoc-mode)
   (add-hook 'typescript-ts-mode-hook #'eldoc-box-hover-mode)
   (add-hook 'typescript-ts-mode-hook #'company-mode)
   (add-hook 'typescript-ts-mode-hook #'display-line-numbers-mode)
   (add-hook 'typescript-ts-mode-hook #'prettier-mode)))

;; had a lot of undo info disappear -- maybe user error?
;; (use-package undo-fu :defer t)

;; (use-package w3m)
;; (use-package web-beautify)
;; (use-package web-mode)
(use-package weyland-yutani-theme)

(use-package whitespace-cleanup-mode :config (global-whitespace-cleanup-mode 1))
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


(push '("\\.ts\\'" . typescript-ts-mode) auto-mode-alist)
(push '("\\.tsx\\'" . tsx-ts-mode) auto-mode-alist)
(push '("\\.js[mx]?\\'" . js-ts-mode) auto-mode-alist)
(push '("\\.har\\'" . js-ts-mode) auto-mode-alist)
;; why is this not working for .yml files?
(push '("\\.ya?ml\\'" . yaml-ts-mode) auto-mode-alist)
(push '("\\.yml\\'" . yaml-ts-mode) auto-mode-alist)

;; not having a lot of luck setting up emacs as a brew service, so far
;; and this doesn't seem to work either.
(use-package
 server
 :config
 (when (null server-process)
   (server-start)))

;; because I often hit this key by accident and use "C-x C-c" instead anyway
(keymap-global-unset "s-q" nil)
;; (keymap-global-set "C-x C-p" (lambda (x) (interactive) (project-list-buffers t)))


;; MAC-specific setup

(when (equal system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (set-fontset-font t 'symbol (font-spec :family "Apple Symbols") nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
  (use-package org-mac-link)
  (use-package osx-lib :defer t)
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
