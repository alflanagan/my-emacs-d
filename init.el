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

;; see repository file README.md

;;; Code:

;; contents of .config/emacs/init.el:  (load "~/.config/emacs/my_emacs/init")

(defun add-subdirs-to-load-path (parent-directory)
  "Add PARENT-DIRECTORY and its immediate child directories to `load-path'."
  (let ((default-directory parent-directory))
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

;; (add-subdirs-to-load-path "/opt/homebrew/share/emacs/site-lisp")
(add-subdirs-to-load-path (directory-file-name (concat (expand-file-name user-emacs-directory) "my_emacs/lisp")))
;; (add-subdirs-to-load-path (directory-file-name (concat (expand-file-name user-emacs-directory) "elpa")))

(require 'secrets)

;; various settings gleaned from package better-defaults

(ido-mode t)
(setopt ido-enable-flex-matching t)

(require 'uniquify)

(save-place-mode t)
(show-paren-mode 1)
(global-auto-revert-mode 1)

(keymap-set global-map "M-/" 'hippie-expand)
(keymap-set global-map "C-x C-b" 'ibuffer)
(keymap-set global-map "C-s" 'isearch-forward-regexp)
(keymap-set global-map "C-r" 'isearch-backward-regexp)
(keymap-set global-map "C-M-s" 'isearch-forward)
(keymap-set global-map "C-M-r" 'isearch-backward)

(setq-default indent-tabs-mode nil)

(setopt
 apropos-do-all t
 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
 ;; displays current column in mode line
 column-number-mode t
 ;; moves file used by customize to my config subdir
 custom-file (concat user-emacs-directory "my_emacs/custom.el")
 ;; set initial window size, and enable scroll bars
 default-frame-alist
 '((horizontal-scroll-bars) (vertical-scroll-bars) (width . 180) (height . 70)) ;; see also early-init.el
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 save-place-file (concat user-emacs-directory "places")
 ;; integrate emacs with system clipboard
 select-enable-clipboard t
 select-enable-primary t
 ;; don't automatically double space after sentence end -- obsolete style
 sentence-end-double-space nil
 uniquify-buffer-name-style 'post-forward-angle-brackets
 user-email-address "lloyd.flanagan@proton.me"
 selection-coding-system 'utf-8)

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

;;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)

  ;; set up use-package
  ;; so I don't have to specify :ensure t on every call
  (require 'use-package-ensure))

(setq use-package-always-ensure t)

(package-initialize)


;;; Use Packages

;; drop into error trace if use-package errors
(when init-file-debug
  (setq
   use-package-verbose t
   use-package-expand-minimally nil
   use-package-compute-statistics t
   debug-on-error t))

;; KEEP THIS SORTED!

(use-package all-the-icons :if (display-graphic-p))
(use-package amx)
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

(use-package chatgpt-shell)
(use-package
 chatu
 :hook ((org-mode markdown-mode) . chatu-mode)
 :commands (chatu-add chatu-open)
 :custom
 ((chatu-input-dir (concat (expand-file-name user-emacs-directory) "./draws"))
  (chatu-output-dir (concat (expand-file-name user-emacs-directory) "./draws_out"))))

;; don't load company until a source file has loaded (check: startup load of org file doesn't load it)
(use-package company :hook prog-mode)

(use-package company-terraform :defer t)
(use-package company-web :defer t)
(use-package
 copilot
 :defer nil
 :hook prog-mode
 :bind (:map copilot-mode-map (("<tab>" . copilot-accept-completion) ("TAB" . copilot-accept-completion))))

(use-package csv-mode :defer t)
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
(use-package dockerfile-mode :defer t)
;; dogears is nice, but I'm not using it much
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
(use-package easysession :defer t)
(use-package editorconfig :config (editorconfig-mode 1))
(use-package eldoc :defer t) ;; :hook (typescript-ts-mode python-ts-mode))
(use-package eldoc-box :defer t :after eldoc)
(use-package
 elisp-autofmt
 :defer t
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook ((emacs-lisp-mode . elisp-autofmt-mode) (lisp-data-mode . elisp-autofmt-mode))
 :bind (:map lisp-mode-shared-map (("C-c f" . elisp-autofmt-buffer))))
(use-package elisp-lint :defer t)

(use-package elpy :defer t :init (advice-add 'python-mode :before 'elpy-enable))
(use-package emacsql :defer t)
(use-package emacsql-pg :defer t :after 'emacsql)
(use-package emmet-mode :hook html-mode)

;; also check out package 'ligature'
(use-package
 fira-code-mode
 ;; == and === come out as set operators on linux?
 :custom (fira-code-mode-disabled-ligatures '("[]" "===" "==" ":")) ; ligatures you don't want
 :hook prog-mode) ; mode to enable fira-code-mode in

;; Flycheck
(use-package flycheck :hook ((after-init . global-flycheck-mode)) :pin "nongnu")

(use-package highlight-parentheses)
(use-package ivy :config (ivy-mode 1))

;; lsp-mode enables integration with language server protocol
;; TODO: investigate how this interacts w/tree-sitter; I think some tree-sitter libraries also
;; use lsp
(use-package lsp-mode :defer t :commands lsp :hook (ruby-base-mode python-ts-mode swift-ts-mode sh-mode))
;; (use-package lsp-origami :hook ((lsp-after-open . lsp-origami-try-enable)))

(use-package lsp-treemacs :after lsp-mode)
(use-package lsp-ui :after lsp-mode)

(use-package magit :defer t)
(use-package magit-todos :defer t)
(use-package
 markdown-mode
 :mode ("README\\.md\\'" . gfm-mode)
 :init
 ; flycheck uses markdownlint, but generates very annoying error message instead of useful checks
 (flycheck-mode 0)
 :custom
 (markdown-code-lang-modes
  '(("ocaml" . tuareg-mode) ("elisp" . emacs-lisp-mode) ("ditaa" . artist-mode) ("asymptote" . asy-mode)
    ("dot" . fundamental-mode) ("sqlite" . sql-mode) ("calc" . fundamental-mode) ("C" . c-mode) ("cpp" . c++-mode)
    ("C++" . c++-mode) ("screen" . shell-script-mode) ("shell" . sh-mode) ("bash" . sh-mode) ("sh" . sh-ode)))
 (markdown-command "/Users/adrianflanagan/bin/markdown2"))
(use-package nodejs-repl)
(use-package nushell-ts-mode :defer t)


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

(use-package org-node :after org)

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

;; http://alhassy.com/org-special-block-extras/ -- define your own Org blocks
(use-package org-special-block-extras :defer t)
(use-package org-superstar :defer t)

(use-package ox-gfm :defer t :after org)
(use-package nice-org-html :defer t)


;; (use-package
;;  origami
;;  :defer t
;;  ;; don't want global origami mode -- it activates in org buffers, etc. where it shouldn't
;;  :bind (("C-+" . origami-forward-toggle-node) ("C-=" . origami-forward-toggle-node)))

(use-package page-break-lines :hook (emacs-lisp-mode))

(use-package prettier :hook (html-mode))

(use-package pyenv-mode :defer t)

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

;; Rainbow delimiters makes nested delimiters easier to understand
(use-package rainbow-delimiters :ensure t :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package smart-mode-line :config (sml/setup))
(use-package smart-mode-line-powerline-theme :config (sml/apply-theme 'light-powerline))
(use-package sql-indent :defer t)

;;; Swift

(defun find-sourcekit-lsp ()
  "Locate sourcekit-lsp on this file system."
  (or (executable-find "sourcekit-lsp")
      (and (eq system-type 'darwin) (string-trim (shell-command-to-string "xcrun -f sourcekit-lsp")))
      "/usr/local/swift/usr/bin/sourcekit-lsp"))

;;; Packages we want installed for Swift development
;; sourcekit-lsp support (Language Server Protocol implementation for Swift and C-based languages)
(use-package
 lsp-sourcekit
 :ensure t
 :after lsp-mode
 :custom (lsp-sourcekit-executable (find-sourcekit-lsp) "Find sourcekit-lsp"))

;; Swift editing support
(use-package swift-ts-mode :mode ("\\.swift\\'" . swift-ts-mode))


(use-package terraform-doc :defer t)
(use-package terraform-mode :defer t)

;; Treemacs https://github.com/Alexander-Miller/treemacs?tab=readme-ov-file#installation
(use-package
 treemacs
 :custom
 (treemacs-is-never-other-window t)
 (treemacs-sorting 'alphabetic-case-insensitive-asc)
 (treemacs-width-is-initially-locked t)
 (treemacs-project-follow-mode t)
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

(use-package tree-sitter :defer t)
(use-package
 treesit-auto
 :custom (treesit-auto-install 'prompt)
 :config
 (treesit-auto-add-to-auto-mode-alist 'all)
 (global-treesit-auto-mode))

(use-package
 treesit-fold
 :after (treesit)
 :hook ((prog-mode . treesit-fold-mode))
 :bind (:map global-map ("C-=" . treesit-fold-toggle)))

(defun mp-setup-install-grammars ()
  "Install Tree-sitter grammars if they are absent and not handled by treesit-auto."
  (interactive)
  (dolist (grammar
           '((hcl "https://github.com/tree-sitter-grammars/tree-sitter-hcl")
             (scss "https://github.com/tree-sitter-grammars/tree-sitter-scss")
             (graphql "https://github.com/bkegley/tree-sitter-graphql")))

    (add-to-list 'treesit-language-source-alist grammar)
    ;; Only install `grammar' if we don't already have it
    ;; installed. However, if you want to *update* a grammar then
    ;; this obviously prevents that from happening.
    (unless (treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar)))))

(mp-setup-install-grammars)

(use-package
 typescript-ts-mode
 :defer t
 :ensure nil ;; built-in mode
 :hook prettier-mode)

(use-package
 ; see https://web-mode.org/!!
 web-mode
 :defer t
 :mode
 (("\\.phtml\\'" . web-mode)
  ("\\.php\\'" . web-mode)
  ("\\.tpl\\'" . web-mode)
  ("\\.[agj]sp\\'" . web-mode)
  ("\\.as[cp]x\\'" . web-mode)
  ("\\.mustache\\'" . web-mode)
  ("\\.djhtml\\'" . web-mode))
 :custom (web-mode-enable-comment-interpolation t) (web-mode-enable-engine-detection t)
 ; this assumes we're always using django
 (web-mode-engines-alist '(("django" . "\\.html\\'"))))


(use-package whitespace-cleanup-mode :config (global-whitespace-cleanup-mode 1))
(use-package xkcd :defer t)


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
(push '("\\.\\([za]sh|bash\\)\\'" . bash-ts-mode) auto-mode-alist)
;; ("/\\(?:Pipfile\\|\\.?flake8\\)\\'" . conf-mode)

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

;; doesn't use use-package -- see lisp directory
;; nifty interface for execute-extended-command (M-x)
;; (require 'smex "smex/smex") replaced by amx
;; (smex-initialize) ;; not required, might make first use faster

;; MAC-specific setup
;; TODO: move this to site.macos.el
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

;; Mac keybindings that conflict with Emacs defaults
;; M-% launches screen capture
;; C-M-k brings up calender in MacOS topbar
;; C-M-q locks the screen
;; enable these for all environments so I don't have to remember on non-Macs
(bind-keys ("C-c C-q" . indent-pp-sexp) ("C-c C-s" . kill-sexp) ("C-%" . query-replace))

(keymap-set global-map "C-x M-r" #'remember)
(keymap-set global-map "C-x M-R" #'remember-region)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

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


;; enabled "risky" commands

(put 'downcase-region 'disabled nil)

(message "%s" "init.el completed")

;; Blacklisted Packages
;; this is reminder (to myself) not to use these packages, and why

;; angular-mode -- so very out of date...
;; djangonaut -- currently, djangonaut commands are failing
;; eglot -- built-in, fine as far as I know, superceded by lsp-mode
;; enh-ruby-mode -- doesn't support ruby-ts-mode??
;; fold-dwim -- it almost never Does What I Mean.
;; go -- the game, not the language -- causes crash in ivy?
;; org-modern --  makes org mode look really nice, but it makes editing much harder
;; paradox -- nice, but has problems like putting all its output into minibuffer and freezing emacs :-(
;; speedbar -- OK, but treemacs is better (for file lists anyway)

;;; init.el ends here :-)
