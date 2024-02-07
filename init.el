;; -*- lexical-binding: t; indent-tabs-mode: nil; -*-

;;; init.el -- Non-site-specific initialization
;; Copyright © 2024, A. Lloyd Flanagan

;; Author: A. Lloyd Flanagan <a.lloyd.flanagan@gmail.com>
;; Maintainer: A. Lloyd Flanagan <a.lloyd.flanagan@gmail.com>
;; Created: 2014

;; This file is not part of GNU Emacs.

;; tree-sitter: to install a language use treesit-install-language-grammar. If the language is part of the tree-sitter
;; project it will fill in the URL for you, just accept the defaults.


;; TODO: iterate over packages and add all subdirs
(push (expand-file-name "~/.emacs.d/packages/emacs-w3m") load-path)
(push (expand-file-name "~/.emacs.d/packages/better-defaults") load-path)

(let ((lispdir (expand-file-name "~/.emacs.d/lisp")))
  (unless (member lispdir load-path)
    (push lispdir load-path)))

;; we only want to use a local version, not a package
;; maybe better to use package version and undo the defaults I don't like?
(use-package better-defaults :ensure nil)
;; better-defaults set custom-file to custom.el
(load custom-file)


;; customization took care of the list of packages, so (use-package) is only needed
;; when you want to set :init, :config, :bind, etc.
(use-package
  package
 :config
 (progn
   (package-initialize)
   (paradox-enable)))

(use-package
 ace-jump-mode
  :bind (("C-c SPC" . ace-jump-mode) ("C-x SPC" . ace-jump-mode-pop-mark)))

(use-package smart-mode-line
  :config (progn
   (sml/setup)
   (sml/apply-theme 'powerline)))

(use-package ibuffer :bind (("C-x C-b" . ibuffer-list-buffers)))

;; because I often hit this key by accident and use "C-x C-c" instead anyway
(keymap-global-unset "s-q" nil)
(keymap-global-set "C-x C-p" #'(project-list-buffers t))

(use-package
 server
 :config
 (unless (default-value 'server-mode)
   (server-start)))

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
;; (keymap-global-set "C-c C-s" 'kill-sexp)
;; (keymap-global-set "C-c C-q" 'indent-pp-sexp)

(bind-keys ("C-c C-q" . indent-pp-sexp) ("C-c C-s" . kill-sexp) ("C-%" . query-replace))

;; steve yegge's replacements for using Meta
(keymap-global-set "C-x C-m" 'execute-extended-command)
(keymap-global-set "C-c C-m" 'execute-extended-command)

(use-package
 projectile
 :config (projectile-mode +1)
 :bind (:map projectile-mode-map ("s-p" . projectile-command-map)))

(use-package
 gameoflife
 :config
 (progn
   (setq gameoflife-screensaver-timeout 300)
   (gameoflife-screensaver-mode 1)))

(use-package mwim :bind (("C-a" . mwim-beginning) ("C-e" . mwim-end)))

(use-package elisp-autofmt :bind (("C-c f" . elisp-autofmt-buffer)))

(use-package
 immaterial-theme
 :config
 (progn
   (load-theme 'immaterial-dark t)
   (load-theme 'immaterial-light t)))

(use-package ivy :config (ivy-mode 1))

(defun typescript-set-up-tree-sitter ()
  "Sets up Typescript to be found by tree-sitter, because it's directory structure is not the default."
  (interactive)
  (add-to-list
   'treesit-language-source-alist
   '(typescript . '("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))))

(use-package
 tree-sitter
 :config
 (progn
   (add-hook 'tree-sitter-after-first-parse-hook #'typescript-set-up-tree-sitter)))
p
(use-package form-feed-st :config (add-hook 'emacs-lisp-mode-hook 'form-feed-st-mode))

(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (setq-local compile-command
               (concat
                "emacs -batch -f batch-byte-compile "
                (if buffer-file-name
                    (shell-quote-argument buffer-file-name))))))

;;; init.el ends here :-)
