
# Table of Contents

1.  [Emacs Configuration](#org507ed5e)
    1.  [File Setup](#orga82fd62)
    2.  [Legal Stuff](#org2e2acd3)
    3.  [Load Path](#orgd3a47a2)
    4.  [Customization](#org7bcdf07)
    5.  [Customizing Emacs Itself](#org4946e61)
    6.  [Packages](#orgd4cd9fd)
        1.  [Better Mode Line](#orgb799ed1)
        2.  [Ido and amx](#org7f89773)
        3.  [Page-break Lines](#org97d1940)
        4.  [Treemacs](#org2b4d6b0)
        5.  [ELisp Programming](#org5e850a5)
        6.  [Language Server Protocol](#orgd76d374)
        7.  [Org Mode](#orgfdf56cc)
    7.  [End of File](#org7e6c944)


<a id="org507ed5e"></a>

# Emacs Configuration

This is file config.org: Main emacs configuration.


<a id="orga82fd62"></a>

## File Setup

**Must** be first line of the file.

    ;; -*- lexical-binding: t; -*-


<a id="org2e2acd3"></a>

## Legal Stuff

Legal/copyright notice for the generated file. All terms are assumed to
apply to this file also

    ;;; config.el -- Non-site-specific initialization -*- lexical-binding: t; -*-
    
    ;; Copyright © 2025 A. Lloyd Flanagan
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


<a id="orgd3a47a2"></a>

## Load Path

In my local /lisp subdirectory, I've got some third-party projects
which should be loaded from local files.

    (defun add-subdirs-to-load-path (parent-directory)
      "Add PARENT-DIRECTORY and its immediate child directories to `load-path'."
      (let ((default-directory parent-directory))
        (add-to-list 'load-path default-directory)
        (normal-top-level-add-subdirs-to-load-path)))
    
    ;; (add-subdirs-to-load-path "/opt/homebrew/share/emacs/site-lisp")
    (add-subdirs-to-load-path
     (directory-file-name
      (concat (expand-file-name user-emacs-directory) "my_emacs/lisp")))


<a id="org7bcdf07"></a>

## Customization

Set up seperate file for use by emacs customzation.

I prefer to set customization options in the `use-package` declaration
for each package, but on-the-fly or local-only customizations will go
to this file.

    (setq custom-file "~/.config/emacs/my_emacs/custom.el")
    (load custom-file)


<a id="org4946e61"></a>

## Customizing Emacs Itself

We can use `use-package` to configure emacs itself as a 'pseudo-package'.

    (use-package
     emacs
    
     :bind
     (("M-/" . 'hippie-expand)
      ("C-x C-b" . 'ibuffer)
      ("C-s" . 'isearch-forward-regexp)
      ("C-r" . 'isearch-backward-regexp)
      ("C-M-s" . 'isearch-forward)
      ("C-M-r" . 'isearch-backward))
    
     :custom
     (ido-mode t)
     (ido-enable-flex-matching t)
     (ido-big-directories '("node_modules" "\\.?venv"))
     (ido-cannot-complete-command 'ido-completion-help)
     (ido-ignore-directories
      '("\\`CVS/" "\\`\\.\\./" "\\`\\./" "\\`__pycache__/"))
     (ido-use-filename-at-point nil)
     (ido-use-url-at-point t)
     (ido-use-virtual-buffers 'auto)
    
     (apropos-do-all t)
     ;; put file backups in single directory, not in same directory with '~' appended.
     (backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
     (column-number-mode t "displays current column in mode line")
     (default-frame-alist
      '((horizontal-scroll-bars)
        (vertical-scroll-bars)
        (width . 180)
        (height . 70))
      "set initial window size, and enable scroll bars") ;; see also early-init.el
     (global-auto-revert-mode t)
     (global-display-line-numbers-mode t)
     (mouse-yank-at-point t)
     (save-interprogram-paste-before-kill t)
     (save-place-mode t)
     (save-place-file (concat user-emacs-directory "places"))
     (select-enable-clipboard t "integrate emacs with system clipboard")
     (select-enable-primary t)
     (selection-coding-system 'utf-8)
     (sentence-end-double-space
      nil
      "don't automatically double space after sentence end -- obsolete style")
     (show-paren-mode t)
     (uniquify-buffer-name-style 'post-forward-angle-brackets)
     (user-email-address "lloyd.flanagan@proton.me"))


<a id="orgd4cd9fd"></a>

## Packages

Eventual goal is to remove these from customization entirely, and use
`use-package` for all.


<a id="orgb799ed1"></a>

### Better Mode Line

[Smart-mode-line](https://github.com/Malabarba/smart-mode-line) is a sexy mode-line for Emacs. It aims to be easy to
read from small to large monitors by using colors, a prefix feature,
and smart truncation.

    (use-package smart-mode-line :config (sml/setup))
    (use-package
     smart-mode-line-powerline-theme
     :config (sml/apply-theme 'powerline))


<a id="org7f89773"></a>

### Ido and amx

There are (at least) two major emacs packages to enable
auto-completion: "ivy" and "ido". Ivy is full-featured and includes
"counsel", which modifies emacs commands to use ivy. However, it looks
not to be actively maintained, and I've come to prefer "ido". It's
builtin to Emacs, which is convenient.

The ["amx" package](https://github.com/DarwinAwardWinner/amx) leverages "ido" to add features to the Emacs
`execute-extended-command` function (M-x).

    (use-package amx)


<a id="org97d1940"></a>

### Page-break Lines

This is a neat little package that displays embedded ctrl-L characters
as horizontal lines. This helps break up the file on the screen, not
just when printed.

    (use-package page-break-lines :config (global-page-break-lines-mode))


<a id="org2b4d6b0"></a>

### Treemacs

Set up treemacs, the directory tree sidebar. See [treemacs repo](https://github.com/Alexander-Miller/treemacs) for all
the gory details.

    (use-package
     treemacs
     :ensure t
     :defer t
     :init
     (with-eval-after-load 'winum
       (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
     :custom
     (treemacs-project-follow-mode t)
     (treemacs-filewatch-mode t)
     (treemacs-hide-gitignored-files-mode nil)
     (when treemacs-python-executable
       (treemacs-git-commit-diff-mode t))
     (treemacs-is-never-other-window t)
     (treemacs-follow-after-init t)
    
     :bind
     ("M-0" . treemacs-select-window)
     ("C-x t 1" . treemacs-delete-other-windows)
     ("C-x t t" . treemacs)
     ("C-x t d" . treemacs-select-directory)
     ("C-x t B" . treemacs-bookmark)
     ("C-x t C-t" . treemacs-find-file)
     ("C-x t M-t" . treemacs-find-tag)
    
     :hook (after-init-hook . treemacs))

1.  Treemacs-icons-dired

    Treemacs icons for Dired.  Code is based on all-the-icons-dired.el
    
        (use-package
         treemacs-icons-dired
         :after (treemacs)
         :hook (dired-mode . treemacs-icons-dired-enable-once)
         :ensure t)

2.  Treemacs-magit &#x2013; integrate git with treemacs.

        (use-package treemacs-magit :after (treemacs magit) :ensure t)

3.  Treemacs start on boot.

    This function is recommended to ensure all elements of treemacs are
    properly in place.
    
        (treemacs-start-on-boot)


<a id="org5e850a5"></a>

### ELisp Programming

1.  Linting

        (use-package elisp-lint :defer t)

2.  Elisp-autofmt

        (use-package
         elisp-autofmt
         :defer t
         :commands (elisp-autofmt-mode elisp-autofmt-buffer)
         :hook
         ((emacs-lisp-mode . elisp-autofmt-mode)
          (lisp-data-mode . elisp-autofmt-mode))
         :bind (:map lisp-mode-shared-map (("C-c f" . elisp-autofmt-buffer))))


<a id="orgd76d374"></a>

### Language Server Protocol

Not long ago, an editor had to have custom handling written for each
language it "knew". This led to a lot of duplication of effort, and
bad editor implementations.

Now there are 2 major protocols that can be provided by language
authors to communicate information about a language to an editor. The
Language Server Protocol allows the editor to connect to an external
server process, which provides information about the language.

The Tree-Sitter protocol provides a set of shared libraries which the
editor dynamically links to, and that run in the editor's process.

Emacs now supports both methods.

This sets up lsp-mode to integrate with the LSP, and then we use a
package to integrate the information into the `treemacs` sidebar also.

    ;; lsp-mode enables integration with language server protocol
    ;; TODO: investigate how this interacts w/tree-sitter; I think some tree-sitter libraries also
    ;; use lsp
    (use-package lsp-mode :defer t :commands lsp :hook prog-mode)
    
    (use-package lsp-treemacs :after lsp-mode)


<a id="orgfdf56cc"></a>

### Org Mode

Org mode itself is set up in the `init.el` file, because we need to
load the newer version in order to process this file correctly.

1.  Beautify Theme

        (use-package org-beautify-theme :after org-mode)

2.  Add Markdown as an Export Format

        (use-package ox-gfm :after org-mode)


<a id="org7e6c944"></a>

## End of File

    ;;; init.el ends here

