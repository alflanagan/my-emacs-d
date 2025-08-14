- [Emacs Configuration](#org5261455)
  - [File Setup](#org3e2525f)
  - [Legal Stuff](#orga03854a)
  - [Load Path](#orga7a5a58)
  - [Customization](#org96e0d27)
  - [Customizing Emacs Itself](#orgcc35022)
  - [Packages](#org0c0a4f9)
    - [Default Font](#orga13250d)
    - [Better Mode Line](#org4dcbe9a)
    - [Ido and amx](#org926e23c)
    - [Page-break Lines](#org0de75ed)
    - [Treemacs](#orgd866940)
    - [Treesit](#orgbcf4462)
    - [ELisp Programming](#orgff548a1)
    - [Language Server Protocol](#orgb447553)
    - [Org Mode](#orgc132a37)
  - [End of File](#org054e431)


<a id="org5261455"></a>

# Emacs Configuration

This is file config.org: Main emacs configuration.


<a id="org3e2525f"></a>

## File Setup

**Must** be first line of the file.

```emacs-lisp
;; -*- lexical-binding: t; -*-
```


<a id="orga03854a"></a>

## Legal Stuff

Legal/copyright notice for the generated file. All terms are assumed to apply to this file also

```emacs-lisp
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

```


<a id="orga7a5a58"></a>

## Load Path

In my local /lisp subdirectory, I've got some third-party projects which should be loaded from local files.

```emacs-lisp
(defun add-subdirs-to-load-path (parent-directory)
  "Add PARENT-DIRECTORY and its immediate child directories to `load-path'."
  (let ((default-directory parent-directory))
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

;; (add-subdirs-to-load-path "/opt/homebrew/share/emacs/site-lisp")
(add-subdirs-to-load-path
 (directory-file-name
  (concat (expand-file-name user-emacs-directory) "my_emacs/lisp")))
```


<a id="org96e0d27"></a>

## Customization

Set up separate file for use by emacs customization.

I prefer to set customization options in the `use-package` declaration for each package, but on-the-fly or local-only customizations will go to this file.

```emacs-lisp
(setq custom-file "~/.config/emacs/my_emacs/custom.el")
(load custom-file)
```


<a id="orgcc35022"></a>

## Customizing Emacs Itself

We can use `use-package` to configure emacs itself as a 'pseudo-package'.

There's a message displayed in the echo area on startup. Disabling it requires setting a variable to your user name. Unless your user name is the same as mine (hey!), youll need to change it.

```emacs-lisp
(use-package
 emacs

 :bind
 (("M-/" . 'hippie-expand)
  ("C-x C-b" . 'ibuffer)
  ("C-s" . 'isearch-forward-regexp)
  ("C-r" . 'isearch-backward-regexp)
  ("C-M-s" . 'isearch-forward)
  ("C-M-r" . 'isearch-backward))

 :config (setq inhibit-startup-echo-area-message "adrianflanagan")

 :custom
 (ido-mode t)
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
 (user-email-address "lloyd.flanagan@proton.me")
 (use-file-dialog nil)
 (initial-scratch-message nil))
```


<a id="org0c0a4f9"></a>

## Packages

Eventual goal is to remove these from customization entirely, and use `use-package` for all.


<a id="orga13250d"></a>

### Default Font

Set up with my current programming font, Fira Code. You must have Fira Code installed on your system for this to work.

```emacs-lisp
(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 160
                    :weight 'normal
                    :slant 'normal
                    :width 'normal)
```

1.  fira-code-mode

    This minor mode enables the use of Fira Code Symbol font to display various ligatures. It's a bit of eye candy.
    
    You need to have Fira Code Symbol installed, see [fira-code-mode](https://github.com/jming422/fira-code-mode).
    
    ```emacs-lisp
    (use-package
     fira-code-mode
     :custom
     (fira-code-mode-disabled-ligatures
      '(";;" "[]" "===" "==" ":" "#{" "#(" "#_" "#_("))
     :hook prog-mode)
    ```


<a id="org4dcbe9a"></a>

### Better Mode Line

[Smart-mode-line](https://github.com/Malabarba/smart-mode-line) is a sexy mode-line for Emacs. It aims to be easy to read from small to large monitors by using colors, a prefix feature, and smart truncation.

```emacs-lisp
(use-package smart-mode-line :config (sml/setup))
(use-package
 smart-mode-line-powerline-theme
 :config (sml/apply-theme 'powerline))
```


<a id="org926e23c"></a>

### Ido and amx

There are (at least) three major emacs packages to enable auto-completion: "ivy", "company", and "ido". Ivy is full-featured and includes "counsel", which modifies emacs commands to use ivy. However, it looks not to be actively maintained, and I've come to prefer "ido". It's builtin to Emacs, which is convenient.

The ["amx" package](https://github.com/DarwinAwardWinner/amx) leverages "ido" to add features to the Emacs `execute-extended-command` function (M-x).

```emacs-lisp
(use-package amx)
```


<a id="org0de75ed"></a>

### Page-break Lines

This is a neat little package that displays embedded ctrl-L characters as horizontal lines. This helps break up the file on the screen, not just when printed.

```emacs-lisp
(use-package page-break-lines :config (global-page-break-lines-mode))
```


<a id="orgd866940"></a>

### Treemacs

Set up treemacs, the directory tree sidebar. See [treemacs repo](https://github.com/Alexander-Miller/treemacs) for all the gory details.

```emacs-lisp
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
```

1.  Treemacs-icons-dired

    Treemacs icons for Dired. Code is based on all-the-icons-dired.el
    
    ```emacs-lisp
    (use-package
     treemacs-icons-dired
     :after (treemacs)
     :hook (dired-mode . treemacs-icons-dired-enable-once)
     :ensure t)
    ```

2.  Treemacs-magit &#x2013; integrate git with treemacs.

    ```emacs-lisp
    (use-package treemacs-magit :after (treemacs magit) :ensure t)
    ```

3.  Treemacs start on boot.

    This function is recommended to ensure all elements of treemacs are properly in place.
    
    ```emacs-lisp
    (treemacs-start-on-boot)
    ```


<a id="orgbcf4462"></a>

### Treesit

Set up to use tree-sitter modes automatically, where one exists. Not sure of the exact details of how treesit and LSP (next section) work together. As far as I can tell, they do.

[(External) Tree Sitter Docs](https://emacs-tree-sitter.github.io/) (some docs here: [info "(elisp) Parsing Program Source"](info:elisp#Parsing Program Source)).

```emacs-lisp
(use-package
 treesit-auto
 :custom (treesit-auto-install 'prompt)
 :config
 (treesit-auto-add-to-auto-mode-alist 'all)
 (global-treesit-auto-mode))
```


<a id="orgff548a1"></a>

### ELisp Programming

1.  Linting

    ```emacs-lisp
    (use-package elisp-lint :defer t)
    ```

2.  Elisp-autofmt

    ```emacs-lisp
    (use-package
     elisp-autofmt
     :defer t
     :commands
     (elisp-autofmt-mode elisp-autofmt-buffer elisp-autofmt-region)
     :custom
     (elisp-autofmt-format-quoted nil)
     (elisp-autofmt-use-default-override-defs t)
     :hook
     ((emacs-lisp-mode . elisp-autofmt-mode)
      (lisp-data-mode . elisp-autofmt-mode))
     :bind
     (:map
      lisp-mode-shared-map
      (("C-c f" . elisp-autofmt-buffer)
       ("C-c r" . elisp-autofmt-region))))
    ```


<a id="orgb447553"></a>

### Language Server Protocol

Not long ago, an editor had to have custom handling written for each language it "knew". This led to a lot of duplication of effort, and bad editor implementations.

Now there are 2 major protocols that can be provided by language authors to communicate information about a language to an editor. The Language Server Protocol allows the editor to connect to an external server process, which provides information about the language.

The Tree-Sitter protocol provides a set of shared libraries which the editor dynamically links to, and that run in the editor's process.

Emacs now supports both methods.

This sets up lsp-mode to integrate with the LSP, and then we use a package to integrate the information into the `treemacs` sidebar also.

[LSP Mode Docs](https://emacs-lsp.github.io/lsp-mode/)

```emacs-lisp
(use-package
 lsp-mode
 :defer t
 :commands lsp
 :hook prog-mode
 :custom (lsp-enable-snippet nil))
(use-package lsp-treemacs :after lsp-mode)
```

Since we've hooked `prog-mode`, it's common to get warnings about a language not having a matching server. Turn those off.

```emacs-lisp
(setopt lsp-warn-no-matched-clients nil)
```


<a id="orgc132a37"></a>

### Org Mode

Org mode itself is set up in the `init.el` file, because we need to load the newer version in order to process this file correctly.

1.  Beautify Theme

    ```emacs-lisp
    (use-package org-beautify-theme :after org-mode)
    ```

2.  Add Markdown as an Export Format

    ```emacs-lisp
    (use-package ox-gfm :after org-mode)
    ```


<a id="org054e431"></a>

## End of File

```emacs-lisp
;;; init.el ends here
```
