;; -*- lexical-binding: t -*-

;; based on https://github.com/DarwinAwardWinner/dotemacs,
;; which has a lot of useful things not included here.

;; Copyright (C) 2025 A Lloyd Flanagan

;; Filename: early-init.el
;; Author: A Lloyd Flanagan

;; This file is NOT part of GNU Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; The purpose of this file is to ensure that the latest org-mode is
;; installed before loading config.org.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


;;; Set up `use-package' package manager.

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
  ;; so I don't have to specify :ensure t on every call
  (require 'use-package-ensure))

(setopt use-package-always-ensure t)

;; drop into error trace if use-package errors
(when init-file-debug
  (setq
   use-package-verbose t
   use-package-expand-minimally nil
   use-package-compute-statistics t
   debug-on-error t))

(package-initialize)



;;; (Try to) Ensure the latest org-mode is installed
(condition-case nil
    (use-package org)
  (error
   (display-warning 'init "Could not install latest org-mode. Falling back to bundled version.")))

(require 'org)

;;; build rest of init code dynamically.
(save-window-excursion
  (org-babel-load-file (expand-file-name "config.org" (concat user-emacs-directory "my_emacs/")) nil))

;;; init.el ends here
