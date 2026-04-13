;; -*- lexical-binding: t -*-

;; based on https://github.com/DarwinAwardWinner/dotemacs,
;; which has a lot of useful things not included here.

;; Copyright (C) 2026 A Lloyd Flanagan

;; Filename: init.el
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


;;; Set up Elpaca package manager.

;; [Elpaca](https://github.com/progfolio/elpaca) is a replacement for the standard
;; Emacs package manger ~package.el~. It has many good features, but I mostly
;; wanted package upgrades and installs to occur in a background process.

(setopt
 package-archives
 '(("gnu" . "https://elpa.gnu.org/packages/") ("nongnu" . "https://elpa.nongnu.org/nongnu/")
   ("melpa" . "https://melpa.org/packages/")))

(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order
  '(elpaca :repo "https://github.com/progfolio/elpaca.git"
           :ref nil :depth 1 :inherit ignore
           :files (:defaults "elpaca-test.el" (:exclude "extensions"))
           :build (:not elpaca-activate)))
(let* ((repo (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list
   'load-path
   (if (file-exists-p build)
       build
     repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28)
      (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop
                    (apply #'call-process
                           `("git" nil ,buffer t "clone" ,@
                             (when-let* ((depth (plist-get order :depth)))
                               (list
                                (format "--depth=%d" depth)
                                "--no-single-branch"))
                             ,(plist-get order :repo) ,repo))))
                  ((zerop
                    (call-process "git"
                                  nil
                                  buffer
                                  t
                                  "checkout"
                                  (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop
                    (call-process emacs
                                  nil
                                  buffer
                                  nil
                                  "-Q"
                                  "-L"
                                  "."
                                  "--batch"
                                  "--eval"
                                  "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn
              (message "%s" (buffer-string))
              (kill-buffer buffer))
          (error "%s"
                 (with-current-buffer buffer
                   (buffer-string))))
      ((error)
       (warn "%s" err)
       (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil))
      (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; And we need to install Elpaca's own version of the ~use-package~ macro.

;; Install use-package support
(elpaca
    elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;; first we add melpa archive (use with caution!)
;; (setq package-archives
;;       '(("gnu" . "https://elpa.gnu.org/packages/")
;;         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
;;         ("melpa" . "https://melpa.org/packages/")))

;;; Bootstrap `use-package'
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; (eval-when-compile
;;   (require 'use-package)
;;   ;; so I don't have to specify :ensure t on every call
;;   (require 'use-package-ensure))

;; (setopt use-package-always-ensure t)

;; drop into error trace if use-package errors
;; (when init-file-debug
;;   (setq
;;    use-package-verbose t
;;    use-package-expand-minimally nil
;;    use-package-compute-statistics t
;;    debug-on-error t))

;; (package-initialize)



;; ;;; (Try to) Ensure the latest org-mode is installed
(use-package
 org
 :pin "gnu"
 :bind (("C-c l" . org-store-link) ("C-c a" . org-agenda) ("C-c c" . org-capture))
 :custom
 (org-adapt-indentation 'headline-data)
 (org-ctrl-k-protect-subtree t)
 (org-special-ctrl-a/e t)
 (org-return-follows-link t))

(require 'org)

;;; build rest of init code dynamically.
(save-window-excursion
  (org-babel-load-file (expand-file-name "config.org" (concat user-emacs-directory "my_emacs/")) nil))

;;; init.el ends here
