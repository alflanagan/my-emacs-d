;;; my_misc.el -- some generally-useful functions   -*- lexical-binding: t -*-

;; Copyright 2024 A. Lloyd Flanagan
;;
;; Author: A. Lloyd Flanagan <lloyd.flanagan@proton.me>
;; Version: 0.1

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

;;; Code:

;;TODO: An implementation of list comprehension ala Python
;;(defmacro list-comprehension (expr list condition) ...)
;; "Generate a list by evaluating EXPR with each member of LIST that
;;satisfies CONDITION.
;;
;;EXPR should be an expression with a variable named "it".

(defun members-with-suffix (source-list suffix-string)
  "Return a list composed of every member of SOURCE-LIST for which `string-suffix-p' detects suffix SUFFIX-STRING."
  (if (null source-list)
      nil
    (if (string-suffix-p suffix-string (car source-list))
        (cons (car source-list) (members-with-suffix (cdr source-list) suffix-string))
      (members-with-suffix (cdr source-list) suffix-string))))

(defun file-name-paths (directory file-list)
  "Return the list created by prepending DIRECTORY to each member FILE-LIST."
  (mapcar (lambda (it) (expand-file-name it directory)) file-list))

(defun my/list-use-packages (&optional file)
  "List all packages declared with `use-package' in FILE.
FILE defaults to config.org under `user-emacs-directory'/my_emacs/.
Displays results in a '*use-package list*' buffer and returns the
sorted package list."
  (interactive)
  (let* ((config-file
          (or file
              (expand-file-name "my_emacs/config.org" user-emacs-directory)))
         (packages '()))
    (with-temp-buffer
      (insert-file-contents config-file)
      (goto-char (point-min))
      (while (re-search-forward "(use-package[[:space:]]+\\([^[:space:]\n)]+\\)"
                                nil
                                t)
        (save-excursion
          (goto-char (match-beginning 0))
          (beginning-of-line)
          (unless (looking-at "[[:space:]]*;")
            (push (match-string-no-properties 1) packages)))))
    (setq packages (sort (delete-dups packages) #'string<))
    (with-output-to-temp-buffer "*use-package list*"
      (princ
       (format "Packages declared with use-package in %s:\n\n"
               (file-name-nondirectory config-file)))
      (dolist (pkg packages)
        (princ (format "  %s\n" pkg))))
    packages))

(defun my/missing-use-package (&optional file)
  "Check `package-selected-packages' against `use-package' forms in FILE.
FILE defaults to config.org under `user-emacs-directory'/my_emacs/.
If `package-selected-packages' is empty, report that in a new buffer.
Otherwise, for each selected package with no matching `use-package'
form in FILE, write its name to a buffer named \"missing use-package\"
and display that buffer."
  (interactive)
  (if (null package-selected-packages)
      (with-current-buffer (get-buffer-create "*package-selected-packages*")
        (erase-buffer)
        (insert "package-selected-packages is empty.")
        (display-buffer (current-buffer)))
    (let* ((config-file
            (or file
                (expand-file-name "my_emacs/config.org" user-emacs-directory)))
           (declared '())
           (missing '()))
      (with-temp-buffer
        (insert-file-contents config-file)
        (goto-char (point-min))
        (while (re-search-forward "(use-package[[:space:]]+\\([^[:space:]\n)]+\\)"
                                  nil
                                  t)
          (save-excursion
            (goto-char (match-beginning 0))
            (beginning-of-line)
            (unless (looking-at "[[:space:]]*;")
              (push (match-string-no-properties 1) declared)))))
      (dolist (pkg package-selected-packages)
        (unless (member (symbol-name pkg) declared)
          (push pkg missing)))
      (with-current-buffer (get-buffer-create "missing use-package")
        (erase-buffer)
        (if missing
            (dolist (pkg (nreverse missing))
              (insert (symbol-name pkg) "\n"))
          (insert "No missing use-package declarations."))
        (display-buffer (current-buffer))))))

(provide 'my_misc)
;;; my_misc.el ends here
