;;; misc.el -- some generally-useful functions   -*- lexical-binding: t -*-

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

(provide 'misc)
;;; misc.el ends here
