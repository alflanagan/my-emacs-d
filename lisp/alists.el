;; -*- lexical-binding: true -*-

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


;;; useful function suggested by ChatGPT

(defun alist-replace-cdr (key new-value alist)
  "Replace the cdr of a key in an alist with a new value."
  (let ((pair (assoc key alist)))
    (when pair
      (setcdr pair new-value))))


(defun alist-key-add-or-replace (key value alist)
  "If KEY is in ALIST, set to VALUE. Otherwise push (KEY . VALUE) to ALIST"
  (let ((pair (assoc key alist)))
    (if pair
        (setcdr pair value)
      (push (cons key value) alist))))

(provide 'alf-alists)
