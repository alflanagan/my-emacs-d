;;; useful function suggested by ChatGPT

(defun replace-alist-cdr (key new-value alist)
  "Replace the cdr of a key in an alist with a new value."
  (let ((pair (assoc key alist)))
    (when pair
      (setcdr pair new-value))))
