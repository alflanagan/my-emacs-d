;; -*- lexical-binding: t -*-

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
    (lambda () (setq gc-cons-threshold (* 5 1024 1024))))

(setopt garbage-collection-messages t) ;; show GC messages for debugging

(if (equal system-type 'darwin)
       (setenv
        "LIBRARY_PATH"
        (string-join
         '("/opt/homebrew/opt/gcc/lib/gcc/13"
           "/opt/homebrew/opt/libgccjit/lib/gcc/13"
           "/opt/homebrew/Cellar/gcc/13.2.0/lib/gcc/current/gcc/aarch64-apple-darwin23/13")
         ":")))

(if (equal system-type 'darwin)
  (set-face-attribute
     'default nil
     :family "FiraCode Nerd Font Mono"
     :height 160
     :weight 'normal
     :slant 'normal
     :width 'normal)
  (set-face-attribute
     'default nil
     :family "Fira Code"
     :height 160
     :weight 'normal
     :slant 'normal
     :width 'normal))

(setopt initial-frame-alist
      '((horizontal-scroll-bars)
        (vertical-scroll-bars)
        (width . 150)
        (height . 40)))

(setq load-prefer-newer t)

;;; early-init.el ends here
