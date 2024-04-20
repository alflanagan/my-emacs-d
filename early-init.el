;;; early-init --- Set config as early as possible -*- lexical-binding: t -*-

;;; Commentary:
;; HACK Work around native compilation on macOS failing with 'ld: library not
;; found for -lemutls_w'.
;; https://github.com/d12frosted/homebrew-emacs-plus/issues/554

;;; Code:
(when (equal system-type 'darwin)
  (setenv "LIBRARY_PATH"
          (string-join '("/opt/homebrew/opt/gcc/lib/gcc/13"
                         "/opt/homebrew/opt/libgccjit/lib/gcc/13"
                         "/opt/homebrew/Cellar/gcc/13.2.0/lib/gcc/current/gcc/aarch64-apple-darwin23/13")
                       ":"))
  ;; need to set this in early-init to set for intitial frame
  (modify-all-frames-parameters
   '((horizontal-scroll-bars t)
     (vertical-scroll-bars)
     (font . "-*-FiraCode Nerd Font Mono-regular-normal-normal-*-16-*-*-*-m-0-iso10646-1"))))

;; set this here so it gets applied to load of init.el (we hope)
(setq load-prefer-newer t) ;; IMHO the default value of this being nil is nuts

;; new frame get these params, including initial frame
(setq default-frame-alist '((width . 180) (height . 60) (vertical-scroll-bars)))

;;; early-init.el ends here
