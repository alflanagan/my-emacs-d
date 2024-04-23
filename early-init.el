;;; early-init --- Set config as early as possible -*- lexical-binding: t -*-

;;; Commentary:
;; HACK Work around native compilation on macOS failing with 'ld: library not
;; found for -lemutls_w'.
;; https://github.com/d12frosted/homebrew-emacs-plus/issues/554
;; also setup of options which the initial frame shoud obey

;;; Code:
(when (equal system-type 'darwin)
  (setenv "LIBRARY_PATH"
          (string-join '("/opt/homebrew/opt/gcc/lib/gcc/13"
                         "/opt/homebrew/opt/libgccjit/lib/gcc/13"
                         "/opt/homebrew/Cellar/gcc/13.2.0/lib/gcc/current/gcc/aarch64-apple-darwin23/13")
                       ":"))
  (modify-all-frames-parameters
   '((font . "-*-FiraCode Nerd Font Mono-regular-normal-normal-*-16-*-*-*-m-0-iso10646-1")
     (ns-transparent-titlebar . t)
     (ns-appearance . dark))))

;; need to set this in early-init to set for intitial frame
(modify-all-frames-parameters '((horizontal-scroll-bars) (vertical-scroll-bars) (width . 180) (height . 60)))

;; set this here so it gets applied to load of init.el (we hope)
(setq load-prefer-newer t) ;; IMHO the default value of this being nil is nuts

;;; early-init.el ends here
