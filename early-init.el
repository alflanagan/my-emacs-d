;;; early-init --- Set config as early as possible -*- lexical-binding: t -*-

;;; Commentary:
;; HACK Work around native compilation on macOS failing with 'ld: library not
;; found for -lemutls_w'.
;; https://github.com/d12frosted/homebrew-emacs-plus/issues/554
;; also setup of options which the initial frame shoud obey

;;; Code:


;; garbage collection settings
;; see https://emacsredux.com/blog/2025/03/28/speed-up-emacs-startup-by-tweaking-the-gc-settings/
;; for a good discussion

;; Temporarily increase GC threshold during startup
;; This saves about 1.8seconds on startup time
(setq gc-cons-threshold most-positive-fixnum)

;; Restore to normal value after startup
(add-hook 'emacs-startup-hook
    ;; 800K is emacs startup default, need to try higher settings to see if they impove performance
    (lambda () (setq gc-cons-threshold 800000))) ;; (* 5 1024 1024))))

(setopt garbage-collection-messages t) ;; show GC messages for debugging


(if (equal system-type 'darwin)
    (progn
      (setenv
       "LIBRARY_PATH"
       (string-join
        '("/opt/homebrew/opt/gcc/lib/gcc/13"
          "/opt/homebrew/opt/libgccjit/lib/gcc/13"
          "/opt/homebrew/Cellar/gcc/13.2.0/lib/gcc/current/gcc/aarch64-apple-darwin23/13")
        ":"))
      (modify-all-frames-parameters
       '((ns-transparent-titlebar . t) (ns-appearance . dark)))
      (set-face-attribute
       'default nil
       :family "FiraCode Nerd Font Mono" ; -regular-normal-normal-*-16-*-*-*-m-0-iso10646-1")
       :height 160
       :weight 'normal
       :slant 'normal
       :width 'normal))
  ;; mac gets fancy FiraCode Nerd, everybody else just Fira Code.
  ;; someday I may get around to setting up nerd font for linux too
  (set-face-attribute 'default nil
                      :family "Fira Code"
                      :height 160
                      :weight 'normal
                      :slant 'normal
                      :width 'normal))

;; need to set this in early-init to set for intitial frame
(setopt initial-frame-alist
      '((horizontal-scroll-bars)
        (vertical-scroll-bars)
        (width . 150)
        (height . 40)))

;; set this here so it gets applied to load of init.el (we hope)
;; OTOH init.elc shouldn't exist (check for it??)
(setq load-prefer-newer t) ;; IMHO the default value of this being nil is nuts

;;; early-init.el ends here
