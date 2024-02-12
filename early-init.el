;; HACK Work around native compilation on macOS failing with 'ld: library not
;; found for -lemutls_w'.
;; https://github.com/d12frosted/homebrew-emacs-plus/issues/554

(when (equal system-type 'darwin)
  (setenv "LIBRARY_PATH"
          (string-join '("/opt/homebrew/opt/gcc/lib/gcc/13"
                         "/opt/homebrew/opt/libgccjit/lib/gcc/13"
                         "/opt/homebrew/Cellar/gcc/13.2.0/lib/gcc/current/gcc/aarch64-apple-darwin23/13")
                       ":")))
