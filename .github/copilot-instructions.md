Repository: my_emacs — Copilot instructions

Purpose

Short, actionable guidance for Copilot sessions working in this repository.
Pulls important, repo-specific rules from README.md and CLAUDE.md.

Build / test / lint commands

- make clean
  - Removes compiled .elc files (preserves source).
  - Command: make clean

- make list-targets
  - Lists Makefile targets (useful to discover available tasks).
  - Command: make list-targets

- make smoke
  - Runs an Emacs batch "startup smoke" using the repository's init.el. Useful for
    validating startup/tangle behavior.
  - Command: make smoke
  - Equivalent invocation shown in Makefile:
    emacs -Q --batch \
      --eval '(setq user-emacs-directory "~/.config/emacs/")' \
      --eval '(setq init-file-debug t)' \
      -L . -l init.el

- Tests
  - No test suite or test files were detected in the repository root. The project
    references the buttercup test framework but does not include a configured
    test harness.
  - If tests are added, run them in Emacs batch mode (load test files or test
    directory and invoke buttercup's runner). See buttercup README for precise
    batch invocation.

High-level architecture (big picture)

- Literate configuration: primary sources are .org files (early-config.org,
  config.org). Those are tangled to generate the corresponding .el files
  (early-init.el, config.el). Do NOT edit generated .el files; edit the .org
  sources instead.

- Boot sequence:
  1) early-init.el (generated from early-config.org) — runs before packages are
     loaded; tuned startup settings (GC, frame geometry, gcc path on macOS, sets
     user-lisp-directory for Emacs 31+).
  2) init.el (hand-written) — bootstraps package manager and pins org; calls
     org-babel-load-file on config.org.
  3) config.org (tangled to config.el) — main package declarations and settings.

- Submodules: The repo includes org-contrib as a submodule. After cloning, run:
  git submodule init && git submodule update

Key conventions and repo-specific rules

- Never edit generated .el files. Edit the corresponding .org files and tangle.
- Files that are generated: early-init.el (from early-config.org) and config.el
  (from config.org).
- First line in every .el file: lexical binding header is required:
  `;; -*- lexical-binding: t -*-`
- use-package is used for third-party packages. The repo expects
  use-package-always-ensure t in init.el; use :ensure nil for built-ins.
- Use :vc in use-package declarations when installing directly from GitHub.
- :bind values use plain quoted symbols ('my-fn), not #'my-fn.
- Prefer setopt (Emacs 29+) over setq for user options where used.
- Indentation: spaces only; indent-tabs-mode nil. Fill column: 80.
- elisp-autofmt is configured for formatting elisp; run C-c f (buffer) or C-c r
  (region) where applicable. The file .elisp-autoformat contains formatter
  settings.
- Auto-chmod: saving a file with a shebang will run chmod +x via
  after-save-hook.
- LSP I/O: read-process-output-max is set to 4 MB in config; avoid reducing
  this.
- my-secrets.el is required by config.org and is intentionally absent from VCS.
  Minimal my-secrets.el stub must provide (provide 'my-secrets).

AI / assistant configs found and incorporated

- CLAUDE.md present — its architecture and conventions are authoritative and
  have been incorporated into this instructions file.
- .claude/settings.local.json exists but is local configuration; no changes
  included here.

Notes for Copilot sessions

- Prefer editing .org source files, not tangled .el files. When asked to change
  behavior loaded at startup, modify the appropriate .org and either tangle (via
  org-babel-tangle) or restart Emacs so init.el re-tangles config.org.
- Use Makefile targets for quick verification: make smoke to test startup, make
  clean to remove compiled objects.
- If adding tests, add a small test harness under a test/ or lisp/tests/
  directory and document the Emacs batch command to run them.

Summary

This file condenses the repo's build/test commands, its literate config
architecture, and the key coding/config conventions Copilot should follow when
producing edits or suggestions.
