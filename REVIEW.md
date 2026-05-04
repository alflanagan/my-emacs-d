# Code Review: `early-config.org` and `config.org`

Below is a prioritized review. Findings are grouped as bugs (likely or
definite), correctness/robustness issues, and stylistic improvements.

## Bugs & correctness issues

- [x] **1. `(load custom-file)` is unguarded — fatal if `custom.el` is absent**
  (`config.org:86`). On a fresh checkout this throws and aborts init. Use
  `(load custom-file 'noerror 'nomessage)` or wrap in `file-exists-p`.

- [x] **2. `secrets` is the name of a built-in Emacs library** (`config.org:104`).
  `(require 'secrets)` resolves whichever copy `load-path` finds first. Your
  `my_emacs/` directory is on the front of `load-path` so it currently works,
  but it's a footgun: any future reordering and you load the freedesktop
  SecretService client instead. Rename your local file (e.g.
  `my-secrets.el` → `(require 'my-secrets)`).

- [x] **3. `load-path-ignore-regexp` is not a real variable** (`config.org:64`).
  The intended one is `load-path-filter-function` (Emacs 30+), or you can
  filter by mutating `load-path` directly. As written, this `setopt`
  silently does nothing — the `.github` directory is still being scanned.
  (You may also be thinking of how `package` filters subdirs; that's
  `package-load-list`/`package-quickstart-refresh`.)

- [x] **4. `elpy-enable` is wired only to `python-mode`, not `python-ts-mode`**
  (`config.org:511-513`). With `treesit-auto` enabled and a Tree-sitter
  grammar present, opening a `.py` file lands in `python-ts-mode` and the
  `:before` advice on `python-mode` never fires — elpy never enables.
  Either advise both, or use
  `:hook ((python-mode python-ts-mode) . elpy-enable)`.

- [x] **5. `lsp-warn-no-matched-clients` is set outside any package config**
  (`config.org:646`). It's at top level after the `lsp-mode` `use-package`,
  so it sets the variable before `lsp-mode` is loaded. That works in
  practice, but belongs inside the `:custom` block of `lsp-mode` for
  cohesion (and so a deferred load doesn't reset it via defcustom default).

- [x] **6. `treemacs-start-on-boot` is called inside `:config` of a `:defer t`
  block** (`config.org:384`). With `:defer t`, `:config` only runs when
  something triggers `treemacs`, so the "start on boot" call never
  executes at startup. Move the `(treemacs-start-on-boot)` call to
  `:init`, or drop `:defer t` for treemacs since you actually want it
  eager. (This matches your earlier complaint about the sidebar not
  appearing on start.)

- [x] **7. `gptel` reads `ANTHROPIC_API_KEY` at load time, not request time**
  (`config.org:726`). `(getenv ...)` is evaluated when the `:config` block
  runs. If the env var isn't in Emacs's environment at startup (common on
  macOS GUI Emacs launched from Finder/Dock), the key is `nil` forever.
  Pass a function so it's resolved on demand:

  ```emacs-lisp
  :key (lambda () (getenv "ANTHROPIC_API_KEY"))
  ```

- [x] **8. `magit` is undeclared but depended on** (`config.org:423`).
  `treemacs-magit :after (treemacs magit)` — `magit` is pulled in
  transitively by `treemacs-magit`, but you have no `(use-package magit
  ...)` to pin/configure it. If you use magit you should declare it
  explicitly; if you don't, drop `treemacs-magit`.

- [x] **9. `web-mode` has no entry for `.html`** (`config.org:558-569`) but
  `web-mode-engines-alist` maps `\\.html\\'` to django. Django template
  `.html` files therefore never open in web-mode at all —
  html-mode/mhtml-mode handle them. Either add
  `("\\.html\\'" . web-mode)` or accept that engine-detection is dead code.

- [x] **10. `markdown-mode` `:mode` ordering is fragile** (`config.org:460-464`).
  `:mode` prepends to `auto-mode-alist`; the last entry added wins for any
  matching pattern. With `("README\\.md\\'" . gfm-mode)` listed first and
  `("\\.md\\'" . markdown-mode)` last, the broader `\\.md\\'` ends up in
  front and shadows the README rule. Reverse the order so the more
  specific `README\\.md\\'` pattern is registered last.

- [x] **11. `trash-directory` is hardcoded to `~/.Trash`** (`config.org:187`).
  Mac-only. On Linux, `delete-by-moving-to-trash` already speaks XDG; the
  explicit setting breaks that. Gate on `system-type`, or just unset it on
  non-darwin.

- [x] **12. Hard-coded gcc cellar path** (`early-config.org:71`). You already
  note this in the comment. The `15.2.0_1` version string will break on
  every brew gcc upgrade. Resurrect or rewrite `find_gcc.el.bkp` and use a
  dynamic discovery function — something like:

  ```emacs-lisp
  (car (file-expand-wildcards
        "/opt/homebrew/Cellar/gcc/*/lib/gcc/current/gcc/*/*"))
  ```

- [x] **13. `inhibit-startup-echo-area-message` is hardcoded to a username
  string** (`config.org:155`). This is a known Emacs quirk (the value
  must be a literal so it can't be programmatically set), but consider
  `(setq inhibit-startup-echo-area-message (user-login-name))` — that
  doesn't actually work for the silencing magic, but you could
  conditionalize on the current user, or just drop it (the message is
  harmless).

- [x] **14. `lsp-mode :hook prog-mode` is too broad.** It tries to start LSP in
  `emacs-lisp-mode`, `lisp-data-mode`, `sh-mode`, etc.
  `lsp-warn-no-matched-clients nil` mutes the noise but you still pay the
  lookup cost on every prog buffer. Hook only the languages where you
  have a server (`(python-ts-mode python-mode go-mode rust-mode ...) .
  lsp-deferred`), and prefer `lsp-deferred` over `lsp` so it waits until
  the buffer settles.

## Robustness improvements

- [ ] **15. Path construction.** Both files use `(concat user-emacs-directory
  "...")`. `user-emacs-directory` is documented to end in `/` but
  `expand-file-name` is the safe form:

  ```emacs-lisp
  (expand-file-name "my_emacs/lisp" user-emacs-directory)
  ```

  Apply at `early-config.org:124`, `config.org:54`, and `config.org:63`.

- [ ] **16. `warning-suppress-log-types` only silences the log buffer.** If you
  also want to stop the popup, set `warning-suppress-types` (without
  `-log`).

- [ ] **17. Spurious `defvar` for `package-menu-hide-low-priority`**
  (`config.org:136`). It's already a defcustom in `package.el`. The
  `defvar` is redundant; the byte-compiler warning, if any, comes from
  referring to it before `package` is loaded — but `package` is loaded by
  the time `early-init`/`init` finishes. Drop it.

- [ ] **18. `:custom` not used for `gptel`** (`config.org:721-725`). Two `setq`
  calls in `:config` should be `:custom (gptel-model 'claude-sonnet-4-6)
  (gptel-backend ...)` — same applies to the `setq
  inhibit-startup-echo-area-message` (must be `setq` for that one, in
  fact, due to the literal-string magic).

- [x] **19. `which-key` `:ensure nil` always.** If you ever drop back to Emacs
  29, the package isn't installed. Gate on version: `:ensure (<
  emacs-major-version 30)`.

- [ ] **20. `package-install-upgrade-built-in t` is global and aggressive.** It
  will silently upgrade every built-in package on `M-x
  package-list-packages`. The comment notes magit needs this — better to
  set it per-package via `package-pinned-packages` or the use-package
  `:pin` mechanism, leaving the default off for everything else.

- [ ] **21. GC threshold restored to 5 MiB** (`early-config.org:52`). Modern
  setups commonly run 50–100 MiB to avoid pause stutter. Worth
  experimenting with `(* 50 1024 1024)` and watching `M-x gcs-done` over
  time.

- [ ] **22. `auto-save-visited-mode` is on globally** (`config.org:169`).
  Combined with `kill-buffer-delete-auto-save-files t`, this means every
  buffer gets a real on-disk save at intervals. That can fight with
  version control hooks (pre-commit formatters running on save, etc.).
  Not wrong, but worth a comment explaining intent.

## Style & consistency

- [ ] **23. Typos.** `early-config.org:47` "impove" → "improve";
  `early-config.org:77` "intitial" → "initial"; `config.org:232`
  "programming  mode." (double space).

- [ ] **24. Mixed `setq`/`setopt`.** Per your CLAUDE.md you prefer `setopt`, but
  several call sites still use `setq` for defcustoms (`config.org:155`,
  `444`, `646`, `721`, `722`). Where the variable is a defcustom and you
  don't need the literal-string trick, switch to `setopt` or move into a
  `:custom` block.

- [ ] **25. `:bind` with `#'`.** `:bind ("C-c v s" . #'vulpea-ui-sidebar-toggle)`
  (`config.org:686`) — `:bind` expects a symbol; `#'` is harmless but
  inconsistent with the rest of the file (most others use bare symbols or
  `#'` mixed). Pick one and stick with it.

- [ ] **26. TODOs that have lingered** (`early-config.org:79`, `109`). The
  auto-tangle-on-save TODO is straightforward — add
  `add-file-local-variable` or a per-file hook:

  ```emacs-lisp
  ;; Local Variables:
  ;; eval: (add-hook 'after-save-hook #'org-babel-tangle nil t)
  ;; End:
  ```

- [ ] **27. `kirigami` declared with no config** (`config.org:332`). Either
  configure it (you have `treesit-fold` doing similar work) or remove it.

- [ ] **28. Commented-out load-path block** (`config.org:67-72`) and
  `find_gcc.el.bkp` reference dead code. If the version-gated logic is
  needed, implement it; otherwise delete.

- [ ] **29. `treemacs` `:custom` has `treemacs-follow-after-init nil`**
  (`config.org:391`) but you also enable `treemacs-follow-mode t`. The
  comment "follow mode makes it difficult to find files that are not
  on-screen" suggests you actually don't want follow-mode. Reconcile.

- [ ] **30. `terraform-mode` defines `my-terraform-mode-init` in `:config` and
  hooks it** (`config.org:701-704`). This is fine but the function is
  global; prefix the name (`my/terraform-mode-init`) per Elisp convention
  to avoid conflicts.

## Quick wins (small, high value)

The top five I'd fix today:

- [x] Wrap `(load custom-file 'noerror 'nomessage)` — prevents fresh-clone
  init failure. (→ item 1)
- [x] Rename `secrets.el` to avoid collision with built-in. (→ item 2)
- [x] Fix the `gptel` API-key resolution (lambda). (→ item 7)
- [ ] Move `(treemacs-start-on-boot)` to `:init` so the sidebar actually
  appears. (→ item 6)
- [x] Drop or fix `load-path-ignore-regexp` — it currently does nothing.
  (→ item 3)
