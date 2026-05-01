# CLAUDE.md — my_emacs

Personal Emacs configuration by A. Lloyd Flanagan. This repo lives at
`~/.config/emacs/my_emacs/` and is loaded by `~/.config/emacs/init.el`.

---

## Architecture

This is a **literate configuration**: primary source lives in `.org` files which
are tangled into `.el` files via `org-babel-tangle`. **Never edit generated
`.el` files directly**; edit the corresponding `.org` file instead.

| Source (edit this) | Generated (do not edit) |
|--------------------|------------------------|
| `early-config.org` | `early-init.el` |
| `config.org` | `config.el` |

`init.el` is the exception — it is hand-written (not tangled) and its sole
purpose is to bootstrap `use-package`, load the latest `org-mode`, then tangle
and load `config.org` at startup.

### Boot sequence

1. `early-init.el` — GC tuning (`most-positive-fixnum` during startup, restored
   to 5 MiB after), frame geometry, gcc library path (macOS), sets
   `user-lisp-directory` to `<user-emacs-directory>/my_emacs/lisp` (Emacs 31+).
2. `init.el` — bootstraps `use-package` (`use-package-always-ensure t`), pins
   `org` to GNU ELPA, then calls `org-babel-load-file` on
   `<user-emacs-directory>/my_emacs/config.org`.
3. `config.org` (tangled to `config.el`) — all package declarations and
   settings.

---

## File Layout

```
my_emacs/
├── early-config.org         # Source for early-init.el
├── early-init.el            # GENERATED — do not edit
├── init.el                  # Hand-written bootstrap (not tangled)
├── config.org               # Source for config.el — main config
├── config.el                # GENERATED — do not edit
├── custom.el                # Written by M-x customize; not tangled
├── secrets.el               # Local secrets (not in VCS); required by config
├── my_emacs-autoloads.el    # GENERATED — autoloads for the my_emacs directory
├── Makefile                 # `make clean` removes *.elc files
├── todo.org                 # Project TODO list
├── packages.txt             # Snapshot of installed packages (output of helper script)
├── find_missing.sh          # Helper: find packages missing use-package declarations
├── get_used_packages.sh     # Helper: list packages referenced in config
├── find_gcc.el.bkp          # Backup of old dynamic gcc-path finder (not loaded)
├── CODE_OF_CONDUCT.md
├── LICENSE
├── README.md
├── lisp/
│   ├── misc.el              # Small utility functions
│   ├── hideshowvis.el       # Third-party: hide/show visualization
│   ├── .user-lisp-autoloads.el  # GENERATED — Emacs 31+ user-lisp autoloads
│   └── org-contrib/         # Git submodule: extra org-mode contrib packages
└── lisp/scripts/
    └── hello.lsp            # Scratch Lisp script
```

---

## Key Conventions

- **Lexical binding** is enabled in every `.el` file via `;;
  -*- lexical-binding: t -*-` as the first line. Maintain this.
- **`use-package`** is used for every third-party package declaration.
  `use-package-always-ensure t` is set globally in `init.el`, so `:ensure t` is
  implicit everywhere. Use `:ensure nil` explicitly for built-in packages.
- **`:vc` keyword** is used to install packages directly from GitHub when they
  are not on ELPA/MELPA (e.g. `batppuccin`, `winpulse`, `treesit-fold`).
- **`setopt`** (Emacs 29+) is preferred over `setq` for user options.
- **Indentation**: spaces only, no tabs (`indent-tabs-mode nil`).
- **Fill column**: 80 characters.
- **Font**: Fira Code, height 160. Must be installed on the system.
- **Formatting**: `elisp-autofmt` is configured and hooked into
  `emacs-lisp-mode` and `lisp-data-mode`. Run `C-c f` (buffer) or `C-c r`
  (region) to format. The config file `.elisp-autoformat` controls its settings.

---

## Package Manager

ELPA archives in priority order:

1. `gnu` — GNU ELPA (stable)
2. `nongnu` — NonGNU ELPA
3. `melpa` — MELPA (use with caution)

`org` is pinned to `gnu` to ensure the latest stable version.

---

---

## Notable Packages & Their Roles

| Package | Purpose |
|---------|---------|
| `org` (pinned gnu) | Org mode; drives literate config loading |
| `treemacs` | File-tree sidebar; starts on boot via `treemacs-start-on-boot` |
| `treemacs-icons-dired` | Treemacs icons in Dired buffers |
| `treemacs-magit` | Integrates Magit with Treemacs sidebar; pulls in `magit` as a dep |
| `lsp-mode` | Language Server Protocol client; hooked to `prog-mode` globally; `lsp-warn-no-matched-clients nil` |
| `lsp-treemacs` | Shows LSP info (errors, symbols) in Treemacs sidebar |
| `treesit-auto` | Auto-install and enable Tree-sitter grammars |
| `treesit-fold` | Code folding using Tree-sitter (installed via `:vc`) |
| `markdown-ts-mode` | Tree-sitter markdown mode (deferred) |
| `flycheck` | On-the-fly syntax checking; global mode; pinned to `nongnu` |
| `elpy` | Python IDE features; deferred until a Python file opens |
| `jinja2-mode` | Jinja2/Django template editing |
| `ob-ts-node` | Org Babel support for TypeScript via `ts-node` |
| `vterm` | Full-featured terminal emulator; scrollback 10 000 lines |
| `prettier` | Auto-format JS/TS/CSS buffers on save |
| `gptel` | LLM/AI integration; model `claude-sonnet-4-6`; reads `ANTHROPIC_API_KEY` from env |
| `gptel-fn-complete` | Function completion via gptel |
| `gptel-agent` | Agent-mode support for gptel |
| `vulpea` | Notes / knowledge base; auto-syncs DB |
| `vulpea-ui` + `vui` | Sidebar UI for vulpea (`C-c v s` to toggle) |
| `vulpea-journal` | Journal integration (`C-c j`) |
| `elisp-autofmt` | Auto-format Emacs Lisp code; hooked to `emacs-lisp-mode` and `lisp-data-mode` |
| `elisp-lint` | Lint Emacs Lisp files (deferred) |
| `smart-mode-line` + `smart-mode-line-powerline-theme` | Enhanced mode line with powerline theme |
| `amx` | Enhanced M-x history and completion (via ido) |
| `ido-completing-read+` | Extends ido completion to all `completing-read` calls |
| `which-key` | Displays available key bindings; built-in Emacs 30, `:ensure nil` |
| `editorconfig` | Read `.editorconfig` files |
| `whitespace-cleanup-mode` | Strip trailing whitespace on save |
| `page-break-lines` | Render `^L` (ctrl-L) as horizontal lines |
| `kirigami` | Enhanced code folding |
| `rainbow-delimiters` | Color-coded matching delimiters in `prog-mode` |
| `winpulse` | Pulse/highlight active window (installed via `:vc`) |
| `batppuccin` | Catppuccin-based color theme (installed via `:vc`); latte variant active |
| `terraform-mode` | Terraform/HCL editing; indent level 4 |
| `web-mode` | Multi-language HTML templates; Django engine default |
| `shfmt` | Shell script formatter (`C-c C-f` in `sh-mode`) |
| `org-beautify-theme` | Visual enhancements for Org mode |
| `ox-gfm` | Export Org files to GitHub-Flavored Markdown |
| `xkcd` | Browse xkcd comics inside Emacs |

---

## Python Tooling

mise manages uv; uv creates `.venv` in each project. `python-shell-exec-path`
is set to `./.venv/bin` so Emacs resolves `python3` from the project venv.
If Python features misbehave, ensure `uv sync` has been run in the project root.

---

## Secrets

`secrets.el` is required by `config.org` and must exist locally. It holds
variables that must not be committed (API keys, passwords, etc.). It is
intentionally absent from VCS. Create it with at minimum:

```emacs-lisp
;; -*- lexical-binding: t -*-
;;; secrets.el --- local secrets
(provide 'secrets)
;;; secrets.el ends here
```

The `gptel` package reads `ANTHROPIC_API_KEY` from the environment.

---

## Build / Maintenance

```bash
make clean        # delete all *.elc byte-compiled files (preserves source)
make list-targets # list available Makefile targets
```

To regenerate `early-init.el` after editing `early-config.org`, open the `.org`
file in Emacs and run `M-x org-babel-tangle`. `config.el` is regenerated
automatically on each Emacs startup via `org-babel-load-file` in `init.el`.

---

## Planned / Open Work (todo.org)

- Set up GitHub Actions to run startup-smoke tests.
- Create an issue template.
- Write `CONTRIBUTING.md`.
