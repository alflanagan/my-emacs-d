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

1. `early-init.el` — GC tuning, frame geometry, gcc library path (macOS), sets
   `user-lisp-directory` to `lisp/` (Emacs 31+).
2. `init.el` — bootstraps `use-package`, pins `org` to GNU ELPA, then calls
   `org-babel-load-file` on `config.org`.
3. `config.org` (tangled to `config.el`) — all package declarations and
   settings.

---

## File Layout

```
my_emacs/
├── early-config.org      # Source for early-init.el
├── early-init.el         # GENERATED — do not edit
├── init.el               # Hand-written bootstrap (not tangled)
├── config.org            # Source for config.el — main config
├── config.el             # GENERATED — do not edit
├── custom.el             # Written by M-x customize; not tangled
├── secrets.el            # Local secrets (not in VCS); required by config
├── tssetup.el            # TypeScript / tree-sitter helpers
├── Makefile              # `make clean` removes *.elc files
├── todo.org              # Project TODO list
├── lisp/
│   ├── misc.el           # Small utility functions
│   ├── site.linux.el     # Linux-specific overrides
│   ├── site.macos.el     # macOS-specific overrides
│   ├── hideshowvis.el    # Third-party: hide/show visualization
│   ├── emacs-which-key/  # Git submodule: which-key package
│   ├── no-littering/     # Git submodule: keep ~/.emacs.d clean
│   └── org-contrib/      # Git submodule: extra org-mode contrib packages
└── lisp/scripts/
    └── hello.lsp         # Scratch Lisp script
```

---

## Key Conventions

- **Lexical binding** is enabled in every `.el` file via `;;
  -*- lexical-binding: t -*-` as the first line. Maintain this.
- **`use-package`** is used for every third-party package declaration.
  `use-package-always-ensure t` is set, so `:ensure t` is implicit.
- **`:vc` keyword** is used to install packages directly from GitHub when they
  are not on ELPA/MELPA (e.g. `batppuccin`, `winpulse`, `treesit-fold`).
- **`setopt`** (Emacs 29+) is preferred over `setq` for user options.
- **Indentation**: spaces only, no tabs (`indent-tabs-mode nil`).
- **Fill column**: 80 characters.
- **Font**: Fira Code, height 160. Must be installed on the system.
- **Formatting**: `elisp-autofmt` is configured and hooked into
  `emacs-lisp-mode`. Run `C-c f` (buffer) or `C-c r` (region) to format.
  The config file `.elisp-autoformat` controls its settings.

---

## Package Manager

ELPA archives in priority order:

1. `gnu` — GNU ELPA (stable)
2. `nongnu` — NonGNU ELPA
3. `melpa` — MELPA (use with caution)

`org` is pinned to `gnu` to ensure the latest stable version.

---

## Platform Support

Two site-specific files provide platform overrides loaded after the main config:

- `lisp/site.macos.el` — macOS paths (Homebrew, pyenv, nvm, cargo, Go).
  Sets `exec-path` and `projectile-project-search-path`.
- `lisp/site.linux.el` — Linux paths (pyenv shims).

These files are not auto-loaded; they must be explicitly required where needed.

---

## Notable Packages & Their Roles

| Package | Purpose |
|---------|---------|
| `org` (pinned gnu) | Org mode; drives literate config loading |
| `treemacs` | File-tree sidebar; starts on boot via `treemacs-start-on-boot` |
| `treemacs-icons-dired` | Treemacs icons in Dired buffers |
| `treemacs-magit` | Integrates Magit with Treemacs sidebar |
| `lsp-mode` | Language Server Protocol client; hooked to `prog-mode` |
| `lsp-treemacs` | Shows LSP info (errors, symbols) in Treemacs sidebar |
| `treesit-auto` | Auto-install and enable Tree-sitter grammars |
| `treesit-fold` | Code folding using Tree-sitter (installed via `:vc`) |
| `flycheck` | On-the-fly syntax checking; global mode; pinned to `nongnu` |
| `elpy` | Python IDE features; deferred until a Python file opens |
| `jinja2-mode` | Jinja2/Django template editing |
| `gptel` | LLM/AI integration; model `claude-sonnet-4-20250514`; reads `ANTHROPIC_API_KEY` from env |
| `gptel-fn-complete` | Function completion via gptel |
| `gptel-agent` | Agent-mode support for gptel |
| `vulpea` | Notes / knowledge base; auto-syncs DB |
| `vulpea-ui` + `vui` | Sidebar UI for vulpea (`C-c v s` to toggle) |
| `vulpea-journal` | Journal integration (`C-c j`) |
| `magit` | Git interface |
| `elisp-autofmt` | Auto-format Emacs Lisp code |
| `elisp-lint` | Lint Emacs Lisp files (deferred) |
| `smart-mode-line` | Enhanced mode line with powerline theme |
| `amx` + `ido-completing-read+` | M-x and minibuffer completion via ido |
| `editorconfig` | Read `.editorconfig` files |
| `whitespace-cleanup-mode` | Strip trailing whitespace on save |
| `page-break-lines` | Render `^L` (ctrl-L) as horizontal lines |
| `kirigami` | Enhanced code folding |
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

The config assumes `uv` and `mise` are installed and in use. `python-shell-exec-path`
is set to `./.venv/bin`, and the interpreter path points into mise's install tree.
If Python features misbehave on a new machine, check that both tools are present and
that `mise` has provisioned an interpreter.

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
