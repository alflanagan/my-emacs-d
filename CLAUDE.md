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
- **`setopt`** (Emacs 29+) is preferred over `setq` for user options.
- **Indentation**: spaces only, no tabs (`indent-tabs-mode nil`).
- **Fill column**: 80 characters.
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
| `treemacs` | File-tree sidebar; starts on boot |
| `lsp-mode` | Language Server Protocol client; hooked to `prog-mode` |
| `treesit-auto` | Auto-install and enable Tree-sitter grammars |
| `flycheck` | On-the-fly syntax checking; global mode |
| `elpy` | Python IDE features; deferred until a Python file opens |
| `gptel` | LLM/AI integration; configured for Claude via `ANTHROPIC_API_KEY` |
| `vulpea` | Notes / knowledge base; auto-syncs DB |
| `magit` | Git interface |
| `elisp-autofmt` | Auto-format Emacs Lisp code |
| `smart-mode-line` | Enhanced mode line with powerline theme |
| `amx` + `ido-completing-read+` | M-x and minibuffer completion via ido |
| `editorconfig` | Read `.editorconfig` files |
| `whitespace-cleanup-mode` | Strip trailing whitespace on save |
| `terraform-mode` | Terraform/HCL editing |
| `web-mode` | Multi-language HTML templates |

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
