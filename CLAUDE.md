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
   to 50 MiB after), frame geometry, gcc library path (macOS), sets
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
├── my-secrets.el            # Local secrets (not in VCS); required by config
├── my_emacs-autoloads.el    # GENERATED — autoloads for the my_emacs directory
├── Makefile                 # `make clean` removes *.elc files
├── todo.org                 # Project TODO list
├── packages.txt             # Snapshot of installed packages (output of helper script)
├── find_missing.sh          # Helper: find packages missing use-package declarations
├── get_used_packages.sh     # Helper: list packages referenced in config
├── find_gcc.el.bkp          # Backup of old dynamic gcc-path finder (not loaded)
├── .dir-locals.el           # Directory-local variables (e.g. gfm-mode for .md)
├── CODE_OF_CONDUCT.md
├── LICENSE
├── README.md
├── REVIEW.md                # Ongoing code-review checklist
├── lisp/
│   ├── my_misc.el           # Small utility functions
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
  are not on ELPA/MELPA (e.g. `batppuccin`, `winpulse`, `treesit-fold`,
  `mermaid-ts-mode`).
- **`:bind` values** use plain quoted symbols (`'my-fn`), not `#'my-fn`.
- **`setopt`** (Emacs 29+) is preferred over `setq` for user options.
- **Indentation**: spaces only, no tabs (`indent-tabs-mode nil`).
- **Fill column**: 80 characters.
- **Font**: Fira Code, height 160. Must be installed on the system.
- **Formatting**: `elisp-autofmt` is configured and hooked into
  `emacs-lisp-mode` and `lisp-data-mode`. Run `C-c f` (buffer) or `C-c r`
  (region) to format. The config file `.elisp-autoformat` controls its settings.
- **Auto-chmod**: `executable-make-buffer-file-executable-if-script-p` is
  hooked into `after-save-hook`, so saving a file with a shebang line
  automatically runs `chmod +x` on it.
- **LSP I/O**: `read-process-output-max` is set to 4 MB (from 64 KB default)
  to reduce read-call overhead with verbose LSP servers like clangd.
- **Bidi**: `bidi-display-reordering` and `bidi-paragraph-direction` are
  forced to `left-to-right` and `bidi-inhibit-bpa` is `t`. Do not change
  these if RTL language support is ever needed.
- **Regexp builder**: `reb-re-syntax` is `'string`, so `re-builder` (`M-x
  re-builder`) uses string (non-escaped) regexp syntax.
- **ffap**: `ffap-machine-p-known` is `'reject` so `find-file-at-point`
  never interprets filenames as URLs.

---

## Package Manager

ELPA archives in priority order:

1. `gnu` — GNU ELPA (stable)
2. `nongnu` — NonGNU ELPA
3. `melpa` — MELPA (use with caution)

`org` is pinned to `gnu` to ensure the latest stable version.

---

## Notable Packages & Their Roles

| Package                                               | Purpose                                                               |
|-------------------------------------------------------|-----------------------------------------------------------------------|
| `org` (pinned gnu)                                    | Org mode; drives literate config loading                              |
| `treemacs`                                            | File-tree sidebar; starts on boot via `treemacs-start-on-boot`        |
| `treemacs-icons-dired`                                | Treemacs icons in Dired buffers                                       |
| `transient`                                           | Pinned to MELPA; explicit dep to satisfy magit's version requirement  |
| `treemacs-magit`                                      | Integrates Magit with Treemacs sidebar; pulls in `magit` as a dep     |
| `lsp-mode`                                            | Language Server Protocol client                                       |
| `lsp-treemacs`                                        | Shows LSP info (errors, symbols) in Treemacs sidebar                  |
| `treesit-auto`                                        | Auto-install and enable Tree-sitter grammars                          |
| `treesit-fold`                                        | Code folding using Tree-sitter (installed via `:vc`)                  |
| `markdown-mode`                                       | Markdown editing; `gfm-mode` for all `.md` and `.markdown` files      |
| `flycheck`                                            | On-the-fly syntax checking; global mode; pinned to `nongnu`           |
| `flycheck-aspell`                                     | Spell-checking via aspell in flycheck                                 |
| `uv-mode`                                             | Activates uv virtualenv in python-mode / python-ts-mode buffers       |
| `vterm`                                               | Full-featured terminal emulator; scrollback 10 000 lines              |
| `prettier`                                            | Auto-format JS/TS/CSS/HTML buffers on save (hook-based)               |
| `gptel`                                               | LLM/AI integration (currently disabled/commented out pending fixes)   |
| `gptel-fn-complete`                                   | Function completion via gptel (currently disabled)                    |
| `gptel-agent`                                         | Agent-mode support for gptel (currently disabled)                     |
| `magit`                                               | Git interface; loaded deferred                                         |
| `vulpea`                                              | Notes / knowledge base; auto-syncs DB                                 |
| `vui`                                                 | Low-level UI primitives used by `vulpea-ui`                           |
| `vulpea-ui`                                           | Sidebar UI for vulpea (`C-c v s` to toggle)                           |
| `vulpea-journal`                                      | Journal integration (`C-c j`)                                         |
| `elisp-autofmt`                                       | Auto-format Emacs Lisp code.                                          |
| `elisp-lint`                                          | Lint Emacs Lisp files (deferred)                                      |
| `buttercup`                                           | BDD-style testing framework for Emacs Lisp                            |
| `eask-mode`                                           | Major mode for Eask package build files                               |
| `eldoc-eask`                                          | Eldoc support for Eask files                                          |
| `flycheck-eask`                                       | Flycheck checker for Eask files                                       |
| `smart-mode-line` + `smart-mode-line-powerline-theme` | Enhanced mode line with powerline theme                               |
| `amx`                                                 | Enhanced M-x history and completion (via ido)                         |
| `ido-completing-read+`                                | Extends ido completion to all `completing-read` calls                 |
| `which-key`                                           | Displays available key bindings.                                      |
| `editorconfig`                                        | Read `.editorconfig` files                                            |
| `whitespace-cleanup-mode`                             | Strip trailing whitespace on save                                     |
| `page-break-lines`                                    | Render `^L` (ctrl-L) as horizontal lines                              |
| `rainbow-delimiters`                                  | Color-coded matching delimiters in `prog-mode`                        |
| ~~`kirigami`                                          | Code folding for buffers without active tree-sitter parsers.~~        |
| `winpulse`                                            | Pulse/highlight active window (installed via `:vc`)                   |
| `batppuccin`                                          | Catppuccin-based color theme; latte variant active.                   |
| `cmake-mode`                                          | Major mode for `CMakeLists.txt` and `.cmake` files                    |
| `cmake-project`                                       | Integrates CMake projects with `project.el`                           |
| `cmake-ide`                                           | Wires `compile_commands.json` into IDE features.                      |
| `terraform-mode`                                      | Terraform/HCL editing; indent level 4                                 |
| `web-mode`                                            | Multi-language HTML templates; Django engine default; handles `.html` |
| `mermaid-ts-mode`                                     | Mermaid diagram editing via Tree-sitter (`:vc` from GitHub); note:    |
|                                                       | the treesitter parser is incomplete and seeking a new maintainer       |
| `ob-mermaid`                                          | Org Babel support for Mermaid diagram blocks; loaded `:after org`     |
| `org-auto-tangle`                                     | Auto-tangle org-babel source blocks on save                           |
| `org-autoexport`                                      | Auto-export org files on save (e.g. to HTML)                          |
| `org-modern`                                          | Modern Org mode styling; enables `global-org-modern-mode`             |
| `org-modern-indent`                                   | Improved indentation display in `org-modern-mode` (`:vc` from GitHub) |
| `shfmt`                                               | Shell script formatter (`C-c C-f` in `sh-mode`)                       |
| `ox-gfm`                                              | Export Org files to GitHub-Flavored Markdown                          |
| `htmlize`                                             | Syntax-highlighted source blocks in org HTML/LaTeX export             |
| `exec-path-from-shell`                                | Imports `$PATH` and env vars from the login shell.                    |
| `xkcd`                                                | Browse xkcd comics inside Emacs                                       |

---

## Python Tooling

mise manages uv; uv creates `.venv` in each project. `python-shell-exec-path`
is set to `./.venv/bin` so Emacs resolves `python3` from the project venv.
If Python features misbehave, ensure `uv sync` has been run in the project root.

---

## Secrets

`my-secrets.el` is required by `config.org` and must exist locally. It
holds variables that must not be committed (API keys, passwords, etc.). It
is intentionally absent from VCS. Create it with at minimum:

```emacs-lisp
;; -*- lexical-binding: t -*-
;;; my-secrets.el --- local secrets
(provide 'my-secrets)
;;; my-secrets.el ends here
```

The `gptel` package reads `ANTHROPIC_API_KEY` from the environment
(currently disabled — see Notable Packages).

---

## Build / Maintenance

```bash
make clean        # delete all *.elc byte-compiled files (preserves source)
make list-targets # list available Makefile targets
```

`early-init.el` is re-tangled automatically when you save `early-config.org`,
via an `after-save-hook` set in that file's local variables. If needed, you can
also run `M-x org-babel-tangle` manually. `config.el` is regenerated
automatically on each Emacs startup via `org-babel-load-file` in `init.el`.

---

## Planned / Open Work (todo.org)

- Set up GitHub Actions to run startup-smoke tests.
- Create an issue template.
- Write `CONTRIBUTING.md`.

## Behavioral Guidelines

Aside: Based on
[andrey-kapathy-skills](https://github.com/multica-ai/andrej-karpathy-skills).

## 1. Think Before Coding

**Don't assume. Don't hide confusion. Surface tradeoffs.**

Before implementing:
- State your assumptions explicitly. If uncertain, ask.
- If multiple interpretations exist, present them - don't pick silently.
- If a simpler approach exists, say so. Push back when warranted.
- If something is unclear, stop. Name what's confusing. Ask.

## 2. Simplicity First

**Minimum code that solves the problem. Nothing speculative.**

- No features beyond what was asked.
- No abstractions for single-use code.
- No "flexibility" or "configurability" that wasn't requested.
- No error handling for impossible scenarios.
- If you write 200 lines and it could be 50, rewrite it.

Ask yourself: "Would a senior engineer say this is overcomplicated?" If yes,
simplify.

## 3. Surgical Changes

**Touch only what you must. Clean up only your own mess.**

When editing existing code:
- Don't "improve" adjacent code, comments, or formatting.
- Don't refactor things that aren't broken.
- Match existing style, even if you'd do it differently.
- If you notice unrelated dead code, mention it - don't delete it.

When your changes create orphans:
- Remove imports/variables/functions that YOUR changes made unused.
- Don't remove pre-existing dead code unless asked.

The test: Every changed line should trace directly to the user's request.

## 4. Goal-Driven Execution

**Define success criteria. Loop until verified.**

Transform tasks into verifiable goals:
- "Add validation" → "Write tests for invalid inputs, then make them pass"
- "Fix the bug" → "Write a test that reproduces it, then make it pass"
- "Refactor X" → "Ensure tests pass before and after"

For multi-step tasks, state a brief plan:
```
1. [Step] → verify: [check]
2. [Step] → verify: [check]
3. [Step] → verify: [check]
```

Strong success criteria let you loop independently. Weak criteria ("make it
work") require constant clarification.
