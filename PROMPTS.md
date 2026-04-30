# Prompts

## 2026-04-29

Analyze the existing code and create a CLAUDE.md file for Claude Code.

## 2026-04-30

Re-analyze changed file config.org and update CLAUDE.md if needed.

check file init.el for errors

implement the fix to remove the eval-when-compile and package-initialize calls.

write a commit message for the changes

shorten the commit message to remove fine details.

add recent prompts to PROMPTS.md

check @config.org for errors

fix the five definite bugs in config.org

re-check @config.org

Implement a change to add the my_emacs/ directory to the load path, and configure emacs to omit the .github directory and its descendants.

Undo the change to treemacs, and add a configuration to omit .github/ directory from load-path.

undo the change to ido

Why does the treemacs sidebar disappear when emacs starts?

yes

Rewrite the get-config-file to properly expand to the my_emacs directory. Fix the the keybinding on line 148. add the :commands line to elpy use-package. Replace the :custom on line 308 with the correct :config clause. Update the gptel-model setting to use Claude Sonnet 4.6.

check @early-init.org for errors.

yes, and update the one-armed if calls.

check for installed emacs packages that don't have use-package declarations, and add the equivalent use-package calls to config.org

evaluate all the keybindings in my config files and report conflicts with each other or with emacs built-ins
