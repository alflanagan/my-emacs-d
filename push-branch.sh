#!/usr/bin/env sh

cd ~/.config/emacs/my_emacs || exit
branch=$(git symbolic-ref --short HEAD) || exit
for r in $(git remote); do git push "$r" "$branch"; done
