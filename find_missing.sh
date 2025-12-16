#!/usr/bin/env zsh

TMPFILE=$(mktemp)

./get_used_packages.sh > "${TMPFILE}"

for PKG in $(diff packages.txt "${TMPFILE}" | grep '<' | cut -c3-)
do
  echo -n "$PKG: "; grep -c $PKG config.org
done | grep ': 0$'
rm "${TMPFILE}"
