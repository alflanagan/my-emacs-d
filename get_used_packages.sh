#!/bin/bash

# TODO: What we need to do is extract only the use-package s-expressions
#       from config.org
for PKG in $(cat packages.txt )
do
  grep ${PKG} config.org
done | grep use-package | grep -v '^ *;;' | sed -e "s/^ *//" | cut -d' ' -f2 | cut -d')' -f1 | sort -u
