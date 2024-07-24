#!/bin/bash

# TODO: call this 'get_unused_packages', edit package list, print diffs
# TODO: figure out way to handle use-package\n package-name
for PKG in $(cat packages.txt )
do
  grep ${PKG} init.el
done | grep use-package | grep -v '^ *;;' | sort -u
