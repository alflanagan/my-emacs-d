find . -name devdocs -prune \
       -o -name elpy -prune \
       -o -name .cache -prune \
       -o -name xkcd -prune \
       -o -name backups -prune \
       -o -name eln-cache -prune \
       -o -name .git -prune \
       -o -name elpa -prune \
       -o -type f -print
