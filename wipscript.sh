#! /bin/sh

echo "Starting"
cd ~/discompiler
git add scratchpad.lisp
git commit scratchpad.lisp -m 'scratchpad/lisp'

git checkout wip/boo
git checkout boo -- scratchpad.lisp
git commit -am 'scratchpad.lisp'

git checkout boo
git reset HEAD^

echo "Finished."
