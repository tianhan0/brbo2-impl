#!/bin/sh

rsync --archive --compress --verbose --progress --executability --delete \
  --human-readable \
  --exclude target/ --exclude output/ --exclude src/main/java/brbo/fuzz/drivers/ \
  --recursive $HOME/Documents/workspace/brbo2-impl/ root@134.209.201.43:/root/Documents/workspace/brbo2-impl/

rsync --archive --compress --verbose --progress --executability --delete \
  --human-readable \
  --exclude target/ --exclude output/ --exclude src/main/java/brbo/fuzz/drivers/ \
  --recursive $HOME/Documents/workspace/brbo2-impl/ root@159.89.85.242:/root/Documents/workspace/brbo2-impl/