#!/bin/sh

rsync --archive --compress --executability --delete \
  --human-readable \
  --exclude target/ \
  --exclude output/ \
  --exclude logs/ \
  --exclude src/main/java/brbo/fuzz/drivers/ \
  --recursive $HOME/Documents/workspace/brbo2-impl/ root@134.209.201.43:/root/Documents/workspace/brbo2-impl/

rsync --archive --compress --executability --delete \
  --human-readable \
  --exclude target/ \
  --exclude output/ \
  --exclude logs/ \
  --exclude src/main/java/brbo/fuzz/drivers/ \
  --recursive $HOME/Documents/workspace/brbo2-impl/ root@64.227.67.224:/root/Documents/workspace/brbo2-impl/