#!/bin/sh

rsync --archive --compress --verbose --progress --executability --delete \
  --human-readable \
  --exclude target/ --exclude output/cfg/ \
  --recursive root@134.209.201.43:/root/Documents/workspace/brbo2-impl/ $HOME/Documents/workspace/brbo2-impl/