#!/bin/sh

rsync --archive --compress --verbose --progress --executability --delete \
  --human-readable \
  --exclude target/ --exclude output/cfg/ --exclude output/fuzz/ --exclude output/cmd/ --exclude src/main/java/brbo/fuzz/drivers/ \
  --recursive root@134.209.201.43:/root/Documents/workspace/brbo2-impl/ $HOME/Documents/workspace/brbo2-impl/