#!/bin/sh

rsync --archive --compress --executability \
  --human-readable \
  --exclude target/ \
  --exclude output/ \
  --exclude src/main/java/brbo/fuzz/drivers/ \
  --exclude .git/ \
  --exclude src/main/scala \
  --exclude src/test \
  --exclude lib/ \
  --exclude scripts/ \
  --recursive root@134.209.201.43:/root/Documents/workspace/brbo2-impl/ $HOME/Documents/workspace/brbo2-impl/

rsync --archive --compress --executability \
  --human-readable \
  --exclude target/ \
  --exclude output/ \
  --exclude src/main/java/brbo/fuzz/drivers/ \
  --exclude .git/ \
  --exclude src/main/scala \
  --exclude src/test \
  --exclude lib/ \
  --exclude scripts/ \
  --recursive root@64.227.67.224:/root/Documents/workspace/brbo2-impl/ $HOME/Documents/workspace/brbo2-impl/