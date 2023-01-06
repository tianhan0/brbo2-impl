#!/bin/sh

rsync --archive --compress --verbose --progress --executability \
  --human-readable --exclude target/ \
  --recursive $HOME/Documents/workspace/brbo2-impl/ root@134.209.201.43:/root/Documents/workspace/brbo2-impl/