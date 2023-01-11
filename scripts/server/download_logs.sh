#!/bin/sh

rsync --archive --compress --verbose --progress --executability \
  --human-readable \
  root@134.209.201.43:/root/Documents/workspace/log*.txt $HOME/Documents/workspace/brbo2-impl/output/logs/