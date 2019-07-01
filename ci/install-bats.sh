#! /usr/bin/env bash
out=mktemp
cd $out
git clone \
  --branch master \
  --depth 1 \
  https://github.com/kirens-dotfiles/bats.git .

./install.sh /usr/local
