#!/usr/bin/env bash
set -xe

# source ./setup_git.sh

NIX_PATH=$GIT_NIX_PATH$NIX_PATH nix-shell --fallback --command "cd ../; return" \
  "$@"
