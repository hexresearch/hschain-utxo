#!/usr/bin/env bash
set -xe

# source ./setup_git.sh

GIT_HASH=$(git rev-parse HEAD)
tag=$(git tag -l --points-at HEAD)
if [ ! -z $tag ]; then
  GIT_TAG_ARG="--arg gitTag \"\\\"$tag\\\"\""
fi
GIT_BRANCH=$(git branch | grep \* | cut -d ' ' -f2)

NIX_PATH=$GIT_NIX_PATH$NIX_PATH nix-shell --fallback --command "cd ../; return" \
  $GIT_TAG_ARG \
  "$@"
