#!/usr/bin/env bash
NIX_PATH=$GIT_NIX_PATH$NIX_PATH nix-build --arg isProd true "$@"
