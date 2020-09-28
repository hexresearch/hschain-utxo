#!/bin/sh
set -e
trap 'echo TRAP; pkill -P $$' SIGINT SIGTERM

cabal new-build hschain-utxo-pow-node --enable-profiling --enable-library-profiling
echo ====
export PK1='"FPY3TBv4c4gKkTsWH68wUAUrXvaLnJnWsDEwhHqotE4T"'
cabal new-exec -- hschain-utxo-pow-node-app node -c pow/node1-1.yaml --secret-env-var PK1 -g pow/utxo-genesis --db db +RTS -xc
