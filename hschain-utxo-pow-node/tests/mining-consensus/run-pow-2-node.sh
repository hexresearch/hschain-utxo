#!/bin/sh
set -e
trap 'echo TRAP; pkill -P $$' SIGINT SIGTERM

cabal new-build hschain-utxo-pow-node 
echo ====
export PK1='"FPY3TBv4c4gKkTsWH68wUAUrXvaLnJnWsDEwhHqotE4T"'
export PK2='"Gt1aMuZqJ2vnVmrVvgTNsmcYg2aYfc8SbXzfaVFBzE6t"'
cabal new-exec -- hschain-utxo-pow-node-app node -c pow/node1.yaml --secret-env-var PK1 -g pow/utxo-genesis --db db &
cabal new-exec -- hschain-utxo-pow-node-app node -c pow/node2.yaml --secret-env-var PK2 -g pow/utxo-genesis --db db
