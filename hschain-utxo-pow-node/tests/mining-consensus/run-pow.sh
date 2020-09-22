#!/bin/sh
set -e
trap 'echo TRAP; pkill -P $$' SIGINT SIGTERM

cabal new-build hschain-PoW 
echo ====
export PK1='"FPY3TBv4c4gKkTsWH68wUAUrXvaLnJnWsDEwhHqotE4T"'
export PK2='"Gt1aMuZqJ2vnVmrVvgTNsmcYg2aYfc8SbXzfaVFBzE6t"'
cabal new-exec -- hschain-utxo-pow-node-app pow/node1.yaml --mine --node-secret-env-var PK1 &
cabal new-exec -- hschain-utxo-pow-node-app pow/node2.yaml --mine --node-secret-env-var PK2
