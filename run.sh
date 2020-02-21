#!/usr/bin/env bash
set -e

trap 'kill 0' SIGINT

cabal -O0 new-build all

cabal -O0 new-run -- hschain-utxo:hschain-utxo validator --config hschain-utxo/config/main/node-val1.yaml --genesis hschain-utxo/config/pool/genesis.json  &
cabal -O0 new-run -- hschain-utxo:hschain-utxo validator --config hschain-utxo/config/main/node-val2.yaml --genesis hschain-utxo/config/pool/genesis.json  &
cabal -O0 new-run -- hschain-utxo:hschain-utxo validator --config hschain-utxo/config/main/node-val3.yaml --genesis hschain-utxo/config/pool/genesis.json  &
cabal -O0 new-run -- hschain-utxo:hschain-utxo validator --config hschain-utxo/config/main/node-val4.yaml --genesis hschain-utxo/config/pool/genesis.json  &

cabal -O0 new-run -- hschain-utxo:hschain-utxo webnode --config hschain-utxo/config/main/node-web.yaml --genesis hschain-utxo/config/pool/genesis.json 
