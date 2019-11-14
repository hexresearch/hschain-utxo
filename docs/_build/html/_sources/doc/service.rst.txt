Hschain-utxo node
==================================

The web-service accepts transactions with API and updates blockchain.

We can start it with command::
   
   cabal new-run hschain-utxo-service

Be default it reads options and genesis from the directory `config`
in the top-most level of the hschain-utxo repository.

We can set the options with flags::

   cabal new-run hschain-utxo-service -- --config config.yaml --genesis genesis.json

We can generate test genesis with utility::

   cabal new-run hschain-utxo-generate-genesis

