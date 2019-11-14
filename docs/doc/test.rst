Tests for hschain-utxo
===================================

Tests are implemented in the module ``hschain-utxo-test``.
There are three scenarios for tests:

* simple exchange of the value

* delayed exchange, when payer can withdaw the money within certain period of time.

* XOR-game. Two users play the XOR-game.

Tests can be run with::

   cabal new-run hschain-utxo-test



