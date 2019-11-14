API for hschain-utxo
============================


With API we can post transactions and query useful information about
current state of the blockchain.


Post transaction
--------------------------------------

To post transaction use the method ``api/tx/post``::

  curl -H "Content-Type: application/json" \
      --data @config/tx-example.json \
      hschain-utxo-host/api/tx/post

Query state
-------------------------------------

We can read the current state of blockchain::

  curl hschain-utxo-host/api/debug/state/get

We can query environment parameters of blockchain::

  curl localhost:8181/api/env

   
Query box balance
--------------------------------------

We can query the balance of the box by its identifier::

  curl hschain-utxo-host/api/box-balance/get/{box-id}



