Interpreter for the language
==========================================

We can use REPL to quickly test ideas and language features::

   cabal new-run hschain-utxo-repl

REPL options
---------------------------

We can type ``:help`` to see complete list of options.
Most useful are 

``:l`` or ``:load``
   loads script or TX. If file extension is json it is loaded as TX otherwise as script.

``:r`` or ``:reload``
   reloads current script and TX


``:q`` or ``:quit``
   to stop REPL

``:t`` or ``:type``
   show type of the expression


Environment
--------------------------
   
Every script works within the context of transaction. 
By default no TX is loaded and blockchain height is set 0.
But we can load concrete TX and test expressions within context of TX.

Note that in case of REPL we have to use not ``BoxId``'s as inputs
but the full structure for Boxes. Because we don't have access to the real blockchain.

.. topic:: TODO

   Maybe it's worth to create REPL that can connect to the blockchain
   real or test one?

