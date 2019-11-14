Introduction
============================

Hschain-utxo is a framework to create blockchains based on UTXO-model 
with rules that are defined with smartcontracts. 
It runs on top of hschain consensus protocol.

To define smartcontracts hschain-utxo uses special language that is inspired by ErgoScript.
The framework consists of several parts:

* the language for smartcontracts 
* interpreter for the language to try out ideas
* service to run blockchain
* client to post transactions and query useful information with API


The structure of the project
------------------------------------------

The modules are separated for the parts:

* ``hschain-utxo-lang`` defines language of smartcontracts and model of execution and key terms
* ``hschain-utxo-state`` defines blockchain and rules to commit the blocks and transactions
* ``hschain-utxo-service`` is service to run a node
* ``hschain-utxo-api`` is API to interact with blockchain
* ``hschain-utxo-repl`` interpreter for the language 
* ``hschain-utxo-test`` test scripts to verify that everything works fine. 
* ``hschain-utxo`` main application and utility tools (to generate genesis for instance)


UTXO-model and smartcontracts
------------------------------------------

Unspent Transaction Output (UTXO) model describes exchange of values. 
It was invented for Bitcoin blockchain
and successfully implemented there. It allows us to model exhange 
of any values in quite smart way. 

Suppose that user holds some amount of values and we want to transfer value
from one user to another. The value is represented 
by UTXO that can become an input for the future transaction.
If Alice wants to send 10 coins to Bob she can use her UTXO as input and
create another UTXO for Bob that will contain 10 coins. The UTXO is often protected
by the user signature. So in order to spend the transaction we need to prove that
we hold the value for this output. 

Once UTXO is spent it is destroyed and in place of it we have new UTXOs.
That's how smartcontracts work. We use UTXOs in the blockchain as inputs
check conditions in the smartcontract and if it holds true we destroy
inputs and substitute them with outputs. 

**Smart contract**
   Is a script that evaluates to true or false. It defines the rules to spend UTXOs.

**Transaction**
   Transaction tries to use UTXOs that exist in the system and turn them to another UTXOs.
   TX takes in UTXOs of the blockchain and environment variables (like height of the blockchain)
   that checks condition and if it evaluates to true it destroys the inputs and produces
   new UTXOs as outputs.
 
**Unspent transaction output (UTXO)**
   Box that contains a value that can be used as input for transaction
   and it is protected by the script (smartcontract). It can define rules
   for smartcontracts that try to spend the value.

**Block**
   Contains a list of transactions. It can be though of as execution of transaction in batch mode.
   If all transactions in the block are valid it is executed on the blockchain and it
   sayed to be *commited*.

**Blockchain**
   For UTXO-model blockchain is a set of UTXOs that can be spent and global time
   of execution which corresponds to the number of commited blocks.


Language of smart contracts
----------------------------------------------

Smart contracts are used to protect UTXOs from illegal spending. 
To define what is legal we have a language. It encodes conditions for protection of the values in UTXOs.
Our language is based on ErgoScript. For now it's typed lambda-calculus with 
support for sigma-protocols. Sigma-protocols define a way to protect the values
with cryptographic algorithms. Sigma-protocols let us prove that user owns private key 
without exposure of the private key itself.

Let's look at example of the simplest script::

   pk alice

``pk`` is a function that evaluates to true only if user can prove that he or she
is owner of the alice secret key.

Also we can use conditions and boolean operators::

   pk alice || (pk bob && getHeight > 100)

In this transaction Alice can grab the value or Bob can have it if 
the height of the blockchain is greater than 100.

Blockchain node
------------------------------------------------

Blockchain node runs a service that accepts postage of blocks by REST API.
It uses hschain as aconsensus algorithm to make commits consistent accross the
network. We can run the node like this::

  cabal new-run hschain-utxo-service

It accepts config and genesis see corresponding section for details.

API
------------------------------------------------

We can use REST API to post transactions and query useful information.
The clinets are defined in the module ``hschain-utxo-api`` see the corresponding section
for details. 

