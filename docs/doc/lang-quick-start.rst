
Quick start guide for language
========================================

The language for smart contracts is heavily insired with ErgoScript and Haskell.
So we can use our haskell intuition in many circumstances.

Functions behave like Haskell functions. They are applied with space defined with equal sign
or slash-lambda. Like in haskell we have ``if-then-else`` and ``let-in`` expressions.::

  > addTwo x = x + 2
  > addTwo 2
   4

Like in Haskell we have tuples. They are accessed with operator ``tuple !! int``.
Also we have vectors with usual operators ``map`` (map over), ``fold`` (left fold), ``length`` (size of the vector), 
``++`` (concatenation). 

the script is a list of defenitions that ends up with boolean expression.::

  one = 1
  two = 2

  check x = one + two == x

  check 3

.. topic:: Order matters

   We should list the definitions in order of execution. 
   So reversing the order is not allowed::

      
      check x = one + two == x   -- error: unbound variable name

      one = 1
      two = 2

.. topic:: No recursion

   The termination of execution should be guaranteed for our language. 
   So the recursive functions are not allowed.

Ownership check - the heart of the language
----------------------------------------------------

The heart of the language is a function ``pk`` that checks for ownership:

.. function:: pk :: Text -> Bool

   Check for ownership of private key.

It implements sigma-protocol. The transaction should have the field ``proof``
that lists all rproofs for ownership of the keys. 

Here is the simple script to protect Bob's values::

  pk "bob-key"


.. topic:: todo

   Bob now contains private-key :) but it have to be implemented with sigma-protocol
   and probalby be based on public key.

Also we can give money to Alice until certain time and later 
give it to Bob excluding Alice::
   
   time = 100

      (pk alice && getHeight <= time) 
   || (pk bob   && getHeight >  time)

Transaction execution
-------------------------------------------

Let's look at how transaction is executed. Transaction has several components:

**Inputs**
   List of identifiers for exisiting UTXOs in the blockchain

**Outputs**
   List of produced UTXOs. They are added to blockchain if TX is valid.
   The input boxes are destroyed.

**Proof**
   List of public keys that proove the ownership (todo: should be implemented with sima-protocol).

**Args**
   Key-value pairs of primitive values. 

User creates transaction and posts it to the system with API-call.
The engine reads all inputs from the blockchain and creates the total
script for transaction by and-concatination of all scripts in inputs.
If all input scripts are valid in the current context of transaction and blockchain
then transaction is valid and we destroy input UTXOs and add output UTXOs to the
blockchain.

In the following sections we are going to look at several examples. 

Simple money exchange script
---------------------------------------------

Let's look at the very simple scenario. Alice gives 2 coins to Bob.
And Bob gives 5 to Alice. Let's see how it can be implemented.

Suppose that Alice has UTXO with 10 coins that is protected by the script::

  pk alice

To send 2 coins to Bob she creates a TX that includes her UTXO as input,
provides proof of her ownership and creates two UTXOs. 
The first UTXO gives money to Bob. It has 2 coins of value and contains script::

  pk bob

The second UTXOs is cashback for Alice, she gives change of 8 coins to herself::

  pk alice

In the UTXO model we have to spend all values of the inputs, because inputs would be destroyed 
after TX confirmation. So we have TX such as::

   {
      "inputs": ["alice-utxo-1"],
      "outputs": 
         [ { "id": "bob-utxo-1",
           , "value": 2,
           , "script": "pk bob",
           , "args": {} 
           }
         , { "id": "alice-utxo-1",
           , "value": 8,
           , "script": "pk alice",
           , "args": {}
           }
         ];
      "proof": ["alice-key"],
      "args": {}
   }

This is slightly simplified version of TX. But we can get the idea.
TX is a json-object that contains the fields: "inputs", "outputs", "proof" and "args".
The outputs is a list of UTXOs, each of them has fields "id", "value", "script" and "args".

To make real transaction we also need to compile the script. But here for simplicity of
explanation it's written in stright form 

Now suppose that Bob has UTXO with 4 coins. And he wants to give 5 coins back to Alice.
But also alice just gave him 2 coins, so he can use two UTXOs as inputs and create
2 UTXOs as outputs for Alice and cashback for himself::

   {
      "inputs": ["bob-utxo-0", "bob-utxo-1"],
      "outputs": 
         [ { "id": "bob-utxo-2",
           , "value": 1,
           , "script": "pk bob",
           , "args": {} 
           }
         , { "id": "alice-utxo-2",
           , "value": 5,
           , "script": "pk alice",
           , "args": {}
           }
         ];
      "proof": ["bob-key"],
      "args": {}
   }

It is enforced by the blockchain that sum of input values should be equal to sum of output values.

Pay for Cofee - delayed exchange
--------------------------------------------

Imagine that Alice wants to buy cofee from Bob and she wants to pay with our blockchain.
But she wants to be able to get the money back until certain amount of time from now.
So Alice wants to give the money to Bob. But bob can collect the money only after 20 steps
of blockchain. Up until that time Alice can get her money back.

To do it Alice can create UTXO with following script::

   timeBound = ... -- some number ahead of current height
   
      (pk alice && getHeight < timeBound) 
   || (pk bob   && getHeight >= timeBound)


XOR-game
--------------------------------------------

For XOR-game we have two players: Alice and Bob. 
Players guess numers 0 or 1. And if numers are the same alice wins
otherwise Bob wins. Let's suppose that both players give 1 coin for the game.
And the winner takes both of them. 

This example is taken from the paper on ErgoScript and adapted for our language.

To start the game Alice creates half-game script with value of 1 coin.
Then Bob joins and creates full game script with value of 2 coins. 
Alice creates a guess ``a`` and secret ``s`` also she computes ``k = hash (s <> a)``. 
She creates UTXO with value of 1 coin.
This box is protected by a script called the half-game script given below. Alice waits
for another player to join her game, who will do so by spending her half-game output and
creating another box that satisfies the conditions given in the half-game script.

Bob joins Alice’s game by picking a random bit b and spending Alice’s half-game output to
create a new box called the full-game output. This new box holds two coins and contains b
(in the clear) alongwith Bob’s public key in the registers. Note that the full-game output
must satisfy the conditions given by the half-game script. In particular, one of the conditions
requires that the full-game output must be protected by the full-game script (given below).

Alice opens k offchain by revealing s, a and wins if a = b. The winner spends the full-game
output using his/her private key and providing s and a as input to the full-game script.
If Alice fails to open k within a specified deadline then Bob automatically wins.

The full-game script encodes the following conditions: The Box arguments with 
names ``"guess"``, ``"publicKey"`` and ``"deadline"`` expected
to store Bob’s bit b, Bob’s public key (stored as a proveDlog proposition) and the deadline for Bob’s
automatic win respectively. The context variables with id 0 and 1 (provided at the time of spending
the full-game box) contain s and a required to open Alice’s commitnent k, which alongwith Alice’s
public key alice is used to compute ``fullGameScriptHash``, the hash of the below script::

   s = getVar "s"
   a = getVar "a"
   b = getArg getSelf "guess"
   bobKey = getArg getSelf "publicKey"
   bobDeadline = getArg getSelf "deadline"

      (pk bob && getHeight > bobDeadline) 
   || (   blake2b256(s <> a) == k 
       &&    ((pk alice) && a == b 
          || (pk bob) && a != b))

The above constants are used to create ``halfGameScript``::

  out = getOutput 0
  b   = getBoxArg out "guess"
  bobDeadline = getBoxArg out "deadline"
  validBobInput = b == 0 || b == 1
     validBobInput
  && (blake2b256 (shwoScript (getBoxScript out)) == fullGameScriptHash)
  && (length getOutputs == 1 || length getOutputs == 2) 
  && bobDeadline >= getHeight + 30
  && getBoxValue out >= getBoxValue getSelf * 2

Alice creates her half-game box protected by halfGameScript, which requires that the transac-
tion spending the half-game box must generate exactly one output box with the following properties:
   
1. Its value must be at least twice that of the half-game box.

2. Its argument "guess" must contain a byte that is either 0 or 1. This encodes Bob’s choice b.

3. Its argument "deadline" must contain an integer that is at least 30 more than the height at which the box is generated. This will correspond to the height at which Bob automatically wins.

4. It must be protected by a script whose hash equals ``fullGameScriptHash``.

The game ensure security and fairness as follows. Since Alice’s choice is hidden from Bob when
he creates the full-game output, he does not have any advantage in selecting b. Secondly, Alice is
guaranteed to lose if she commits to a value other than 0 or 1 because she can win only if a = b.
Thus, the rational strategy for Alice is to commit to a correct value. Finally, if Alice refuses to
open her commitment, then Bob is sure to win after the deadline expires.

Create transaction and send it with API
---------------------------------------------

We can post the transaction over API. To do it we have to create TX as JSON object.
Every TX  is a JSON-object that contains following fields::

  { "inputs": ["utxo-input-id-1", "utxo-input-id-2"]
  , "outputs": [box1, box2]
  , "proof": [pk1, pk2]
  , "args": { "arg1": val1,
            , "arg2": val2
            }
  }



Inputs  contain the list of Box identifiers that are used as inputs.
Ouptuts contain boxes that are going to be produced after TX is validated.
Proof contains the list of publik keys to prove the ownership (see the function ``pk``). 
Args contains the map of key-value. It can be empty.

The sum of values of inputs should be equal to sum of values of outputs. 
For TX to be valid all conditions for scripts of the inputs should evalueate to true.
List of inputs should be non-empty.

Let's look at the value of output box. It's JSON-object::

   {
      "id": "utxo",
      "value": 10,
      "script": "string with compiled script",
      "args": {},
   }

It contains UTXO identifier, amount of maney as a value, script and arguments.
The script is written in our language. But to get the final string for transaction we need
to compile it with compiler ``hschain-utxo-compiler``::

  cabal new-run hschain-utxo-compiler -i script.hs -o out.txt

if flag ``-o`` is omitted then the result is dumped to stdout. 
Then paste the output to the output box script field. We can save the TX to file ``tx.json``
and post the TX with following curl::

  curl -XPOST --data @tx.json service.host/api/post-tx


How to do it with Haskell
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

With Haskell we can create transactions and post them with easy to use library.
We need libriaries ``hschain-utxo-lang`` to create value for transaction 
and library ``hschain-utxo-api-client`` to post the transaction.

Let's create a transaction and post it.


Let's post it with the client.






