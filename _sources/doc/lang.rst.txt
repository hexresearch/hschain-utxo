 
Language of smart contracts
=====================================

The smart contract is a script that evaluates to boolean. 
It is used to protect the value of UTXOs from illegal spending.

Our language is based on ErgoScript. It's strongly-tpyed lamda-calculus
with support for sigma-protocols. With sigma-protocols we can prove
that user owns the secret key without exposure of the private data.

Let's look at the language in details. 
It's very simple and minimalistic. 

We can start interpreter to test expressions::

   cabal new-run hschain-utxo-repl

Basic building blocks
---------------------------------

Script is a sequence of defenition with last expression being boolean expression::

  one = 1
  two = 2
  condition x y = x <= y

  condition one two && pk "alice-key"

Definitions
^^^^^^^^^^^^^^^^^^^^^^^

We can define constants::

  greet = "Hello"

And we can define functions::

  addWorld x = x <> " World"

Also we can define synonyms with ``let`` construct::

  let greet = "Hello"   
  in  addWorld greet

Functional application
^^^^^^^^^^^^^^^^^^^^^^^^^

Functions are applied with space. Function namegoes first and then arguments follow::

  > addTwo x = x + 2
  > addTwo 1 
    3

Branching
^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can branch with if-then-else::

  x = if getHeight > 10
    then a
    else b

Ownership
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can test for ownership with function ``pk``

.. function:: pk :: Text -> Bool

   It evaluates to true if the owner of the key is proven in the transaction field ``proof``
   of the transaction.

Transaction has special field ``proof`` where it contains provement of the ownership.

Type signatures
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can give signature with double colon::

  (1 :: Int)

  ((1, "Hello") :: (Double, Text))


Primitive types
---------------------------------

Language has following primitives:

``Bool``
   Boolean values (with constants ``True`` and ``False``)

``Int``
   integer values (represented internaly as Int64). Moneys are represented with integers.

``Text``
   strings. Constants are enclosed in double-quotes: ``"Hello world!"``

``Script``
   script that protects the value.

``Box``
   input or output boxes. We create boxes with scripts to hold values protected by the scripts.
   Box can be input or output boxes. We spend values from input boxes and create output boxes
   when script is true.


Boolean operators
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We have usual boolean operations:  ``not``, ``||``, ``&&``.
Also we have conditions defined for all primitive types like: ``==``, ``/=``, ``<``, ``<=`` etc.

We have if-then-else::

  x = if getHeight > 10
    then a
    else b

Numeric operators
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We have numeric operators: ``negate``, ``+``, ``*``, ``/``.
They work on all numeric types.

Text operators
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Text are lists of chars. We have following functions:

.. function:: lengthText :: Text -> Int

   Return the size of text

.. function:: (<>) :: Text -> Text -> Text

   Text concatenation.

.. function:: showBool, showInt, showMoney, shwoDouble, showScript

   Converts various primitives to text.

.. function:: sha256, blake2b256 :: Text -> Text

   Compute hash of the text.

Also we can check texts for equality and compare it. 

Script operators
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The only thing we can do with script is to check for equality and convert it to text with function

.. function:: showScript :: Script -> Text

   Converts script to text.


Compound types
---------------------------------------------

For compound types we have functions, tuples, vectors and boxes.

Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can create function with lambda expression::

  addTwo = \x -> x + 2
  
  square = \x -> x * x

  mul = \x y -> x * y

Also we can define them in this way::

   addTwo x = x + 2

   square x = x * x

   mul x y = x * y

We have usual function combinators: ``id``, ``const``. We can use partial application.

Functions can have guards::

   signum x
     | x > 0  = 1
     | x == 0 = 0
     | x < 0  = -1

Functions can contain pattern-matching cases::

   xor True  False = True
   xor False True  = True
   xor _     _     = False

Also functions can contain local definitions that are defined with ``where``-clause::

   sumSquares :: Int -> Int -> Int
   sumSquares x y = square x + square y
     where
       square a = a * a

It's equivalent to the following definition with let-expression::
   
   sumSquares x y = 
     let square a = a * a
     in  square x + square y
   
User-defined types
^^^^^^^^^^^^^^^^^^^^^^^^^

User can define a type. the style is borrowed from Haskell::

   data Color = Red | Blue | Green

We define a color with three alternative cases represented with type-constructors.
Also type-constructors can hold values. Let's define type for users with name and age::

   data User = User Text Int

   john :: User
   john = User "John" 23

Types can have parameters. This way we can define Haskell type ``Maybe`` for optional values::

   data Maybe a = Nothing | Just a

We can construct the values of our own type with constructors. To deconstruct them we use case-expressions.

Note that recursive types are prohibited. We can not construct recursive types.
It is deliberate restriction of our language since we do not support recursion.

case-expressions
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Case-expression deconstruct values from our own types::

   getName :: User -> Text
   getName user = case user of
      User name _ -> name

   colorIsRed :: Color -> Bool
   colorIsRed color = case color of
      Red -> True
      _   -> False

   happyBirthday :: User -> User 
   happyBirthday user = case user of
      User name age -> User name (age + 1)
      

pattern-matching for function arguments
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can also deconstruct the value in the arguments of the functions with pattern-matching.
Let's rewrite the functions with pattern-matching::

   getName (User name _) = name
   
   happyBirthday (User name age) = User name (age + 1)

   colorIsRed Red = True
   colorIsRed _   = False

Records
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Sometimes it's useful to have named fields in the type-constructors. 
This part of the language is also inspired by Haskell and we can create named-fields
and update them just like in Haskell::

   data User = User
      { user'name :: Text
      , user'age  :: Int
      }

Note that just like in Haskell names of the fields have to be unique across the whole global 
scope of the program. 

Now we can create users and supply information about the fields::

   john = User
      { user'name = "John"
      , user'age  = 23
      }

We can update the fields::

   happyBirthday user = user { user'age = 1 + user'age user }

Tuples
^^^^^^^^^^^^^^^^^

Tuples represent sets of heterogeneous values. We use parenthesis to denote them `(True, 1)`
We construct tuples with parens and destruct them pattern-matching or case-expressions or pattern-matching::

  x = (1, True)

  y = case x of
   (a, _) -> a

  fst :: (a, b) -> a
  fst (a, _) = a


Vectors
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Vectors represent a row of values of the same type. 
We use brackets to denote constant vectors: `[1,2,3]`.
To get vector values we use operator ``!``::

  > x = [1, 2, 3]
  > x ! 1
    2
  > map (\n -> n * n) x
    [1, 4, 9]

We have operations for vectors

.. function:: (!) :: Vector a -> Int -> a

   Get vector value by index.

.. function:: length :: Vector a -> Int

   Return the length of the vector.

.. function:: (++) :: Vector a -> Vector a -> Vector a

   Concatenation of the vectors.

.. function:: map :: (a -> b) -> Vector a -> Vector b

   Maps over values of the vector

.. function:: fold :: (a -> b -> a) -> a -> Vector b -> a

   Left fold for the vector

.. function:: sum :: Vector Int -> Int

   Sum of numeric values

.. function:: product :: Vector Int -> Int

   Product of numeric values

.. function:: any :: Vector Bool -> Bool

   Any of values is true

.. function:: all :: Vecotr Bool -> Bool

   All values should be true

.. function:: getInputs :: Vector Box

   Read vector of transaction inputs

.. function:: getOutputs :: Vector Box

   Read vector of transaction outputs

Boxes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Box represent UTXO. It has 

* identifier (textual name)
* assigned value (some money to spend)
* script that protects the value 
* arguments (list of key-value pairs)

Every smartcontract is executed in the context that is provided by transaction.
Transaction has inputs and outputs also it can contain some primitive values that
are used as arguments. 

Inputs and outputs contain boxes. Input boxes exist in the system and are going
to be spent with transaction. Ouput boxes are produced with transaction that is valid and commited.

So smart contracts can use condition based on the values that are stoed in 
the input and output boxes. Let's look at how we can do that. Let's consider first
the funcitions to read the boxes:

.. function:: getSelf :: Box

   Reads the self box. The box of the current input.

.. function:: getInputs :: Vector Box
   
   Reads the vector of all input boxes.

.. function:: getOutputs :: Vector Box
   
   Reads the vector of all output boxes.

.. function:: getInput :: Int -> Box

   Reads input box by index

.. function:: getOutput :: Int -> Box

   Reads output box by index

Now we know how to read the boxes. Let's look at how to process the values.
We can check the values stored in the boxes if we read the them.
Let's look at the extractors:

.. function:: getBoxId :: Box -> Text

   Return box identifier.

.. function:: getBoxValue :: Box -> Money

   Return box value

.. function:: getBoxScript :: Box -> Script

   Return box script

.. function:: getBoxArg :: Box -> Text -> a

   Return box argument by name. It access the storage of key value pairs.

For example we can check that total value of the inputs is greater than 100::

   sum (map getBoxValue getInputs) > 100

Or we can check that all inputs have value greater than 100::

   all (map (\x -> getBoxValue x > 100) getInputs)


Environment 
-----------------------------------

We have already discussed how to read input and output boxes. 
But there are also some global variables that we can read.

.. function:: getHeight :: Int
   
   Height of the blockchain at the time of execution of transaction.

.. function:: getVar :: Text -> a
              getVar argumentKey

   Reads primitive value from the argument list of key-value pairs that is provided with transaction.
   Some scripts can require user to supply additional values with transaction.


Debugging
------------------------------------

Sometimes it is useful to trace the execution of the values in the script.
We can use the function ``trace`` 

.. function:: trace :: Text -> a -> a
   
   Adds text to debug output when second argument is executed.

We can read the debug information in REPL-console. Also the debug is returned with
API call that posts transaction.


Compilation
-----------------------------------

To use script in transaction JSON-object we have to compile it. To do it we invoke
compailer ``hschain-utxo-compiler``::

   cabal new-run hschain-utxo-compiler -- compile --input script.hs --output out.txt

If flag ``output`` (``-o`` for short) is omitted the result is dumped to stdout.
See flag ``--help`` for all options of the compiler.




