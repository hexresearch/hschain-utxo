Core-language
================================

TODO: this is a stub of article on Core-language

The core is a low-level representation of our script that is suitable for
fast execution. Transactions contain only core-language programms.
We compile our scripts to this language with hschain-utxo-compiler.

Our core language is a monomorphic typed-lambda-calculus without anonymous lambda-abstraction.
All lambdas should be globaly defined.

The program on the Core language contains a list of definitions called supercombinators.
Suoercombinator is a function with arguments that contain no free variables beside the
references to other supercombinators.

Examples of supercombinators::

   add :: Int -> Int -> Int
   add x y = x + y

   main :: Bool
   main = add 2 2 > 3

Recursion is prohibited in our language so we can not have cyclic dependencies.

The program to be a script should be 

   * well typed 

   * types should be monomorphic

   * it should contain no cyclic dependencies

   * it should have constant called ``main`` that evaluates to sigma-expression or boolean.

For fast type-checking all top-level functions and let-bindings should be explicitly typed by a programmer.
In our scripting-language it's not the case but compiler can infer all types for us.

TODO: more descriptions


