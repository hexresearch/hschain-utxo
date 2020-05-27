-- | Programms to test G-machine evaluation
module Gmachine where

import Data.Either
import Data.String
import Data.Text (Text)

import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Gmachine

import qualified Data.Vector as V

import Text.Show.Pretty

---------------------------------------------
-- utils

scomb :: Text -> [Text] -> Expr -> Scomb
scomb name args body = Scomb name (V.fromList args) body

ap2 :: Expr -> Expr -> Expr -> Expr
ap2 f a b = EAp (EAp f a) b

ap3 :: Expr -> Expr -> Expr -> Expr -> Expr
ap3 f a b c = EAp (ap2 f a b) c

instance IsString Expr where
  fromString = EVar . fromString

----------------------------------------------

main =
  pPrint $ eval $ compile prog2

-- | Test that all programms terminate without errors.
testAll = all (isRight . eval . compile)
  [ prog1, prog2, prog3, prog4, prog5, prog6
  , prog7, prog8, prog9, prog10, prog11, prog12
  , prog13, prog14, prog15
  , prog16, prog17, prog18 ]

-- Prelude functions
--
-- > id x = x
-- > S x y z = x z (y z)
-- > K x y = x
prelude :: [Scomb]
prelude = [s, k, k1, id']
  where
    s   = scomb "S"   ["x", "y", "z"]  (ap2 "x" "z" (EAp "y" "z"))
    k   = scomb "K"   ["x", "y"]       "x"
    k1  = scomb "K1"  ["x", "y"]       "y"
    id' = scomb "I"   ["x"]            "x"

prelude2 :: [Scomb]
prelude2 = [s, k, k1, id', cons, nil]
  where
    s   = scomb "S"   ["x", "y", "z"]  (ap2 "x" "z" (EAp "y" "z"))
    k   = scomb "K"   ["x", "y"]       "x"
    k1  = scomb "K1"  ["x", "y"]       "y"
    id' = scomb "I"   ["x"]            "x"
    cons = scomb "cons" ["x", "y"] (ap2 (EConstr 2 2) "x" "y")
    nil  = scomb "nil" [] (EConstr 1 0)

-- | Programm
--
-- > main = S K K 3
prog1 :: CoreProg
prog1 = main : prelude
  where
    main = scomb "main" [] (ap3 "S" "K" "K" (ENum 3))

-- | Program
--
-- > twice f x = f (f x)
-- >
-- > main = twice twice I 3
prog2 :: CoreProg
prog2 = [ twice, main ] ++ prelude
  where
    twice = scomb "twice" ["f", "x"] (EAp "f" (EAp "f" "x"))
    main  = scomb "main"  []         (ap3 "twice" "twice" "I" (ENum 3))


prog3 :: CoreProg
prog3 = main : twice : prelude
  where
    twice = scomb "twice" ["f", "x"] (EAp "f" (EAp "f" "x"))
    main = scomb "main" [] (ap2 "twice" (ap2 "I" "I" "I") (ENum 3))

-- | Functional representation of the lists. Test for lazy evaluation.
-- Programm creates infinite list of 4s, and takes second element of the list
--
-- > cons a b cc cn = cc a b
-- > nil cc cn = cn
-- > hd list = list K abort
-- > tl list = list K1 abort
-- > abort = abort
-- > infinite x = cons x (infinite x)
-- >
-- main = hd (tl (infinite 4))
prog4 :: CoreProg
prog4 = [cons, nil, hd, tl, abort, infinite, main] ++ prelude
  where
    cons  = scomb "cons" ["a", "b", "cc", "cn"] (ap2 "cc" "a" "b")
    nil   = scomb "nil"  ["cc", "cn"]           "cn"
    hd    = scomb "hd"   ["list"]               (ap2 "list" "K" "abort")
    tl    = scomb "tl"   ["list"]               (ap2 "list" "K1" "abort")
    abort = scomb "abort" []                    "abort"
    infinite = scomb "infinite" ["x"]           (ap2 "cons" "x" (EAp "infinite" "x"))

    main  = scomb "main" []                     (EAp "hd" (EAp "tl" (EAp "infinite" (ENum 4))))



-- | Simple test for let-expressions
--
-- > main = let id1 = I I I
--          in   id1 id1 id1 3
prog5 :: CoreProg
prog5 = main : prelude
  where
    main = scomb "main" [] (ELet [("id1", ap2 "I" "I" "I")] (ap3 "id1" "id1" "id1" (ENum 3)))

-- | Simple arithmetic
--
-- > main = 3 + 4 * 5
prog6 :: CoreProg
prog6 = [main]
  where
    main = scomb "main" [] (ap2 "+" (ENum 3) (ap2 "*" (ENum 4) (ENum 5)))


-- | Simple arithmetic
--
-- > main = let x = 3 * (2 + 2) in x * x
prog7 :: CoreProg
prog7 = [main]
  where
    main = scomb "main" [] (ELet [("x", ap2 "*" (ENum 3) (ap2 "+" (ENum 2) (ENum 2)))] (ap2 "*" "x" "x"))

prog8 :: CoreProg
prog8 = [main, inc, twice] ++ prelude
  where
    inc = scomb "inc" ["x"] (ap2 "+" "x" (ENum 1))
    twice = scomb "twice" ["f", "x"] (EAp "f" (EAp "f" "x"))
    main = scomb "main" [] (ap3 "twice" "twice" "inc" (ENum 4))

-- | Test for arithmetic. Length of the list
--
-- > length xs = xs length1 0
-- > length1 x xs = 1 + (length xs)
-- >
-- > main = length (cons 3 (cons 3 (cons 3 nil)))
prog9 :: CoreProg
prog9 = [main, length', length1, cons, nil] ++ prelude
  where
    cons  = scomb "cons" ["a", "b", "cc", "cn"] (ap2 "cc" "a" "b")
    nil   = scomb "nil"  ["cc", "cn"]           "cn"
    length' = scomb "length" ["xs"] (ap2 "xs" "length1" (ENum 0))
    length1 = scomb "length1" ["x", "xs"] (ap2 "+" (ENum 1) (EAp "length" "xs"))
    main = scomb "main" [] (EAp "length" (ap2 "cons" (ENum 3) (ap2 "cons" (ENum 3) (ap2 "cons" (ENum 3) "nil"))))

-- | With conditionals. Factorial
--
-- > fac n = if (n == 0) 1 (n * fac (n - 1))
-- > main = fac 10
prog10 = [main, fac]
  where
    fac  = scomb "fac"  ["n"] (ap3 "if" (ap2 "==" "n" (ENum 0)) (ENum 1) (ap2 "*" "n" (EAp "fac" (ap2 "-" "n" (ENum 1)))) )
    main = scomb "main" [] (EAp "fac" (ENum 10))

-- > gcd a b = if (a == b)
-- >              a
-- >              if (a < b)
-- >                 (gcd b a) (gcd b (a - b))
-- >
-- > main = gcd 6 10
prog11 = [main, gcd]
  where
    gcd  = scomb "gcd"  ["a", "b"] (ap3 "if"
                                          (ap2 "==" "a" "b")
                                          "a"
                                          (ap3 "if"
                                                (ap2 "<" "a" "b")
                                                (ap2 "gcd" "b" "a")
                                                (ap2 "gcd" "b" (ap2 "-" "a" "b")) ))
    main = scomb "main" [] (ap2 "gcd" (ENum 6) (ENum 10))

-- | Fibonacci numbers
--
-- > nfib n = if (n <= 0) 1 (1 + nfib (n - 1) + nfib (n - 2))
-- > main = nfib 5
prog12 = [main, nfib]
  where
    nfib = scomb "nfib" ["n"] (ap3 "if"
                                   (ap2 "<=" "n" (ENum 0))
                                   (ENum 1)
                                   (ap2 "+"
                                         (ENum 1)
                                         (ap2 "+"
                                               (EAp "nfib" (ap2 "-" "n" (ENum 1)))
                                               (EAp "nfib" (ap2 "-" "n" (ENum 2)))  )))
    main = scomb "main" [] (EAp "nfib" (ENum 5))

-- | Infinite term programm. Evaluation does not terminate.
--
-- F x = x x x
-- main = F F
badProg :: CoreProg
badProg = [main, f]
  where
    f = scomb "F" ["x"] (ap2 "x" "x" "x")
    main = scomb "main" [] (EAp "F" "F")

---------------------------------------------
-- data structures. we use prelude2 where there are supercombinators cons and nil
-- as synonyms for @(EConstr 2 2)@ and @(EConstr 1 0)@

-- | Contructs countdown list
--
-- > downfrom n = if (n == 0)
-- >                 nil
-- >                 cons n (downfrom (n - 1))
-- >
-- > main = downfrom 5
prog13 :: CoreProg
prog13 = [main, downfrom] ++ prelude2
  where
    main = scomb "main" [] (EAp "downfrom" (ENum 5))
    downfrom = scomb "downfrom" ["n"]
        (ap3 "if"
              (ap2 "==" "n" (ENum 0))
              "nil"
              (ap2 "cons" "n" (EAp "downfrom" (ap2 "-" "n" (ENum 1))))
        )


-- | Program that takes first 5 elements of infinite list of twos.
--
-- > main = take 5 (repeat 2)
-- >
-- > take n xs = if (n <= 0)
-- >                nil
-- >                (case xs of
-- >                   <1>      -> nil;
-- >                   <2> p ps -> cons p (take (n - 1) ps)
-- >                )
-- >
-- > from a = cons a (from (a + 1))
prog14 :: CoreProg
prog14 = [main, take', from] ++ prelude2
  where
    main = scomb "main" [] (ap2 "take" (ENum 5) (EAp "from" (ENum 0)))
    take' = scomb "take" ["n", "xs"]
      (ap3 "if"
           (ap2 "<=" "n" (ENum 0))
           "nil"
           (ECase "xs" [ CaseAlt 1 [] "nil"
                       , CaseAlt 2 ["p", "ps"] (ap2 "cons" "p" (ap2 "take" (ap2 "-" "n" (ENum 1)) "ps"))
                        ])
           )
    from = scomb "from" ["a"] (ap2 "cons" "a" (EAp "from" (ap2 "+" "a" (ENum 1))))


-- | Eratosthenes sieve. Prints list of prime numbers
--
-- > main = take 3 (sieve (from 2))
-- >
-- > from a = cons a (from (a + 1))
-- >
-- > sieve xs = case xs of
-- >              <1>      -> nil;
-- >              <2> p ps -> cons p (sieve (filter (nonMultiple p) ps))
-- >
-- > filter predicate xs
-- >   = case xs of
-- >        <1>      -> nil;
-- >        <2> p ps -> let rest = filter predicate ps
-- >                    in  if (predicate p) (cons p rest) rest;
-- >
-- > nonMultiple p n = ((n / p) * p) /= n
-- >
-- > take n xs = if (n <= 0)
-- >                nil
-- >                (case xs of
-- >                   <1>      -> nil;
-- >                   <2> p ps -> cons p (take (n - 1) ps)
-- >                )
prog15 :: CoreProg
prog15 = [main, take', from, sieve, filter', nonMultiple] ++ prelude2
  where
    main = scomb "main" [] (ap2 "take" (ENum 5) (EAp "sieve" (EAp "from" (ENum 2))))

    nonMultiple = scomb "nonMultiple" ["p", "n"] (ap2 "/=" (ap2 "*" (ap2 "/" "n" "p") "p") "n")

    take' = scomb "take" ["n", "xs"]
      (ap3 "if"
           (ap2 "<=" "n" (ENum 0))
           "nil"
           (ECase "xs" [ CaseAlt 1 [] "nil"
                       , CaseAlt 2 ["p", "ps"] (ap2 "cons" "p" (ap2 "take" (ap2 "-" "n" (ENum 1)) "ps"))
                        ])
           )
    from = scomb "from" ["a"] (ap2 "cons" "a" (EAp "from" (ap2 "+" "a" (ENum 1))))

    filter' = scomb "filter" ["predicate", "xs"]
      (ECase "xs" [ CaseAlt 1 [] "nil"
                  , CaseAlt 2 ["p", "ps"] (ELet [("rest", (ap2 "filter" "predicate" "ps"))]
                                           (ap3 "if"
                                                 (EAp "predicate" "p")
                                                 (ap2 "cons" "p" "rest")
                                                 "rest"
                                           ))
                  ])
    sieve = scomb "sieve" ["xs"]
      (ECase "xs" [ CaseAlt 1 [] "nil"
                  , CaseAlt 2 ["p", "ps"] (ap2 "cons" "p" (EAp "sieve"
                                                                (ap2 "filter"
                                                                      (EAp "nonMultiple" "p")
                                                                      "ps")))
                  ])

---------------------------------------------
-- booleans

-- | Simple boolean expression
--
-- > main = not (true && false)
prog16 :: CoreProg
prog16 = [main] ++ prelude2
  where
    main = scomb "main" [] (EAp "not" (ap2 "&&" "true" "false"))

-- | Table of values for && function
--
-- main = [ true && true
--        , true && false
--        , false && true
--        , false && false ]
prog17 :: CoreProg
prog17 = [main] ++ prelude2
  where
    main = scomb "main" []
        (ap2 "cons"
              (ap2 "&&" "true" "true")
              (ap2 "cons"
                  (ap2 "&&" "true" "false")
                  (ap2 "cons"
                        (ap2 "&&" "false" "true")
                        (ap2 "cons"
                              (ap2 "&&" "false" "false")
                              "nil"
                        )
                  )
              ))

-- | Table of values for || function
--
-- main = [ true || true
--        , true || false
--        , false || true
--        , false || false ]
prog18 :: CoreProg
prog18 = [main] ++ prelude2
  where
    main = scomb "main" []
        (ap2 "cons"
              (ap2 "||" "true" "true")
              (ap2 "cons"
                  (ap2 "||" "true" "false")
                  (ap2 "cons"
                        (ap2 "||" "false" "true")
                        (ap2 "cons"
                              (ap2 "||" "false" "false")
                              "nil"
                        )
                  )
              ))





