-- | Programms to test G-machine evaluation
module Hschain.Utxo.Lang.Core.Compile.Example where

import Data.Either
import Data.Text (Text)

import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Gmachine
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Core.Compile.Primitives(preludeTypeContext)

import qualified Data.List as L
import qualified Data.Vector as V

import Text.Show.Pretty

---------------------------------------------
-- utils

scomb :: Text -> [Text] -> Expr -> [Type] -> Type -> Scomb
scomb name args body argsT bodyT =
  Scomb name (V.fromList $ zipWith Typed args argsT) (Typed body bodyT)

ap2 :: Expr -> Expr -> Expr -> Expr
ap2 f a b = EAp (EAp f a) b

ap3 :: Expr -> Expr -> Expr -> Expr -> Expr
ap3 f a b c = EAp (ap2 f a b) c

instance Num Expr where
  (+) = ap2 "+"
  (*) = ap2 "*"
  negate = EAp "negate"
  abs = undefined
  signum = undefined
  fromInteger = EPrim . PrimInt . fromInteger

varA, varB, varC, varD :: Type

varA = varT "a"
varB = varT "b"
varC = varT "c"
varD = varT "d"

arrow2 :: Type -> Type -> Type -> Type
arrow2 a b c = arrowT a (arrowT b c)

funT :: [Type] -> Type -> Type
funT args resT = L.foldl' (\res arg -> arrowT arg res) resT args

----------------------------------------------

main :: IO ()
main =
  pPrint $ eval $ compile prog2

-- | Test that all programms terminate without errors.
-- Shows only failed cases. If list is empty then everything is fine.
testAll :: [(Int, Either Error Gmachine)]
testAll = filter (isLeft . snd) $
  zipWith (\n prog -> (n, eval $ compile prog)) [1..] allProgs

-- | Type check all programms and show which one are ill-typed.
typeCheckAll :: [Int]
typeCheckAll = fmap fst $ filter (not . snd) $
  zipWith (\n prog -> (n, typeCheck preludeTypeContext prog)) [1..] allProgs

allProgs :: [CoreProg]
allProgs =
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
    s   = scomb "S"   ["x", "y", "z"]  (ap2 "x" "z" (EAp "y" "z")) [arrow2 varA varB varC, arrowT varA varB,varA] varC
    k   = scomb "K"   ["x", "y"]       "x"  [varA, varB] varA
    k1  = scomb "K1"  ["x", "y"]       "y"  [varA, varB] varB
    id' = scomb "I"   ["x"]            "x"  [varA]       varA

prelude2 :: [Scomb]
prelude2 = [s, k, k1, id', cons, nil]
  where
    s   = scomb "S"   ["x", "y", "z"]  (ap2 "x" "z" (EAp "y" "z")) [arrow2 varA varB varC, arrowT varA varB,varA] varC
    k   = scomb "K"   ["x", "y"]       "x"  [varA, varB] varA
    k1  = scomb "K1"  ["x", "y"]       "y"  [varA, varB] varB
    id' = scomb "I"   ["x"]            "x"  [varA]       varA
    cons = scomb "cons" ["x", "y"] (ap2 (EConstr consTy 2 2) "x" "y") [varA, listT varA] (listT varA)
    nil  = scomb "nil" [] (EConstr (listT varA) 1 0) [] (listT varA)
    consTy = arrow2 varA (listT varA) (listT varA)

-- | Programm
--
-- > main = S K K 3
prog1 :: CoreProg
prog1 = main : prelude
  where
    main = scomb "main" [] (ap3 "S" "K" "K" 3) [] intT

-- | Program
--
-- > twice f x = f (f x)
-- >
-- > main = twice twice I 3
prog2 :: CoreProg
prog2 = [ twice, main ] ++ prelude
  where
    twice = scomb "twice" ["f", "x"] (EAp "f" (EAp "f" "x")) [arrowT varA varA, varA] varA
    main  = scomb "main"  []         (ap3 "twice" "twice" "I" 3) [] intT


prog3 :: CoreProg
prog3 = main : twice : prelude
  where
    twice = scomb "twice" ["f", "x"] (EAp "f" (EAp "f" "x")) [arrowT varA varA, varA] varA
    main = scomb "main" [] (ap2 "twice" (ap2 "I" "I" "I") 3) [] intT

-- | Functional representation of the lists. Test for lazy evaluation.
-- Programm creates infinite list of 4s, and takes second element of the list
-- notice that this program is ill-typed
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
    cons  = scomb "cons" ["a", "b", "cc", "cn"] (ap2 "cc" "a" "b") [varA, varB, arrow2 varA varB varC, varD] varC
    nil   = scomb "nil"  ["cc", "cn"]           "cn" [varA, varB] varB
    hd    = scomb "hd"   ["list"]               (ap2 "list" "K" "abort") [arrow2 (arrow2 varA varB varA) varC varD] varD
    tl    = scomb "tl"   ["list"]               (ap2 "list" "K1" "abort") [arrow2 (arrow2 varA varB varB) varC varD] varD
    abort = scomb "abort" []                    "abort" [] varA
    infinite = scomb "infinite" ["x"]           (ap2 "cons" "x" (EAp "infinite" "x")) [varA] (varB)

    main  = scomb "main" []                     (EAp "hd" (EAp "tl" (EAp "infinite" 4))) [] intT

-- | Simple test for let-expressions
--
-- > main = let id1 = I I I
--          in   id1 id1 id1 3
prog5 :: CoreProg
prog5 = main : prelude
  where
    main = scomb "main" [] (ELet [(Typed "id1" (arrowT varA varA), ap2 "I" "I" "I")] (ap3 "id1" "id1" "id1" 3)) [] intT

-- | Simple arithmetic
--
-- > main = 3 + 4 * 5
prog6 :: CoreProg
prog6 = [main]
  where
    main = scomb "main" [] (3 + 4 * 5) [] intT


-- | Simple arithmetic
--
-- > main = let x = 3 * (2 + 2) in x * x
prog7 :: CoreProg
prog7 = [main]
  where
    main = scomb "main" [] (ELet [(Typed "x" intT, 3 * (2 + 2))] ("x" * "x")) [] intT

prog8 :: CoreProg
prog8 = [main, inc, twice] ++ prelude
  where
    inc = scomb "inc" ["x"] ("x" + 1) [intT] intT
    twice = scomb "twice" ["f", "x"] (EAp "f" (EAp "f" "x")) [arrowT varA varA, varA] varA
    main = scomb "main" [] (ap3 "twice" "twice" "inc" 4) [] intT

-- | Test for arithmetic. Length of the list
-- ill-typed program
--
-- > length xs = xs length1 0
-- > length1 x xs = 1 + (length xs)
-- >
-- > main = length (cons 3 (cons 3 (cons 3 nil)))
prog9 :: CoreProg
prog9 = [main, length', length1, cons, nil] ++ prelude
  where
    cons  = scomb "cons" ["a", "b", "cc", "cn"] (ap2 "cc" "a" "b") [varA, varB, varC, varD] varA
    nil   = scomb "nil"  ["cc", "cn"]           "cn" [varA, varB] varC
    length' = scomb "length" ["xs"] (ap2 "xs" "length1" 0) [varA] varB
    length1 = scomb "length1" ["x", "xs"] (1 + (EAp "length" "xs")) [varA, varB] varC
    main = scomb "main" [] (EAp "length" (ap2 "cons" 3 (ap2 "cons" 3 (ap2 "cons" 3 "nil")))) [] varA

-- | With conditionals. Factorial
--
-- > fac n = if (n == 0) 1 (n * fac (n - 1))
-- > main = fac 10
prog10 = [main, fac]
  where
    fac  = scomb "fac"  ["n"] (ap3 "if" (ap2 "==" "n" 0) 1 ("n" * (EAp "fac" ("n" - 1))) ) [intT] intT
    main = scomb "main" [] (EAp "fac" 10) [] intT

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
                 [intT, intT]
                 intT
    main = scomb "main" [] (ap2 "gcd" 6 10) [] intT

-- | Fibonacci numbers
--
-- > nfib n = if (n <= 0) 1 (1 + nfib (n - 1) + nfib (n - 2))
-- > main = nfib 5
prog12 = [main, nfib]
  where
    nfib = scomb "nfib" ["n"] (ap3 "if"
                                   (ap2 "<=" "n" 0)
                                   1
                                   (1 +
                                         ( (EAp "nfib" ("n" - 1))
                                         + (EAp "nfib" ("n" - 2))  )))
                 [intT]
                 intT
    main = scomb "main" [] (EAp "nfib" 5) [] intT

-- | Infinite term programm. Evaluation does not terminate.
-- ill-typed programm
--
-- F x = x x x
-- main = F F
badProg :: CoreProg
badProg = [main, f]
  where
    f = scomb "F" ["x"] (ap2 "x" "x" "x") [varA] varB
    main = scomb "main" [] (EAp "F" "F") [] varA

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
    main = scomb "main" [] (EAp "downfrom" 5) [] (listT intT)
    downfrom = scomb "downfrom" ["n"]
        (ap3 "if"
              (ap2 "==" "n" 0)
              "nil"
              (ap2 "cons" "n" (EAp "downfrom" (ap2 "-" "n" 1)))
        )
        [intT] (listT intT)


-- | Program that takes first 5 elements of infinite list of integers.
--
-- > main = take 5 (from 0)
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
    main = scomb "main" [] (ap2 "take" 5 (EAp "from" 0)) [] (listT intT)
    take' = scomb "take" ["n", "xs"]
      (ap3 "if"
           (ap2 "<=" "n" 0)
           "nil"
           (ECase (Typed "xs" (listT varA))
                       [ CaseAlt 1 [] "nil"
                       , CaseAlt 2 [ Typed "p" varA
                                   , Typed "ps" (listT varA)] (ap2 "cons" "p" (ap2 "take" (ap2 "-" "n" 1) "ps"))
                        ])
           )
          [intT, listT varA] (listT varA)
    from = scomb "from" ["a"] (ap2 "cons" "a" (EAp "from" (ap2 "+" "a" 1)))
              [intT] (listT intT)


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
    main = scomb "main" [] (ap2 "take" 5 (EAp "sieve" (EAp "from" 2))) [] (listT intT)

    nonMultiple = scomb "nonMultiple" ["p", "n"] (ap2 "/=" (ap2 "*" (ap2 "/" "n" "p") "p") "n")
                        [intT, intT] boolT

    take' = scomb "take" ["n", "xs"]
      (ap3 "if"
           (ap2 "<=" "n" 0)
           "nil"
           (ECase (Typed "xs" (listT varA))
                       [ CaseAlt 1 [] "nil"
                       , CaseAlt 2 [ Typed "p" varA
                                   , Typed "ps" (listT varA)] (ap2 "cons" "p" (ap2 "take" (ap2 "-" "n" 1) "ps"))
                        ])
           )
      [intT, listT varA] (listT varA)

    from = scomb "from" ["a"] (ap2 "cons" "a" (EAp "from" (ap2 "+" "a" 1)))
                [intT] (listT intT)

    filter' = scomb "filter" ["predicate", "xs"]
      (ECase (Typed "xs" (listT varA))
                  [ CaseAlt 1 [] "nil"
                  , CaseAlt 2 [ Typed "p" varA
                              , Typed "ps" (listT varA)]
                                        (ELet [(Typed "rest" (listT varA), (ap2 "filter" "predicate" "ps"))]
                                           (ap3 "if"
                                                 (EAp "predicate" "p")
                                                 (ap2 "cons" "p" "rest")
                                                 "rest"
                                           ))
                  ])
          [arrowT varA boolT, listT varA] (listT varA)
    sieve = scomb "sieve" ["xs"]
      (ECase (Typed "xs" (listT intT))
                  [ CaseAlt 1 [] "nil"
                  , CaseAlt 2 [ Typed "p" intT
                              , Typed "ps" (listT intT)] (ap2 "cons" "p" (EAp "sieve"
                                                                (ap2 "filter"
                                                                      (EAp "nonMultiple" "p")
                                                                      "ps")))
                  ])
                [listT intT] (listT intT)

---------------------------------------------
-- booleans

-- | Simple boolean expression
--
-- > main = not (true && false)
prog16 :: CoreProg
prog16 = [main] ++ prelude2
  where
    main = scomb "main" [] (EAp "not" (ap2 "&&" "true" "false")) [] boolT

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
            []
            (listT boolT)

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
        []
        (listT boolT)

