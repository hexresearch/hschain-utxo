-- | Programms to test G-machine evaluation
module Gmachine where

import Data.String
import Data.Text (Text)

import Hschain.Utxo.Lang.Core.Compile.Prog
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
-- > main = 2 + 2
prog6 :: CoreProg
prog6 = [main]
  where
    main = scomb "main" [] (ap2 "+" (ENum 2) (ENum 2))


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
-- > main = fac 5
prog10 = [main, fac]
  where
    fac  = scomb "fac"  ["n"] (ap3 "if" (ap2 "==" "n" (ENum 0)) (ENum 1) (ap2 "*" "n" (EAp "fac" (ap2 "-" "n" (ENum 1)))) )
    main = scomb "main" [] (EAp "fac" (ENum 10))

-- | Infinite term programm. Evaluation does not terminate.
--
-- F x = x x x
-- main = F F
badProg :: CoreProg
badProg = [main, f]
  where
    f = scomb "F" ["x"] (ap2 "x" "x" "x")
    main = scomb "main" [] (EAp "F" "F")


