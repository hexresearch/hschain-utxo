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

-- | Infinite term programm. Evaluation does not terminate.
--
-- F x = x x x
-- main = F F
badProg :: CoreProg
badProg = [main, f]
  where
    f = scomb "F" ["x"] (ap2 "x" "x" "x")
    main = scomb "main" [] (EAp "F" "F")


