-- | Module defines code for supercombinators.
-- Superconbinators can be used as core-language.
module Hschain.Utxo.Lang.Core.Scomb(
) where

import Data.Fix
import Data.Text (Text)
import Data.Vector (Vector)

import Data.Map.Strict (Map)

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

data Type = Type

data Typed a = Typed
  { typed'type :: Type
  , typed'val  :: a
  }

-- | Program that is based on supercombinator language.
-- It's parametrised by the functor of primitive expressions.
data Prog f v = Prog
  { prog'binds   :: Map v (Scomb f v)  -- ^ definitions for all scombinators of the program
  , prog'main    :: v                  -- ^ main expression
  }


-- | Supercobinators do not contain free variables.
--
-- > S a1 a2 a3 = expr
data Scomb f v = Scomb
  { scomb'name :: v              -- ^ name
  , scomb'args :: Vector v       -- ^ list of arguments
  , scomb'body :: ScombExpr f v  -- ^ body
  }

type ScombExpr f v = Fix (E f v)

data E prim ident a
  = Prim prim              -- ^ constants and built-in expressions of the language
  | Comb ident             -- ^ reference to globally defined combinator
  | Var ident              -- ^ references to variables and other supercombinators
  | App a a                -- ^ application
  | Let (Typed ident) a a  -- ^ local variables
  deriving (Functor, Traversable, Foldable)

data Prim
  = PrimInt  !Int         -- ^ constant integers
  | PrimBool !Bool        -- ^ constant booleans
  | PrimText !Text        -- ^ constant texts
  | Add                   -- ^ integer addition
  | Mul                   -- ^ integer multiplication
  | Neg                   -- ^ integer negation
  | And                   -- ^ boolean AND
  | Or                    -- ^ boolean OR
  | Not                   -- ^ boolean negation
  | If                    -- ^ if-expressions

class IsPrim p where
  isConst :: p -> Bool

data Error ident
  = NameNotFound ident

{-
-- | Eval takes in function to evaluate primitive expressions
-- context and evaluates
eval :: (f (Fix f) -> Fix f)
  -> Prog f v -> Either (Error v) (Fix f)
eval getPrim Prog{..} = evalScomb prog'binds prog'main
  where
    evalScomb combs name = undefined
-}


data Gcode f v
  = Begin
  -- ^ Begin of the program
  | End
  -- ^ End of the code
  | GlobStart v !Int
  -- ^ Start of the execution of supercombinator
  -- It keeps name of the combinator and the size of its arguments
  | Update !Int
  -- ^ Update the root of the redex
  | Pop !Int
  -- ^ pops N elements from the stack
  | Push !Int
  -- ^ Pushes Nth element of the stack on top of it
  | Slide !Int
  -- ^ squeeze out Nth element from the stack
  | Mkap
  -- ^ Make application. Pops two element from the stack
  -- makes application on the heap and pushes pointer to it
  -- on top of the stack
  | Unwind
  -- ^ Continue the evaluation
  | Return
  -- ^ Like unwind but assumes that expression is in WHNF and does not performs
  -- check for it.
  | PushConst f
  -- ^ push constant on top of the stack
  | PushBuiltIn f
  -- ^ push built-in function on top of the stack
  | PushGlobal v
  -- ^ push combinator name on top of the stack
  | Eval
  -- ^ Evaluate the top of the stack

  deriving (Show, Eq)

compR :: forall f v . (Ord v, IsPrim f) => Scomb f v -> Either (Error v) [Gcode f v]
compR Scomb{..} =
  fmap (GlobStart scomb'name n : ) $ fromBody scomb'body rho n
  where
    n = V.length scomb'args
    rho = M.fromList $ zip (V.toList scomb'args) [n, n-1 ..]

    fromBody :: ScombExpr f v -> Map v Int -> Int -> Either (Error v) [Gcode f v]
    fromBody expr r d = fmap (\c -> c <> [Update (d + 1), Pop d, Unwind]) (compC expr r d)


compC :: (Ord v, IsPrim f) => ScombExpr f v -> Map v Int -> Int -> Either (Error v) [Gcode f v]
compC (Fix x) rho d = case x of
  Prim p | isConst p -> pure [PushConst p]
  Prim p             -> pure [PushBuiltIn p]
  Comb a             -> pure [PushGlobal a]
  Var a              -> fmap (\n -> [Push (d - n)]) $ getVar a
  App f a            -> do
    fCode <- compC f rho (d + 1)
    aCode <- compC a rho d
    return $ aCode <> fCode <> [Mkap]
  Let tVar expr body -> do
    exprCode <- compC expr rho d
    bodyCode <- compC body (M.insert (typed'val tVar) (d + 1) rho) (d + 1)
    return $ exprCode <> bodyCode <> [Slide 1]
  where
    getVar var = maybe (Left $ NameNotFound var) Right $ M.lookup var rho








