-- | Defines G-code set of instructions for G-machine
module Hschain.Utxo.Lang.Core.Gcode(
    Name
  , Gcode(..)
) where

import Data.Text (Text)

type Name = Text

data Gcode
  = Unwind
  | Pushglobal !Name
  | Pushint !Int
  | Push !Int
  | Mkap
  | Slide !Int
  deriving (Show, Eq)


{-
data Gcode f v
  = Begin
  -- ^ Begin of the program
  | End
  -- ^ End of the code
  | Eval
  -- ^ Evaluate the top of the stack
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

  deriving (Show, Eq)
-}
