module Type.Binding where

import Control.Monad

import Type.ClassEnv
import Type.Infer
import Type.Subst
import Type.Type
import Type.Expr

import qualified Data.List as L

-- type Infer e t = ClassEnv → [Assump] → e → TI ([Pred ], t)


