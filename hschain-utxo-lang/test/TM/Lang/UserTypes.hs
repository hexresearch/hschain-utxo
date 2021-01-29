module TM.Lang.UserTypes(
    tests
  , prog1
  , prog2
  , maybeProg
  , encodeUserType
) where

import Hschain.Utxo.Lang
import Test.Tasty
import TM.Core.Common (testScript)

tests :: TestTree
tests = testGroup "User types"
  [ testScript "Prog with user types"    prog1
  , testScript "Prog with record types"  prog2
  , testScript "Prog with maybe types"   maybeProg
  , testScript "Encode user type"        encodeUserType
  , testScript "Equality for user type"  genEquality
  ]

-- | Program with user-defined data-types.
prog1 :: Module
prog1 = [utxoModule|

data User = User Text Int Gender Bytes

data Gender = Male | Female

john = User "John" 18 Male (bytes "123")
mary = User "Mary" 22 Female (bytes "3456")

check (User name age gender key) =
  toSigma ((age > 20) && isMale gender) &&* pk key

isMale x = case x of
  Male   -> True
  Female -> False

main = check john ||* check mary
|]

-- | Program with records
prog2 :: Module
prog2 = [utxoModule|

data User = User
  { user'name :: Text
  , user'age  :: Int
  , user'key  :: Bytes
  }

john = User
  { user'name = "John"
  , user'age  = 18
  , user'key  = bytes "123"
  }

check x = user'age x >* 18 &&* pk (user'key x)

birthdayUpdate x = x { user'age = user'age x + 1 }

main = check (birthdayUpdate john)
|]

maybeProg :: Module
maybeProg = [utxoModule|

mKey = Just (bytes "1234")

main = pk (fromJust mKey)
|]


encodeUserType :: Module
encodeUserType = [utxoModule|

data Wrap a = Wrap
  { wrap'value   :: a
  , wrap'message :: Text
  }

data User = User
  { user'name :: Text
  , user'age  :: Int
  , user'key  :: Bytes
  }

john = User
  { user'name = "John"
  , user'age  = 18
  , user'key  = bytes "123"
  }

johnBytes = serialise (Wrap john "ho")

main = user'name (wrap'value (deserialise johnBytes :: Wrap User)) == "john"
|]


genEquality :: Module
genEquality = [utxoModule|

data User = User
  { user'name :: Text
  , user'age  :: Int
  }

john = User "john" 12
mary = User "mary" 10

main = john == mary
|]

