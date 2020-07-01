data Wallet = Wallet Text Int

data Maybe a = Nothing | Just a

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f x = case x of
  Nothing -> Nothing
  Just a  -> Just (f a)

fromMaybe :: a -> Maybe a -> a
fromMaybe n x = case x of
  Just a  -> a
  Nothing -> n

data Color
  = Red
  | Green
  | Blue
  | Gen Int Int Int

colToNum :: Color -> Int
colToNum x = case x of
  Red -> 1
--  Green -> 2
--  Blue -> 3
--  Gen r g b -> 100 * r + 10 * g + b
  _ -> 5

double f x = f (f x)

constInt :: Text -> Int -> Int
constInt a b = b + 1

walletName :: Wallet -> Text
walletName x = case x of
  Wallet name _ -> name

walletMoney :: Wallet -> Int
walletMoney x = case x of
  Wallet _ money -> money

t3_1 :: (a, b, c) -> a
t3_1 x = case x of
  (a, b, c) -> a

-- t3_2 :: (a, b, c) -> b
t3_2 x = case x of
  (a, b, c) -> b

q x = p x + 2

p :: Int -> Int
p x = x

xorC :: Bool -> Bool -> Bool
xorC x y = case (x, y) of
  (True, True)   -> True
  (False, False) -> True
  (True,  False) -> False
  (False, True)  -> False

{-

xorCol :: Color -> Color -> Bool
xorCol Red Red = True
xorCol Green Green = True
xorCol _ _ = False
-- xorCol Red Green = False
-- xorCol Green Red = False

twistedSucc :: Int -> Int
twistedSucc 1 = 1
twistedSucc n = n + 1
-}

data User = User
  { user'name  :: Text
  , user'age   :: Int
  , user'goods :: Goods
  }

data Goods = Goods
  { goods'cash  :: Int
  , goods'accs  :: Int
  }


john :: User
john = User { user'name = "john", user'age = 23, user'goods = Goods 1 2 }

setName :: Text -> User -> User
setName name user = user { user'name = name }

-- sumPair (a, b) = a + b
