module Operation where

infixl 5 `plus`
plus :: Integer -> Integer -> Integer
plus = (+)

infixl 5 `multipl`
multipl :: Integer -> Integer -> Integer
multipl = (*)