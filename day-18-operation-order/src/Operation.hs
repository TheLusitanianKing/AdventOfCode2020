module Operation (plus, plus', multipl, multipl') where
-- using custom operators with infixl to make it work

-- part 1 operators
infixl 5 `plus`
plus :: Integer -> Integer -> Integer
plus = (+)

infixl 5 `multipl`
multipl :: Integer -> Integer -> Integer
multipl = (*)

-- part 2 operators
infixl 6 `plus'`
plus' :: Integer -> Integer -> Integer
plus' = (+)

infixl 5 `multipl'`
multipl' :: Integer -> Integer -> Integer
multipl' = (*)