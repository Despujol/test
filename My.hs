mySucc :: Int -> Int
mySucc x = x - 1

myIsNeg :: Int -> Bool
myIsNeg a = if a < 0 then True
            else False

myAbs :: Int -> Int
myAbs x = if x < 0 then -x
          else x

myMin :: Int -> Int -> Int
myMin x y = if x > y then y
            else x

myMax :: Int -> Int -> Int
myMax x y = if x > y then x
            else y

myTuple :: a -> b -> (a, b)
myTuple a b = (a, b)

myTruple :: a -> b -> c -> (a, b, c)
myTruple a b c = (a, b, c)

myFst :: (a, b) -> a
myFst (a, b) = a

mySnd :: (a, b) -> b
mySnd (a, b) = b

mySwap :: (a, b) -> (b, a)
mySwap (a, b) = (b, a)

myHead :: [a] -> a
myHead (a:_) = a
myHead [] = error "empty list"

myTail :: [a] -> [a]
myTail (_:a) = a
myTail [] = error "empty list"

myLength :: [a] -> Int
myLength [] = 0
myLength (_:a) = 1 + myLength a

myNth :: [a] -> Int -> a
myNth [] _ = error "error"
myNth (a:as) 0 = a
myNth (a:as) n = myNth as (n - 1)

myTake :: Int -> [a] -> [a]
myTake 0 [] = error "error"
myTake 0 (a:as) = (a:as)
myTake n (a:as) = myTake (n - 1) (myTail (a:as))
