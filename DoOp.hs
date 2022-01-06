myElem :: Eq a => a -> [a] -> Bool
myElem x [] = False
myElem x (a:as) | a == x = True
             | otherwise = myElem x as

safeDiv :: Int -> Int -> Maybe Int
safeDiv 0 b = Nothing
safeDiv a 0 = Nothing
safeDiv a b | (div a b) == 0 = Nothing
            | otherwise = Just (div a b)

safeNth :: [a] -> Int -> Maybe a
safeNth [] x = Nothing
safeNth [a] 0 = Nothing
safeNth [a] x | x < 0 = Nothing
              | x  > length [a] = Nothing
              | otherwise = Just ([a] !! x)

safeSucc :: Maybe Int -> Maybe Int
safeSucc Nothing = Nothing
safeSucc (Just x) = Just (x + 1)

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup x [] = Nothing
myLookup x ((a,b):ab) | a == x = Just b
                      | otherwise = myLookup x ab

maybeDo :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c