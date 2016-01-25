-- Example: toDigits 1234 == [1,2,3,4]
-- Example: toDigitsRev 1234 == [4,3,2,1]
-- Example: toDigits 0 == []
-- Example: toDigits (-17) == []toDigits :: Integer -> [Integer]

toDigits n
  | n > 0 = map (\x -> read[x] :: Integer) (show n)
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev n =
    reverse(toDigits n)

-- Example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
-- Example: doubleEveryOther [1,2,3] == [1,4,3]
doubleEveryOther :: [Integer] -> [Integer]
-- doubleEveryOther [] = []
-- doubleEveryOther (x1:x2:xs) = x1 : (x2*2) : doubleEveryOther xs
-- doubleEveryOther (x:xs) = x : []
doubleEveryOther xs = zipWith (*) (cycle[1,2]) (xs)

-- sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ map (sum . toDigits) xs

validate :: Integer -> Bool
validate n
  | (sumDigits . doubleEveryOther . toDigits) n `mod` 10 == 0 = True
  | otherwise = False
