square :: (Num a) => a -> a
square n = n * n

isDivisor :: (Integral a) => a -> a -> Bool
isDivisor a b = b `mod` a == 0

findDivisor :: (Integral a) => a -> a -> a
findDivisor divisor n
    | square divisor > n    = n
    | isDivisor divisor n   = divisor
    | otherwise             = findDivisor (divisor + 1) n

smallestDivisor :: (Integral a) => a -> a
smallestDivisor = findDivisor 2

prime :: (Integral a) => a -> Bool
prime n = n == smallestDivisor n

no = prime 4
yes = prime 3