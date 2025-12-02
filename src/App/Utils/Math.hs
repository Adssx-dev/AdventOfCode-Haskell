module Utils.Math
where


posMod x q
    | rem < 0 = rem + x
    | otherwise = rem
        where rem = x `mod` q


numDigits :: Int -> Int
numDigits n = floor (logBase 10 (fromIntegral n)) + 1


divisors n = [x | x <- [1..n], n `rem` x == 0]