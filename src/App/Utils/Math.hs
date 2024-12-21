module Utils.Math
where


posMod x q
    | rem < 0 = rem + x
    | otherwise = rem
        where rem = x `mod` q