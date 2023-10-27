import System.IO  

main = do  
    handle <- openFile "data/2020/day1.txt" ReadMode  
    contents <- hGetContents handle  
    print $ sums2To2020 (map read $ lines contents)
    print $ sums3To2020 (map read $ lines contents)
    hClose handle  

sums2To2020 :: [Int] -> Int
sums2To2020 lst = head [x * y | x <- lst, y <- lst, x + y  == 2020]

sums3To2020 :: [Int] -> Int
sums3To2020 lst = head [x * y * z | x <- lst, y <- lst, z <- lst, x + y + z  == 2020]
