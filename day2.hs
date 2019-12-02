import Input

-- * Part One
-- | Go 4 by for and return

day2 :: Int -> Int -> [Int] -> Int
day2 n v = day2Aux1 0 . updateList 1 n . updateList 2 v

day2Aux1 :: Int -> [Int] -> Int
day2Aux1 i l = if length z >= 4 then day2Aux1 (i + 4) uL else l !! 0
    where z = take 4 (snd (splitAt i l))
          uL = day2Aux z l

day2Aux :: [Int] -> [Int] -> [Int]
day2Aux z l = case a of
    1 -> updateList d (b + c) l
    2 -> updateList d (b * c) l
    _ -> l
    where ( a : bc : cc : d : t) = z
          b = l !! bc
          c = l !! cc


updateList :: Int -> a -> [a] -> [a]
updateList _ _ [] = []
updateList 0 x (h:t) = x:t
updateList i x (h:t) = h : updateList (i-1) x t


-- * Part Two

dayP2 :: Int -> [Int] -> Int
dayP2 o l = z
    where  z = day2P2Aux o l s 0
           s = read (take 2 (show o)) :: Int

day2P2Aux :: Int -> [Int] -> Int -> Int -> Int
day2P2Aux o l n v | o == z = 100 * n + v
                  | o < z || s1 < s2 = day2P2Aux o l (n+1) 0
                  | o > z && cond = day2P2Aux o l n (v+1)
    where z = day2 n v l
          cond = n >= 0 && n <= 99 && v >= 0 && v <= 99
          s1 = read (take 2 (show z)) :: Int
          s2 = read (take 2 (show o)) :: Int
