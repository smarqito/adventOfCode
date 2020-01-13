import Input
import Data.List

-- * Part One

-- | Main Function
day4 :: Int -> Int -> Int
day4 min max = day4Aux 0 min max

-- | Aux function that counts if the conditions are True
-- ! number is a sequence
-- ! two adjacent numbers are the same

day4Aux :: Int
 -> Int
 -> Int
 -> Int
day4Aux i c max | possible c && c <= max = day4Aux (i+1) (c+1) max
                | c > max = i                
                | otherwise = day4Aux i (c+1) max

-- | Function to check if the number is a possible password
possible :: Int -> Bool
possible m = rep && seqT
    where s = show m
          two = groupBy (\x y -> x == y ) s
          rep = maximum lg > 1
          seqT = sequenceT s
          lg = map length two
          
-- | check if the number is a sequence

sequenceT :: String -> Bool
sequenceT [x] = True
sequenceT (h : b : t) = h <= b && sequenceT (b : t)