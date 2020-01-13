import Input
import Data.List
-- | create coordinated points

day3 :: [[Dd a]] -> [(Int, Int)]
day3 l = intersect (x !! 0) (x !! 1)
    where x = map (day3Aux2 (0,0)) l

calculateMan :: [(Int, Int)] -> [Int]
calculateMan = map (\x -> ( abs (fst x) ) + ( abs (snd x) ) )

day3Aux2 :: (Int, Int) -> [Dd a] -> [(Int, Int)]

day3Aux2 (x,y) ((R 0) : []) = [(x,y)]
day3Aux2 (x,y) ((L 0) : []) = [(x,y)]
day3Aux2 (x,y) ((U 0) : []) = [(x,y)]
day3Aux2 (x,y) ((D 0) : []) = [(x,y)]


day3Aux2 (x,y) ((R 0) : t) = (x,y) : day3Aux2 (x,y) t
day3Aux2 (x, y) ((R z) : t) = (x + 1, y) : day3Aux2 (x+1,y) ((R (z-1)) : t)

day3Aux2 (x,y) ((L 0) : t) = (x,y) : day3Aux2 (x,y) t
day3Aux2 (x, y) ((L z) : t) = (x - 1, y) : day3Aux2 (x - 1,y) ((L (z-1)) : t)

day3Aux2 (x,y) ((U 0) : t) = (x,y) : day3Aux2 (x,y) t
day3Aux2 (x, y) ((U z) : t) = (x, y + 1) : day3Aux2 (x,y + 1) ((U (z-1)) : t)

day3Aux2 (x,y) ((D 0) : t) = (x,y) : day3Aux2 (x,y) t
day3Aux2 (x, y) ((D z) : t) = (x, y - 1) : day3Aux2 (x,y - 1) ((D (z-1)) : t)