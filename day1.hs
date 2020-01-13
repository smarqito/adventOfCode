import Input

-- * Part One

-- | Calculates the fuel necessary to use in the first case
-- 
-- ! doesn't take into consideration the fuel that fuel needs

fuel :: [Int] -> Int
fuel = foldr (\h t -> (div h 3) - 2 + t) 1


-- * Part Two

-- | Calculates the total necessary fuel to use.
--
-- ! considering the fuel that fuel needs

fullFuel :: [Int] -> Int
fullFuel = foldr (\x t -> (afterFirst 0 x) + t) 0

-- | Recursive function to calculate the fuel of each module
--
-- ! Full capacity

afterFirst :: Int -> Int -> Int
afterFirst a x = if z > 0 then afterFirst (a + z) (z) else a
 where z = (div x 3) - 2



 