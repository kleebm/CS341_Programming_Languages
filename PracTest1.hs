prefix :: [Integer] -> [Integer] -> Bool
prefix (x:xs) (y:ys)
 | x == y = prefix xs ys
 | otherwise = False
prefix [] ys = True
prefix xs [] = False

sublist :: Eq a => [a] -> [a] -> Bool
sublist (x:xs) (y:ys)
 | x ==y = sublist xs ys
 | otherwise = sublist (x:xs) ys
sublist [] ys = True
sublist xs [] = False 

removeAll :: [Integer] -> Integer -> [Integer]
removeAll xs y = [ ys | ys <- xs, ys /= y]

singleton :: [Integer]  -> [[Integer]]
singleton  xs = [ [ys] | ys <-xs]

removeFirst :: [Integer] -> Integer -> [Integer]
removeFirst xs y
 | x == y = [] ++ xs
 | otherwise = removeFirst xs
 where x = head(xs)
removeFirst [] y = []