maxpair :: Ord a => [(a,a)] -> [a]
maxpair = map ( uncurry max) 

between :: Ord a => a -> a -> [a] -> [a]
between x y = filter (>=x) . filter (<=y)

negsqaure :: [Int] -> [Int]
negsqaure xs = map square (filter (\x -> x <= -1) xs )
 where square x = x*x
 
funcpair :: (a -> a) -> (b -> b) -> ((a,b) -> (a,b))
funcpair f g = \(x,y) -> (f x, g y)

posneg :: a -> a -> (Int -> a)
posneg x y = (\z -> if z < 0 then y else x)

data Person = P String String Char Int
 deriving Show

diffages :: Person -> [Int] -> [Person]
diffages (P f l g a) xs = map (P f l g) xs 

instance Eq Person where
 P f l _ _ == P first last _ _ = f == first && l == last
 
data Tree a = Nil | Node a [Tree a]
 deriving Show
 
count :: Eq a => a ->  Tree a -> Int
count x (Node y ts) = (if x == y then 1 else 0) + sum (map (count x) ts)

numChildren :: Tree Int -> Tree Int
numChildren (Node x t) = Node (length t) (map numChildren t)

functree :: (a -> b) -> Tree a -> Tree between
functree f (Node x t) = Node (f x) (map (functree f) t)
 