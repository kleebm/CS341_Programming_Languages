--Due Date: 2017-11-02 23:59:59.999999

module TestQ4 (BinaryTree(..),inOrderSearch,insertm,insList,findm,Employee(..),getSalary,addEmployee,sumSalary) where

import Debug.Trace

{-
A BinaryTree is either a Leaf or a Node.  A Node contains a key,
a value, a left subtree and a right subtree.

This structure will hold a binary search tree, where the keys are
compared when inserting items.  In other words, the left child of
a node has a smaller key than its parent, and the right subchild
has a larger key.  Note this means that there can be no duplicate
keys in a tree.
-}
data BinaryTree a b = Leaf | Node a b (BinaryTree a b) (BinaryTree a b) 

-- Sample BinaryTrees
t1 ::  BinaryTree Int String
t1 = Leaf
t2 ::  BinaryTree Int String
t2 = Node 20 "abc" Leaf Leaf
t3 ::  BinaryTree Int String
t3 = Node 20 "abc" Leaf (Node 30 "def" Leaf Leaf)
t4 ::  BinaryTree Int String
t4 = Node 20 "abc" (Node 10 "ghi" Leaf Leaf) (Node 30 "def" Leaf Leaf)
t5 ::  BinaryTree Int String
t5 = Node 10 "ghi" Leaf (Node 30 "def" (Node 20 "abc" Leaf Leaf) Leaf)
t6 ::  BinaryTree Int String
t6 = Node 10 "jkl" Leaf (Node 20 "mno" Leaf (Node 30 "pqr" Leaf Leaf))
  
{-
BinaryTree is an instance of Show. A Binary Tree is shown as follows:
  - A Leaf appears as a dollar sign.
  - A Node appears as the key and value in parentheses, separated by a colon
    followed by "<", followed by the left subtree, followed by a comma,
    followed by the right subtree, followed by ">".
  - As a special case, a Node with no children just appears as the
    key and value in parentheses, separated by a colon.

Examples:
*TestQ4> t1
$
*TestQ4> t2
(20:"abc")
*TestQ4> t3
(20:"abc")<$,(30:"def")>
*TestQ4> t4
(20:"abc")<(10:"ghi"),(30:"def")>
*TestQ4> t5
(10:"ghi")<$,(30:"def")<(20:"abc"),$>>
*TestQ4> t6
(10:"jkl")<$,(20:"mno")<$,(30:"pqr")>>
*TestQ4>
-}
instance (Show a, Show b) => Show (BinaryTree a b) where
-- Your code goes here
  show Leaf = "$"
  show (Node  x y Leaf Leaf) = "(" ++ show x ++ ":" ++ show y ++ ")"
  show (Node x y l r) = "(" ++ show x ++ ":" ++ show y ++ ")<" ++ show l ++ "," ++ show r ++ ">"

{-
findm takes a key and a Binary Tree.  If the key appears in the tree
then it returns the associated value.  If the key is not in the tree
it returns Nothing. Otherwise it returns Just the associated value.

Examples:
*TestQ4> findm 30 t5
Just "def"
*TestQ4> findm 25 t5
Nothing
*TestQ4>
-}
findm :: Ord a => a -> BinaryTree a b -> Maybe b
-- Your code goes here
findm key Leaf = Nothing
findm key (Node x y l r)
 | key == x = Just y
 | key < x = findm key l
 | otherwise = findm key r
    
{-
insertm inserts a key and a value into a BinaryTree.
It returns a Maybe BinaryTree.  If the key is already in the tree,
it returns Nothing.  If the key is not already in the tree, it inserts
it into the proper place and returns Just that BinaryTree.

Examples:
*TestQ4> insertm 25 "vw" t5
Just (10:"ghi")<$,(30:"def")<(20:"abc")<$,(25:"vw")>,$>>
*TestQ4> insertm 20 "vw" t5
Nothing
*TestQ4>
-}
insertm :: (Ord a, Show a, Show b) =>
  a -> b -> BinaryTree a b -> Maybe (BinaryTree a b)
--insertm k v t | trace ("insertm " ++ (show k)
--                  ++ " " ++ (show v) ++ " " ++ (show t)) False = undefined
-- Your code goes here
insertm key val Leaf = Just (Node key val Leaf Leaf)
insertm key val (Node x y l r) = Nothing
--Dont know how to do this one


{-
insList take a list of key value pairs and a Maybe Binary Tree
and inserts all those keys value pairs into the Maybe Binary Tree
in order.  If any key appears twice in the list then this whole
process will return Nothing.

Examples:
*TestQ4> insList [(10,"ghi"),(30,"def"),(20,"abc"),(25,"vw")] (Just Leaf)
Just (10:"ghi")<$,(30:"def")<(20:"abc")<$,(25:"vw")>,$>>
*TestQ4> insList [(10,"ghi"),(30,"def"),(10,"abc"),(25,"vw")] (Just Leaf)
Nothing
*TestQ4>
-}
insList :: (Ord a, Show a, Show b) =>
  [(a,b)] -> Maybe (BinaryTree a b) -> Maybe (BinaryTree a b)
-- Your code goes here
insList [] z = z
insList ((x,y):xys) z = insList xys (insertm  x y)
--Close but cant figure it out

{-
inOrdereSearch performs an inOrder search of a BinaryTree.
It creates a list of key value pairs out of the tree, such that the left
child of a node appears before that node, and the right child appears after.

Example:
*TestQ4> inOrderSearch t4
[(10,"ghi"),(20,"abc"),(30,"def")]
*TestQ4>
-}
inOrderSearch :: BinaryTree a b -> [(a,b)]
-- Your code goes here
inOrderSearch Leaf = []
inOrderSearch (Node x y l r) = inOrderSearch l ++ ((x,y):inOrderSearch r)

{-
BinaryTree is an instance of Eq.  Two BinaryTrees are equal if they
contain exactly the same key value pairs.

Examples:
*TestQ4> t4 == t5
True
*TestQ4> t4 == t6
False
*TestQ4>
-}
instance (Eq a, Eq b) => Eq (BinaryTree a b) where
-- Your code goes here
  t1 == t2 = False

{-
Employee is a data structure for an employee of a company.
It contains the person's name, their salary, and a list of
all the employees whom they are the direct boss of.

As a special case, if that employee is not the boss of anybody,
then the list of employees under them will be empty.

Employee is also allowed to have the value Nobody.  That value
will only be used when there is nobody in the company.
-}
data Employee a b = Nobody | Boss a b [Employee a b]
  deriving Show

{-
addEmployee adds an Employee to the company.
It takes a person's name, their salary and their boss.
That person then becomes a new Employee of the company,
working under the specified boss.

If the boss is not working in the company, the function
does not add the new employee.

One exception is that if nobody works at the company, then
the new pereson is added as the only employee, and the boss
parameter is ignored.

Examples:
*TestQ4> e1
Boss "ann" 1 []
*TestQ4> e2
Boss "ann" 1 [Boss "bill" 2 []]
*TestQ4> e3
Boss "ann" 1 [Boss "chuck" 3 [],Boss "bill" 2 []]
*TestQ4> e4
Boss "ann" 1 [Boss "chuck" 3 [],Boss "bill" 2 [Boss "dave" 4 []]]
*TestQ4> addEmployee "ed" 20 "fred" e4
Boss "ann" 1 [Boss "chuck" 3 [],Boss "bill" 2 [Boss "dave" 4 []]]
-}
addEmployee :: Eq a => a -> b -> a -> Employee a b -> Employee a b
addEmployee nm sal boss Nobody = Boss nm sal []
addEmployee nm sal boss es@(Boss n s emps) 
 | n == nm = es
 | n == boss = Boss n s (Boss nm sal [] : emps)
 | otherwise = Boss n s (map (addEmployee nm sal boss) emps)

e1 = addEmployee "ann" 1 "blah" Nobody
e2 = addEmployee "bill" 2 "ann" e1
e3 = addEmployee "chuck" 3 "ann" e2
e4 = addEmployee "dave" 4 "bill" e3

{-
getSalary takes an Employees name n and an Employee e.
It finds the salary of n in the company where Employee
e is the main boss.  If n doesn't work there it returns Nothing,
otherwise it returns Just n's salary.

Examples:
*TestQ4> getSalary "bill" e4
Just 2
*TestQ4> getSalary "frank" e4
Nothing
*TestQ4>
-}
getSalary :: (Eq a, Eq b) => a -> Employee a b -> (Maybe b)
-- Your code goes here
getSalary _ Nobody = Nothing
getSalary nm (Boss name sal emps)
 | nm == name = Just sal
 | null salaries = Nothing
 | otherwise = head salaries
   where salaries = filter (/= Nothing) $ map (getSalary nm) emps
{-
sumSalary returns the sum of the salaries of everybody working
at the company with the given Employee as the big boss.  If nobody
works at the company, return 0.

Examples:
*TestQ4> sumSalary e4
10
*TestQ4> sumSalary Nobody
0
*TestQ4> sumSalary e1
1
*TestQ4>
-}
sumSalary :: Num b => Employee a b -> b
-- Your Code goes here
sumSalary Nobody = 0
sumSalary (Boss _ sal emps) = sal + sum (map sumSalary emps)
  
