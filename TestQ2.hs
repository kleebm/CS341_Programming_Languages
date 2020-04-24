module TestQ2 (prefix,subList,addb,convertNumList,addNum,threeN,exec,test_exec,geography) where

import Debug.Trace

{-
Prefix takes two lists.  
It returns true if the first list is a prefix of the second list

Example:
prefix "abc" "abcdef"
True
prefix "abc" "abc"
True
-}
prefix :: Eq a => [a] -> [a] -> Bool
-- Fill in your code here
prefix (x:xs) (y:ys) 
 | x /= y = False
 | x == y = prefix xs ys
prefix [] ys = True
prefix xs [] = False

{-
subList takes two lists.
It returns true if the first list is a sublist of the second list.

Example:
subList "bcd" "abcdef"
True
subList "bce: "abcdef"
False
-}
subList :: Eq a => [a] -> [a] -> Bool
subList (x:xs) (y:ys)
 | prefix xs ys == True = subList xs ys 
 | prefix xs ys == False = subList (x:xs) ys
subList [] ys = True
subList xs [] = False


{-
Digit is a digit in base 10, in other words an Int in [0..9]
-}
type Digit = Int
{-
Number is a list of digits representing a positive integer
  the list will be in reverse (i.e., form lower order to higher order digit)
  the list will not have leading zeroes
-}
type Number = [Digit]


{-
addb takes 3 arguments
  1. A positive integer, represented as a list of digits, in reverse
       (In other words, it goes from lowest order digit to highest order digit,
        and not containing any leading zeroes)
  2. Another positive intger, represented as a list of digits, in reverse
  3. A carry, which will always be 0 or 1
addb returns the result of adding the two positive integers plus the carry

Example:
addb [5,7] [9,8] 0
[4,6,1]

-}
addb :: Number -> Number -> Digit -> Number
-- Fill in your code here
addb (x:xs) (y:ys) c
 | x + y > 10 = [x+y-10] ++ addb xs ys 1
 | x + y < 10 = [x+y] ++ addb xs ys c
addb [] [] c = [c]
addb xs [] 0 = xs
addb [] ys 0 = ys
addb [] ys 1 = addb [1] ys 0
addb xs [] 1 = addb xs [1] 0


{-
convertNumList takes a positive integer
It returns a list of digits in reverse, representing that number

Example:
convertNumList 75
[5,7]

-}
convertNumList :: Int -> Number
-- Fill in your code here
convertNumList n
 | n > 10 = [rem n 10] ++ convertNumList (div n 10) 
convertNumList n =  [n]


{-
addNum takes two integers
It returns the sum of those two integers as a list of digits in reverse

Example:
addNum 75 89
[4,6,1]

-}
addNum :: Int -> Int -> Number
--No need to fill in any code here
addNum m n = addb (convertNumList m) (convertNumList n) 0 





{-
threeN takes a list A of Integers as argument
It returns a list B, which the following properties:
1. A is a suffix of B.
1. If a number n in B is even then the number preceding it will be n/2.
2. If a number n in B is odd then the number preceding it will be 3n+1.
3. The first number in B will be 1.
You can assume that A has properties 2 and 3.

Example:
threeN [3]
[1,2,4,8,16,5,10,3]
-}

threeN :: [Integer] -> [Integer]
-- Fill in your code here
threeN [n] 
 | x == 1 = [n]
 | rem x 2 /= 0 = threeN([3*x +1] ++ [n])
 | otherwise = threeN([div x 2] ++ [n])
 where x = head [n]
threeN [] = []

--gives non-exhasutive error but dont know how to fix, this seems like the right answer
  
  
  
  
{-
Statement is an assembly language statement with three parts
  1. an instruction i
  2. a variable name v
  3. an integer n
Program is a list of Statements, to be executed in order
  implicitly think of each statement in the program to have a line number
  with the first instruction at line number 0
Memory represents the memory of the computer
  Memory is a list of pairs of a variable and its assigned value
The meaning of each instruction is as follows:
  1. load v n
    give variable v the value n in memory
  2. add v n
    add n to the value of v in memory
  3. jmp v n
    go to line number n, note that v is ignored so anything is allowed
  4. blz v n
    if the value of v is <= 0 then go line n
      otherwise proceed to the next line in the program
  5. ret v n
    quit the program and return the value v, here n is ignored
-}
type Inst = String
type Variable = String 
type Statement = (Inst,Variable,Int)
type Program = [Statement]
type Memory = [(Variable,Int)]  
  
  
{-
exec executes your program (or a part of your program)
  and returns its return value
A program is executed by executing each instruction in order, except for jmp or blz
exec takes as parameters:
  1. The entire program
  2. The piece of the program that is currently being evaluated
    (i.e., the current instruction up to the end of the program)
It returns the result of the first return statement it encounters
Note 1: The simplest way to update a value is to add a new pair to memorey
  instead of changing the value of what is there
Note 2: I don't care what you do if there are errors, such as:
  1. syntax errors
  2. jumping out of the program
  3. not encountering a return statement

 exec prog1 prog1 []
9

-}
exec :: Program -> Program -> Memory -> Int  
-- Fill in your code here
exec prog_whole ((i,v,n):xs) mem
  | i == "load"   = exec prog_whole xs ((v,n):mem)
  | i == "add"    = exec prog_whole xs ((v,n + (lokup mem v)):mem)
  | i == "jmp"    = exec prog_whole (drop n prog_whole) mem
  | i == "blz"    = if (lokup mem v) < 0 then exec prog_whole (drop n prog_whole) mem else exec prog_whole xs mem
  | i == "ret"    = lokup mem v 

lokup :: Eq a => [(a,b)] -> a -> b
lokup store x
  | null found = error "Not in list"
  | otherwise = head found 
  where found = [y | (x',y) <- store, x == x']
{-
Initially calling exec function

test_exec prog1
9

-}
test_exec :: Program -> Int
--No need to fill in code here
test_exec p = exec p p []


{-
Example program
-}
prog1 :: Program
prog1 = [("load","x",4),("load","y",5),("load","z",0),
         ("blz","y",7),("add","z",1),("add","y",(-1)),("jmp","",3),
         ("blz","x",11),("add","z",1),("add","x",(-1)),("jmp","",7),("ret","z",0)] 

  


  
{-
geography takes as arguments two lists of words, A and B.
It returns a list of all words C with the following properties:
1. B is a suffix of C.
2. Every word in C, except the first, must have the same last letter
     as the first letter of the word before it.
3. C contains all the words of A but no other words.  
You can assume the following:
1. B has property 2.
2. All the words in B are in A.
3. A has no duplicate words.
4. B has no duplicate words.

Example:
geography ["tiger","elephant","spot","rat","turtle"] ["spot"]
[["elephant","turtle","rat","tiger","spot"],["rat","tiger","elephant","turtle","spot"]]
-}
geography :: [String] -> [String] -> [[String]]
geography wordList listSoFar 
 |  length wordList == length listSoFar = [listSoFar]
  | otherwise = concat [geography wordList (newWord:listSoFar) | newWord <- wordList, head newWord == last (head listSoFar), notElem newWord listSoFar]
 
