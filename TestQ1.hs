module TestQ1 (pairFirst,encryptChar,encryptString,howManyValues,numInvalid,distinctMap,ownInverse,subset,allMapped,mapLetters) where

-- Code is a type synonym
-- it says that a Code is a list of Pairs of Chars
type Code = [(Char,Char)]

-- domain of our code
domain1 :: [Char]
domain1 = ['a'..'z']
domain2 = ['a','b','a']

-- associated range
range1 :: [Char]
range1 = ['z','y'..'a']
range2 = ['a','c','c']

-- Turns two strings into a code
makeCode :: [Char] -> [Char] -> Code
makeCode domain range = zip domain range

-- create a code out of our domain and range
-- I will call each pair a mapping from the first element of the pair to the second
code1 :: Code
code1 = makeCode domain1 range1
code2 = makeCode domain2 range2

-- pairFirst takes a Code and Char
-- returns the list of all Pairs which have that Char as first element
pairFirst :: Code -> Char -> Code

-- Question 1 for homework  
-- I have given a definition here just so it type checks
-- You need to replace this with the proper definition
pairFirst code char = [(x,y) | (x,y) <- code, fst(x,y) == char]

-- uses a code to encrypt a Char
-- if the Char has no mapping then the Char encrypts as itself
-- if more than one mapping just use the first value
encryptChar :: Code -> Char -> Char 
encryptChar code ch
-- Question 2 for homework  
-- I have given a definition here just so it type checks
-- You need to replace this with the proper definition
  = if null (pairFirst code ch) then ch else snd(head (pairFirst code ch))

-- uses a Code to encrypt a String
encryptString :: Code -> String -> String
encryptString code chars 
-- Question 3 for homework  
-- I have given a definition here just so it type checks
-- You need to replace this with the proper definition
  = [encryptChar code x | x <- chars]

-- takes a Code and returns the number of elements a Char is mapped to in that Code
howManyValues :: Code -> Char -> Int
howManyValues code ch 
-- Question 4 for homework  
-- I have given a definition here just so it type checks
-- You need to replace this with the proper definition
  = length (pairFirst code ch) 

-- takes a Code and returns the number of Chars mapped to more than one element
numInvalid :: Code -> Int
numInvalid code 
-- Question 5 for homework  
-- I have given a definition here just so it type checks
-- You need to replace this with the proper definition
   = length [x | x <- ['a'..'z'],howManyValues code x > 1 ] 
   
-- takes a Code and returns True if no Char maps to itself
distinctMap :: Code -> Bool
distinctMap code 
-- Question 6 for homework  
-- I have given a definition here just so it type checks
-- You need to replace this with the proper definition
  = if numInvalid code == 0 then True else False

-- checks that whenever you encrypt a small letter twice you get the original letter back
ownInverse :: Code -> Bool
ownInverse code 
-- Question 7 for homework  
-- I have given a definition here just so it type checks
-- You need to replace this with the proper definition
  = if length [(x,y) | (x,y) <- code, encryptChar code (encryptChar code (fst(x,y))) /= fst(x,y)] == 0 then True else False

-- checks if everything in first String is contained in second String
subset :: String -> String -> Bool 
subset s1 s2 
-- Question 8 for homework  
-- I have given a definition here just so it type checks
-- You need to replace this with the proper definition
  = if [ x | x <-s1 , elem x s2 ] == s1 then True else False

-- checks if every small letter is the first element of a pair in Code
allMapped :: Code -> Bool
allMapped code 
-- Question 9 for homework  
-- I have given a definition here just so it type checks
-- You need to replace this with the proper definition
  = if subset ['a'..'z'] [x | (x,y) <- code] then True else False 
  
  

-- checks if everything in Code maps to a small letter
mapLetters :: Code -> Bool
mapLetters code 
-- Question 10 for homework  
-- I have given a definition here just so it type checks
-- You need to replace this with the proper definition
  = if length [(x,y) | (x,y) <- code] == length ['a'..'z'] then True else False

