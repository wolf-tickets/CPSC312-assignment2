-- CPSC 312 Assignment 2
-- Alexander Smith - 40066003
-- Question 1(a)
tails []    = [[]]
tails (e:r) = (e : r) : tails (r)

-- (i) tails is of type [a] -> [[a]] - takes a list of a's, returns a list of lists of a's
-- (ii) the value of tails "happy" is ["happy","appy","ppy","py","y",""]
-- (iii) the type of tails "happy" is [[char]] - a list of lists of chars.
-- (iv)
tails1 [] = [[]]
tails1 (e:r)
  | length (e : r) == 1 = [(e : r), []]
  | length (e : r) == 2 = [(e : r), r, []]
  | otherwise = [(e : r), r, []]

-- Question 1(b)
doif f g [] = []
doif f g (h:t)
  | f h = g h : doif f g t
  | otherwise = h : doif f g t

-- (i) the inferred type of doif is (a -> Bool) -> (a -> a) -> [a] -> [a]
-- (ii) [11,11,33,22,55,33]
-- (iii)
toUpper :: Char -> Char
toUpper x = toEnum (fromEnum x - fromEnum 'a' + fromEnum 'A')

capvowel (e:r) = doif1 (\x -> elem x vowels) (toUpper) (e : r)
  where
    vowels = ['a', 'e', 'i', 'o', 'u']

-- (iv)
-- doif1 f g [] = []
doif1 f g (h:t) =
  [ if f x
    then g x
    else x
  | x <- (h : t)
  ]
