-- CPSC 312 Assignment 2
-- Alexander Smith - 40066003
-- Question 1(a)
-- I don't think it's possible without resorting to hacky solutions (e.g.
-- implementing list length and indexing functions with foldl or foldr).
-- I assumed this sort of thing was not in the spirit of the question, so
-- I have omitted my solution. Because the list length is not known at compile
-- time, some form of recursion is necessary to go through the list.
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

capvowel [] = []
capvowel (e:r) = doif (\x -> elem x vowels) (toUpper) (e : r)
  where
    vowels = ['a', 'e', 'i', 'o', 'u']

-- (iv)
doif1 f g [] = []
doif1 f g (h:t) =
  [ if f x
    then g x
    else x
  | x <- (h : t)
  ]

capvowel1 [] = []
capvowel1 (e:r) = doif1 (\x -> elem x vowels) (toUpper) (e : r)
  where
    vowels = ['a', 'e', 'i', 'o', 'u']

-- (v)
doif2 f g [] = []
doif2 f g (h:t) =
  foldr
    (\x acc ->
       (if f x
          then (g x : acc)
          else (x : acc)))
    []
    (h : t)

capvowel2 [] = []
capvowel2 (e:r) = doif2 (\x -> elem x vowels) (toUpper) (e : r)
  where
    vowels = ['a', 'e', 'i', 'o', 'u']

-- Question 2(a)
harmonic :: (Enum a, Fractional a) => a -> a
harmonic n = foldl (\acc x -> acc + (1 / x)) 0 [1 .. n]

-- Question 2(c)
myremoveduplicates :: (Eq a) => [a] -> [a]
myremoveduplicates [] = []
myremoveduplicates (h:t) =
  foldr
    (\x acc ->
       if elem x acc
         then acc
         else (x : acc))
    []
    (h : t)

-- Question 2(d)
myreplace :: (Eq a) => a -> a -> [a] -> [a]
myreplace x y [] = []
myreplace x y (h:t) =
  foldr
    (\a acc ->
       if a == x
         then (y : acc)
         else (a : acc))
    []
    (h : t)
-- Question 3
-- 1(a) I spent more time on this question than any of the others, mostly out
-- of an unwillingness to give up on part (iv). Probably around an hour. I
-- learned I should probably cut my losses a bit sooner next time.
-- 1(b) I spent about 30 minutes on this question. I was able to solidify the
-- basics of non-recursive list functions using list comprehensions and
-- fold[l/r] in doing this question.
-- 2(a) This was quite simple - took about a minute. Good practice for applying
-- the concepts mentioned above.
-- 2(b) This took 5-10 minutes. Again, it was good practice for non-recursive
-- list processing.
-- 2(d) Ditto.
-- I didn't find any of the questions unreasonable.
