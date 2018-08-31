import Data.Maybe
import Data.List
-- import Data.Function
import Data.Char

doubleUs x y = doubleMe x + doubleMe y

doubleMe x = x + x

doubleSmallNumber x = (if x <= 100
                        then x*2
                        else x) + 1

moduloUsingList a n = last (take a (cycle [1..n]))

moduloUsingDiv a n = a - (div a n ) * n

oddNumbers = [ x+x+1 | x <- [0..]]
twoDigitsOddNumbers = [ x | x <- takeWhile(<=100) oddNumbers, x < 100, x > 9 ]

congruency min max a n = [ x | x <- [min..max], x `mod` n == a ]

oddEven xs = [if odd x then "odd!" else "even!" | x <- take 100 xs]

-- congruency a n = congruency 0 n-1 a n

-- scalarProduct v1 v2 = sum [x*y | i <- [0..(length v1)-1], x == v1 !! i, y == v2 !! i]

length' xs = sum [1 | _ <- xs]

removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

remOddNumbersWithoutFlatteringTheList xxs = [[x | x <- xs, even x] | xs <- xxs]

triangles maxSide perimeter = [(a,b,c) | c <- [1..maxSide], b <- [1..c], a <- [1..b], a+b+c == perimeter, a+b>c]
rightTriangles maxSide perimeter = [(a,b,c) | (a,b,c) <- triangles maxSide perimeter, c^2 == a^2+b^2]

factorial :: Integer -> Integer
factorial n = product[1..n]

head' :: [a] -> a
head' [] = error "List is empty!!"
head' (x:_) = x

head'' :: [a] -> a
head'' xs = case xs of [] -> error "List is empty!!"
                       (x:_) -> x


tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length'' :: [a] -> Int
length'' [] = 0
length'' (x:xs) = 1 + length'' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]


bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"
    where bmi = weight / height^2
          (skinny, normal, fat) = (18.5, 25, 30)

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a < b     = LT
    | otherwise = EQ

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

initials' :: String -> String -> String
initials' (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

calcBmis' :: (RealFloat a) => [(a,a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

calcFatBmis :: (RealFloat a) => [(a,a)] -> [a]
calcFatBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, let (skinny, normal, fat) = (18.5, 25.0, 30.0), bmi >=normal]


maximum' :: (Ord a) => [a] -> a
maximum' [] = error "list is empty!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "list is empty!"
maximum'' [x] = x
maximum'' (x:xs)
    | x > maxTail       = x
    | otherwise         = maxTail
    where maxTail = maximum'' xs

replicate' :: (Integral n) => n -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

take' :: (Integral n) => n -> [a] -> [a]
take' n xs
    | n <= 0    = []
take' n []      = []
take' n (x:xs)  = x:take' (n-1) xs

-- quicksort :: (Ord a) => [a] -> [a]
-- quicksort [] = []
-- quicksort (x:xs) = let	smallerSorted = quicksort [a | a <- xs, a <= x]
-- 			biggerSorted = quicksort [a | a <- xs, a > x]
-- 		   in	smallerSorted ++ [x] ++ biggerSorted


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = smallerSorted ++ x : quicksort [a | a <- xs, a > x]
                 where smallerSorted = quicksort [a | a <- xs, a <= x]


-- 6. Higher Order Functions

-- Curried functions

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

multTwoWithNine :: (Num a) => a -> (a -> a)
multTwoWithNine = multThree 9

multWithEighteen :: (Num a) => a -> a
multWithEighteen = multTwoWithNine 2

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Fractional a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- Some higher-orderism is in order

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f(f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f y x = f x y

map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
             | p x       = x : filter' p xs
             | otherwise = filter' p xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = 
           let smallerSorted = quicksort' (filter' (<=x) xs)
               biggerSorted = quicksort' (filter' (>x) xs)
            in smallerSorted ++ x : biggerSorted

divideList :: (a -> Bool) -> [a] -> ([a],[a])
divideList _ [] = ([],[])
divideList p (x:xs)
            | p x       = (x:filteredIn, filteredOut)
            | otherwise = (filteredIn, x:filteredOut)
      where filteredIn  = fst (divideList p xs)
            filteredOut = snd (divideList p xs)

quicksort'' :: (Ord a) => [a] -> [a]
quicksort'' [] = []
quicksort'' (x:xs) = smallerSorted ++ x : biggerSorted
      where smallerSorted = quicksort' smaller
            biggerSorted = quicksort' bigger
            smaller = fst divided
            bigger  = snd divided
            divided = divideList (<=x) xs

largestDivisible :: (Integral a) => a -> a -> a
largestDivisible max divider = head (filter p [max, max-1..])
                             where p x = x `mod` divider == 0

sumOfOddSquaresSmallerThan :: (Integral a) => a -> a
-- sumOfOddSquaresSmallerThan maxSquare = sum (takeWhile (<maxSquare) (map (^2) (filter odd [1..])))
sumOfOddSquaresSmallerThan maxSquare = sum $ takeWhile (<maxSquare) . map (^2) $ filter odd [1..]

firstWordOfString :: String -> String
firstWordOfString string = takeWhile(notWhitespaceOrPunctation) string
                      where notWhitespaceOrPunctation x = null (filter (==False) (map (x/=) [' ', ',', '.', '?', '!']))

collatzSequence :: (Integral a) => a -> [a]
collatzSequence 0 = [0]
collatzSequence 1 = [1]
collatzSequence n
            | even n = n : collatzSequence (n `div` 2)
            | odd  n = n : collatzSequence (3*n+1)

numCollatxSequencesLongerThan :: Int -> Int
numCollatxSequencesLongerThan n = length (filter (\xs -> length xs > n) (map collatzSequence [1..100]))

addThree :: (Num a) => a -> a -> a -> a
-- addThree = \x y z -> x + y + z
addThree = \x -> \y -> \z -> x + y + z

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

-- sum' [] = 0
-- sum' (x:xs) = x + sum' xs

foldl'' :: (a -> b -> a) -> a -> [b] -> a
foldl'' _ z [] = z
foldl'' f z (x:xs) = foldl'' f (f z x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

sum'' :: (Num a) => [a] -> a
-- sum'' xs = foldl (\acc x -> acc + x) 0 xs
-- sum'' xs = foldl (+) 0 xs
sum'' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
-- elem' y ys = foldl(\acc x -> x == y || acc) False ys
elem' y = foldl(\acc x -> x == y || acc) False

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x acc -> f x : acc) []

maximum''' :: (Ord a) => [a] -> a
maximum''' = foldl1 (\acc x -> if (x > acc) then x else acc)

reverse' :: [a] -> [a]
-- reverse' = foldl (\acc x -> x : acc) []
reverse' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl1 (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x:acc else acc) []

head''' :: [a] -> a
head''' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1(\_ x -> x)

find' :: (Eq a) => (a -> Bool) -> [a] -> Maybe a
find' p xs = foldl (\acc x -> if acc /= Nothing then acc else if p x then Just x else acc) Nothing xs

on :: (b -> b -> c) -> (a -> b) -> (a -> a -> c)
f `on` g = \x y -> f (g x) (g y)

sortArraysByLength :: [[a]] -> [[a]]
sortArraysByLength xs = sortBy (compare `on` length) xs

encode :: Int -> String -> String
encode n txt = map (\c -> chr $ c + n) $ map ord txt
-- encode n txt = map (chr . (+n) . ord) txt

decode :: Int -> String -> String
decode n txt = encode (-n) txt


infixr 5 :-:  
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty      .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)
