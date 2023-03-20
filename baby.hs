doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x < 100 then
			doubleMe x
			else x

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

factorial :: Integer -> Integer
factorial n = product [1..n]

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, no luck."

factorial' :: (Integral a) => a -> a
factorial' 0 = 1
factorial' n = n * factorial' (pred n)

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1,y1) (x2,y2) = (x1+x2, y1+y2)

first :: (a,b,c) -> a
first (a,_,_) = a

second :: (a,b,c) -> b
second (_,a,_) = a

third :: (a,b,c) -> c
third (_,_,a) = a

head' :: [a] -> a
head' [] = error "Error: empty list"
head' (x:_) = x

length'' :: (Num b) => [a] -> b
length'' [] = 0
length'' (_:rst) = 1 + length'' rst

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]  

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "Eat something!"
    | bmi <= normal = "Maintain!"
    | bmi <= fat = "Exercise! Cut out sugar!"
    | otherwise = "Lipo-suction!"
    where bmi = weight / height ^ 2
          (skinny,normal,fat) = (18.5,25.0,30.0)

max' :: (Ord a) => a -> a -> a
max' a b
    | a < b = b
    | otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
myCompare a b
   | a < b = LT
   | a > b = GT
   | otherwise = EQ

initials :: String -> String -> String

initials first last = [f] ++ "." ++ [l] ++ "."
    where (f:_) = first
          (l:_) = last

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w,h) <- xs]
    where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder h r =
   let topArea = pi * r ^ 2
       sideArea = 2 * pi * r * h
   in sideArea + 2 * topArea


calcBmis' :: (RealFloat a) => [(a,a)] -> [a]
calcBmis' xs  = [bmi | (w,h) <- xs, let bmi = w/h^2]

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > tailMax = x
    | otherwise = tailMax
    where tailMax = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

replicate' :: (Num a, Ord a) => a -> b -> [b]
replicate' n x 
    | n <= 0 = []
    | otherwise = x:(replicate' (n-1) x)

take' :: (Num a, Ord a) => a -> [b] -> [b]
take' _ [] = []
take' n (x:xs)
    | n <= 0 = []
    | otherwise = x:take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) = (a == x) || elem' a xs

zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys 

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    let smallerList = [a | a <- xs, a <= x]
        biggerList = [a | a <- xs, a > x]
    in quickSort smallerList ++ [x] ++ quickSort biggerList

compareWithHundred :: (Ord a, Num a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperCase :: Char -> Bool
isUpperCase = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x
