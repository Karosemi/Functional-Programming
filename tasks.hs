import Distribution.Simple.Utils (xargs)
import GHC.ExecutionStack (Location(functionName))
import qualified Data.Set as Set
import Graphics.Win32.Misc (iDI_EXCLAMATION)
import GHC.List (errorEmptyList)
import Data.IntMap.Strict (toList)
import Data.List (inits, tails)
-- import GHC.Num (integerToFloat#)
-- Ex 1.

-- Standard recusion
factorial 0 = 1
factorial n = n * factorial (n-1)

-- tail recursion
fact 0 acc = acc
fact n acc = fact (n-1)  (n * acc)

tailFactorial n = fact n 1

--  Ex 2. 
--  standard recusrion
rev [] = []
rev (x:xs) = rev xs ++ [x]

-- tail recusion
rever xs acc = foldl (flip (:)) acc xs

tailRev (x:xs) =  rever (x:xs) []

-- Ex 3.

zerosCounter n
    | n < 5 = n `div` 5
    | otherwise = zerosCounter(n `div` 5) + n `div` 5


-- Ex 4.

f(a,b) = a * b
g a b = a + b
(myCurry f) a b = f (a,b)
(myDecurry g) (a,b) = g a b

-- Ex 5.
isPrime n
    = g True 2 n where
        g acc k n
            | k == n = acc
            | mod n k == 0 = False
            | otherwise = g acc (k+1) n

--  TODO modify this method 
-- sieve :: (Integral a, Floating a) => a -> [a]
-- sieve n = filter isPrime [2..n**(1/2)] 
-- sievie n = filter isPrime [2..n]


-- Ex 6.
-- znalezc takie liczby gdzie najwiekszy wspolny dzielnik to 1
--  (a)
-- greatest common divisor
cgcd(0, b) = b
cgcd(a, b) = cgcd(b `mod` a, a)


-- check if number a is coprime to b
isCoprime a b
    | cgcd(a, b) == 1 = True
    | otherwise = False


phi n = length (filter (isCoprime n) [1..n])

-- (b)
--hypothethis f(n) = n
isDivisor n k
    | n `mod` k == 0 = True
    | otherwise =  False


getDivisor n = filter (isDivisor n) [1..n]

getEulerFuncToDivisors n = map phi (getDivisor n)
checkHipothethis n
    | n == sum(getEulerFuncToDivisors n) = True
    | otherwise = False


checkForNumbers n = length(filter checkHipothethis [1..n])


-- Ex 7.
-- tail recursion
-- (a)

fibAcc a b n
    | n == 0 = b
    | otherwise = fibAcc b (a+b) (n-1)

fibbonaci = fibAcc 0 1

-- (b)


modFibAcc a b c n
    | n == 0 = 1
    | n == 1 = b
    | otherwise = modFibAcc b (a+b+c) (c+1) (n-1)

modFibbonaci = modFibAcc 1 1 2

newFib n
    | n == 0 = 1
    | n == 1 = 1
    | otherwise  = n + newFib (n-1)  + newFib (n-2)


--  Ex 8.

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
-- main :: IO ()
-- main = print $ take 10 fibs



-- Ex 9.
--  (a)
fnl [] x = [x]
fnl xs x
    | last xs == x = xs
    | otherwise = xs ++ [x]


ecd [] = []
ecd ys = map (:[]) (foldl fnl [] ys)


catchSame x [] = [[x]]
catchSame x (b@(b1:_):bs)
    | x == b1    = (x:b):bs
    | otherwise = [x]:b:bs

pairSame y= foldr catchSame [] y

encodeOne (y:ys) = (y, length(y : ys))
encode y =  map encodeOne (pairSame y)

-- (b)

expandPair (k, n) = replicate n k


decode y = map (:[]) (concatMap expandPair y)

-- Ex 10.
rev_rev listOFStrings = tailRev (map tailRev listOFStrings)

-- another solution
reverseString str = tailRev str



-- Ex 11.
nonEmptySubstrings = concatMap (tail . inits) . tails
substrings y = filter (not . null) (nonEmptySubstrings y)

-- Ex 12.
power_list [] = [[]]
power_list (x:xs) = [x:sublist | sublist <- power_list xs] ++ power_list xs

-- another solution
power_list1 [] = [[]]
power_list1 xs = [ys ++ zs| ys <- [[], [head xs]], zs <- power_list1(tail xs)]
-- Ex 13. 
permutate [] = [[]]
permutate l = [a:x | a <- l, x <- (permutate $ filter (\x -> x /= a) l)]

-- main :: IO ()
-- main = print $ tailFactorial 25
-- main = print $ zerosCounter 24
-- main = print $ myCurry f 6 2
-- main = print $ sieve 20
-- main = print $ phi 121
-- main =  print $ getDivisor 9
-- main = print $ checkForNumbers 200
-- main = print $ modFibbonaci 9
-- main = print $ ecd  [1,1,2,3,3]
-- main = print $ encode ['a','a','a','b','c', 'b', 'c', 'b']
-- main = print $  remdups1 [1,1,1,2,2,2,4,4,2]
-- main = print $  decode (encode ['a','a', 'n', 'n', 'n', 'a', 'b', 'b', 'a', 'n'])
-- main = print $ rev_rev ["abc","xyz"]
-- main = print $ substrings "abc"
-- main = print $ permutate [1,2,3]


main :: IO ()
main = print $ power_list1 [1,2,3]









