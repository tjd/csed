mylength :: [a] -> Integer
mylength []           =  0
mylength (first:rest) =  1 + mylength rest

myhead :: [a] -> a
myhead (first:rest) = first   -- or: head (x:_) = x

mytail :: [a] -> [a]
mytail (first:rest) = rest  -- or: tail (_:xs) = xs

add :: Integer -> Integer -> Integer
add x y = x + y

inc = add 1    -- defines a new function via currying

--
-- descriptive statistics on lists
--

mean :: (Floating a) => [a] -> a
mean [] = error "Empty list has no mean!"
mean lst = (sum lst) / n
           where n = realToFrac (length lst)

-- note that the input to the sum function uses a list comprehension
variance :: (Floating a) => [a] -> a
variance lst = (sum [(x - u)^2 | x<-lst]) / (realToFrac (n - 1))
               where u = mean lst
                     n = length lst

std_dev :: (Floating a) => [a] -> a
std_dev lst = sqrt (variance lst)

smallest :: (Ord a) => [a] -> a
smallest [] = error "Empty list has no smallest element!"
smallest [x] = x
smallest (x:xs) = min x (smallest xs)

biggest :: (Ord a) => [a] -> a
biggest [] = error "Empty list has no smallest element!"
biggest [x] = x
biggest (x:xs) = max x (biggest xs)

extreme :: (Ord a) => (a -> a -> a) -> [a] -> a
extreme _ [] = error "Empty list has no extreme values"
extreme _ [x] = x
extreme f (x:xs) = f x (extreme f xs)

listmax :: (Ord a) => [a] -> a
listmax = extreme max

listmin :: (Ord a) => [a] -> a
listmin = extreme min

-- A recursive version of selection sort.
-- As the name suggests, this is not very efficient.
-- But it's simple!
slowsort :: (Ord a) => [a] -> [a]
slowsort [] = []
slowsort [x] = [x]
slowsort lst  = first : (slowsort rest)
                where first = listmin lst
                      rest = tail lst

-- returns a list [0, 1, 2, ..., n-1]
rng n | n <= 0    = []
      | otherwise = (rng (n - 1)) ++ [n - 1]

ones = 1 : ones
ts = 't' : ts

intsFrom n stride = n : (intsFrom (n + stride)  stride)
rng2 start end stride = takeWhile (<end) (intsFrom start stride)

-- not yet correct
--rng3 start end stride = takeWhile (<end) (start : (map (+stride) [start ..]))


------------------------------------------------------------

-- see http://scienceblogs.com/goodmath/2006/11/simple_functions_in_haskell_1.php
-- Note that this is a lazy list --- it is both infinite and defined
-- recursively in terms of itself.

fiblist = 0 : 1 : (zipWith (+) fiblist (tail fiblist))

-- lst !! n returns the item at location n of the list
fib n = fiblist !! n

-- this fib function is quite elegant, but terribly inefficient
slow_fib 1 = 1
slow_fib 2 = 1
slow_fib n = (slow_fib (n - 2)) + (slow_fib (n - 1))

dot_prod1 a b = sum (zipWith (*) a b)
