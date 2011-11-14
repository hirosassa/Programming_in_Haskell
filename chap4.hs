myabs :: Int -> Int 
myabs n = if n >= 0 then n else -n

mysignum :: Int -> Int 
mysignum n = if n < 0 then  -1 else
              if n == 0 then 0 else 1

g_abs :: Int -> Int
g_abs n | n >= 0    = n
        | otherwise = -n

g_signum :: Int -> Int
g_signum n | n < 0  = -1
           | n == 0  = 0
           | n > 0  = 1

mynot :: Bool -> Bool
mynot False = True 
mynot True = False


-- This is my "AND" operator
(%%) :: Bool -> Bool -> Bool
True %% True = True
True %% False = False
False %% True = False
False %% False = False

-- This is my "AND" operator written with "pattern matching"
(%%%) :: Bool -> Bool -> Bool
True %%% True = True
_ %%% _ = False


{--
  This expression is invalid. Arguments in single equation must be different.
  Use "guard" expression!!

       b %%% b = b
       _ %%% _ = False
--}


-- Applying "pattern matching" to other argument types
first :: (a,b) -> a
first (x, _) = x
second :: (a,b) -> b
second (_, x) = x

test :: [Char] -> Bool
test['a', _, _] = True
test _ = False

mynull :: [a] -> Bool
mynull [] = True
mynull (_ : _) = False

{-- 
    cons patterns must be parenthesised because 
    func application has higher priority than others
--}
myhead :: [a] -> a
myhead (x : _) = x  

mytail :: [a] -> [a]
mytail (_ : xs) = xs

{-- 
    This expression is not recommended and 
    this is wrong in Haskell2010
    mypred :: Int -> Int
    mypred 0 = 0
    mypred n = n
--}

-- Lambda expressions
odds :: Int -> [Int]
odds n = map f [0 .. n-1]
         where f x = x * 2 + 1

l_odds :: Int -> [Int]
l_odds n = map (\x -> x * 2 + 1) [0 .. n-1]