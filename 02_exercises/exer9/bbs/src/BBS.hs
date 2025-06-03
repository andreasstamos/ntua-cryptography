{-# LANGUAGE BangPatterns #-}

module BBS where

import Control.Monad.Random
import NTheory (crt, modexp)
import qualified MillerRabin (isPrime)

isPrime :: Integer -> Bool
isPrime = (flip MillerRabin.isPrime) 30

getGen :: (MonadRandom m) => Integer -> m Integer
getGen p = do
   g <- getRandomR (2,p-2)
   if modexp g ((p-1) `div` 2) p == 1 then getGen p else return g

get_s0 :: (MonadRandom m) => Integer -> Integer -> m Integer
get_s0 p q = do
  gp <- getGen p
  gq <- getGen q
  let (s0,_) = crt [gp,gq] [p,q]
  return s0

is_safesafe :: Integer -> Bool
is_safesafe p =
  let p' = (p-1) `div` 2
      p'' = (p'-1) `div` 2
  in isPrime p && isPrime p' && isPrime p''

get_safesafe :: (MonadRandom m) => Integer -> m Integer
get_safesafe l = do
  r <- getRandomR (0, 2^(l-5)-1)
  let p = (r+(2^(l-5)))*(2^4) + 7
  if is_safesafe p then return p else get_safesafe l


-- Returns (p,q,s_0). p,q with l bits. For BBS use n=p*q and initial value s1 = s0*s0.
get_bbs_setup :: (MonadRandom m) => Integer -> m (Integer, Integer, Integer)
get_bbs_setup l = do
  p <- get_safesafe l
  q <- get_safesafe l
  if p==q then get_bbs_setup l else do
    s_0 <- get_s0 p q
    return (p,q,s_0)


-- takes n and returns the pseudorandom generator function. this function takes the previous value and outputs a new pseudorandom value.
bbs :: Integer -> (Integer -> Integer)
bbs n = (\s -> (s*s) `mod` n)

-- Finds the period of function f. if a value f xi = x0 is reached, then definitely the sequence will repeat afterwards.
findPeriod :: (Integer -> Integer) -> Integer -> Int
findPeriod f x0 = aux (f x0) 1
  where
    aux :: Integer -> Int -> Int
    aux !xi !cnt
      | xi == x0 = cnt
      | otherwise = aux (f xi) (cnt+1)
 
