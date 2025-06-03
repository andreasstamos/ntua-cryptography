module MillerRabin
  ( isPrime
  ) where

import Control.Monad
import System.Random.Stateful

modexp :: Integer -> Integer -> Integer -> Integer
modexp b e m =
  let aux _ 0 acc = acc
      aux b1 e1 acc =
        let acc' =
              if odd e1
                then (b1 * acc) `mod` m
                else acc
         in aux ((b1 * b1) `mod` m) (e1 `div` 2) acc'
   in aux b e 1

isPrime_pass :: Integer -> Integer -> Bool
isPrime_pass n b
  | modexp b (n - 1) n /= 1 = False
  | otherwise =
    let t = go (n - 1)
        go x
          | even x = go (x `div` 2)
          | otherwise = x
        aux 1 = False
        aux bt'
          | bt' == (n - 1) = True
          | otherwise = aux ((bt' * bt') `mod` n)
        bt = modexp b t n
     in if bt == 1 || bt == n - 1
          then True
          else aux bt

isPrime :: Integer -> Int -> Bool
isPrime 1 _ = False
isPrime n checks =
  let randomexp gen = do
        bs <- replicateM checks $ uniformRM (1, n - 1) gen
        return (all (isPrime_pass n) bs)
      pureGen = mkStdGen 42
   in runStateGen_ pureGen randomexp
