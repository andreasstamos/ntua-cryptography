module Main (main) where

import System.Exit (exitFailure)
import MillerRabin (isPrime)

isqrt :: Integer -> Integer
isqrt n
    | n < 2 = n
    | otherwise = aux 1 n
  where
    aux l r
        | r < l = r
        | m*m > n = aux l (m-1)
        | otherwise = aux (m+1) r
      where
        m = (l+r) `div` 2


isPrime_oracle :: Integer -> Bool
isPrime_oracle n
  | n <= 1 = False
  | n == 2 = True
  | even n = False
  | otherwise = null [x | x <- [3,5.. isqrt n], n `mod` x == 0]

checks :: Int
checks = 10

test :: Integer -> IO ()
test n =
  let p = isPrime n checks
      p' = isPrime_oracle n
  in
  if p == p' then return () else do
     putStrLn $ "isPrime " ++ show n ++ " = " ++ show p ++ " , isPrime_oracle " ++ show n ++ " = " ++ show p'
     exitFailure

mersennes :: [Integer]
mersennes = [ 2^p-1 | p <- [3217, 4253, 4423, 9689] ]
nonmersennes :: [Integer]
nonmersennes = [ 2^i-1 | i <- [1234, 4567, 9876, 5432] ]

test_big :: Bool -> Integer -> IO ()
test_big is n =
  let p = isPrime n checks
  in
  if p == is then return () else do
     putStrLn $ "(the following is (/is not) a prime), isPrime " ++ show n ++ " = " ++ show p
     exitFailure

 
main :: IO ()
main = do
  mapM_ test [1..10000]
  putStrLn "Checked all numbers from 1 to 10000."

  mapM_ (test_big True) mersennes
  putStrLn "Checked some bigint mersenne primes."
  
  mapM_ (test_big False) nonmersennes
  putStrLn "Checked some bigint, mersenne-like non-primes (2^i-1)."

  test_big True 67280421310721
  putStrLn "Checked 67280421310721, found correctly it is a prime."

  test_big False 1701411834604692317316873037158841057
  putStrLn "Checked 1701411834604692317316873037158841057, found correctly it is not a prime."

  test_big False (2^1001 - 1)
  putStrLn "Checked 2^1001 - 1, found correctly it is not a prime."

  test_big True (2^2281 - 1)
  putStrLn "Checked 2^2281 - 1, found correctly it is a prime."

  test_big True (2^9941 - 1)
  putStrLn "Checked 2^9941 - 1, found correctly it is a prime."

