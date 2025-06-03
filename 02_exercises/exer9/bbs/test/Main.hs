{-# LANGUAGE BangPatterns #-}

module Main
  ( main
  ) where

import NTheory
import BBS

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import System.CPUTime

import Control.Monad.Random

newtype PositiveGt1 a = PositiveGt1
  { getPositiveGt1 :: a
  } deriving (Eq, Ord, Show, Read)

instance Functor PositiveGt1 where
  fmap f (PositiveGt1 x) = PositiveGt1 (f x)

instance (Num a, Ord a, Arbitrary a) => Arbitrary (PositiveGt1 a) where
  arbitrary = fmap PositiveGt1 (fmap abs arbitrary `suchThat` (> 1))
  shrink (PositiveGt1 x) = [PositiveGt1 x' | x' <- shrink x, x' > 1]

checkEGCD :: Integer -> Integer -> (Integer, Integer, Integer) -> Property
checkEGCD a b (d, s, t) =
  d === s * a + t * b .&&. a `rem` d === 0 .&&. b `rem` d === 0

propEGCD :: Positive Integer -> Positive Integer -> Property
propEGCD (Positive a) (Positive b) = checkEGCD a b (egcd a b)

propGCD :: Positive Integer -> Positive Integer -> Property
propGCD (Positive a) (Positive b) =
  let egcd_res@(d1, _, _) = egcd a b
      d2 = gcd_ a b
   in (checkEGCD a b egcd_res) .&&. d1 === d2

propModInv :: Positive Integer -> PositiveGt1 Integer -> Property
propModInv (Positive a) (PositiveGt1 n) =
  isCoprime a n
    ==> let a1 = modinv n a
         in 0 <= a1 .&&. a1 < n .&&. (a * a1) `mod` n === 1

genCRT :: Int -> Gen ([Integer], [Integer])
genCRT len = aux [] [] len
  where
    aux as ns 0 = return (as, ns)
    aux as ns n = do
      ni <-
        chooseInteger (2, 2 ^ (fromIntegral len))
          `suchThat` (\x -> all (isCoprime x) ns)
      ai <- chooseInteger (0, ni)
      aux (ai : as) (ni : ns) (n - 1)

propCRT :: ([Integer], [Integer]) -> Property
propCRT (!as, !ns) =
  let (x, n) = crt as ns
   in n === (product ns) .&&. 0
        <= x .&&. x
        < n .&&. all (\(ai, ni) -> x `mod` ni == ai `mod` ni) (zip as ns)

genGen :: Int -> Gen (Integer, Integer)
genGen len = do
  seed <- arbitraryBoundedIntegral
  let gp = do
        p <- get_safesafe (fromIntegral len+16)
        g <- getGen p
        return (g,p)
  return $ evalRand gp (mkStdGen seed)

propGen :: (Integer, Integer) -> Property
propGen (g, p) =
  let o = ordm_ g p
  in o === p - 1

genBBS :: Integer -> Gen (Integer, Integer, Integer)
genBBS len = do
  seed <- arbitraryBoundedIntegral
  let bbs_setup = get_bbs_setup len
  return $ evalRand bbs_setup (mkStdGen seed)


propBBSPeriod :: (Integer,Integer,Integer) -> Property
propBBSPeriod (p,q,s0) =
  let n = p*q
      s1 = (s0 * s0) `mod` n
      g = bbs n
      t_ideal = 2* ((p-3) `div` 4) * ((q-3) `div` 4)
      t = findPeriod g s1
  in fromIntegral t === t_ideal
 
 

main :: IO ()
main =
  hspec $ do
    describe "EGCD"
      $ modifyMaxSuccess (const 10000)
      $ modifyMaxSize (const $ 10 ^ 9)
      $ prop "Bezout identity holds, d|a, d|b (these hold iff d=gcd(a,b)."
      $ propEGCD
    describe "GCD"
      $ modifyMaxSuccess (const 10000)
      $ modifyMaxSize (const $ 10 ^ 9)
      $ prop
          "Produces the same result as EGCD and EGCD results checks correct as above."
      $ propGCD
    describe "modinv"
      $ modifyMaxSuccess (const 10000)
      $ modifyMaxSize (const $ 10 ^ 9)
      $ prop "a * (modinv n a) \\equiv 1 \\pmod{n}."
      $ propModInv
    describe "crt"
      $ modifyMaxSuccess (const 100)
      $ modifyMaxSize (const 100)
      $ prop "solution satisfies the system and n is the product of the moduli."
      $ forAll (sized genCRT)
      $ propCRT
    describe "getGen"
      $ modifyMaxSuccess (const 100)
      $ modifyMaxSize (const $ 4)
      $ prop "checking generator has order (with brute-force calculation) p-1."
      $ forAll (sized genGen)
      $ propGen
    describe "Blub-Blum-Shub" $ do
      modifyMaxSuccess (const 10)
        $ prop "checking period is maximum for 15 bits i.e T == 2p''q''."
        $ forAll (genBBS 15)
        $ propBBSPeriod
      modifyMaxSuccess (const 1)
        $ prop "checking period is maximum for 20 bits i.e T == 2p''q''. Just 1 test because it takes long time."
        $ forAll (genBBS 20)
        $ propBBSPeriod

