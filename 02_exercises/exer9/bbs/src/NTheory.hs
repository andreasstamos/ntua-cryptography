module NTheory
  ( egcd
  , gcd_
  , modinv
  , isCoprime
  , crt
  , modexp
  , ordm_
  ) where

-- calcalates b^e mod m
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



-- Returns (d,s,t) of the Bezout identity s*a + t*b = d
egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd a b =
  let egcd_ rk sk tk 0 _ _ = (rk, sk, tk)
      egcd_ r0 s0 t0 r1 s1 t1 =
        let (q2, r2) = quotRem r0 r1
            s2 = s0 - q2 * s1
            t2 = t0 - q2 * t1
         in egcd_ r1 s1 t1 r2 s2 t2
   in egcd_ a 1 0 b 0 1

-- Calculates gcd of a, b. (simplification of egcd for efficiency)
gcd_ :: Integer -> Integer -> Integer
gcd_ d 0 = d
gcd_ a b = gcd_ b (a `mod` b)

-- Checks if a b are coprime
isCoprime :: Integer -> Integer -> Bool
isCoprime a b = gcd_ a b == 1

-- Return a^{-1} mod n. The caller must ensure a,n are coprime. a should be positive.
modinv :: Integer -> Integer -> Integer
modinv n a =
  let (_, _, t) = egcd n a
   in (n + (t `mod` n)) `mod` n

-- Solves Linear Congruence System x = a_i mod n_i (Chinese Remainder Theorem). Returns (x, prod ni)
crt :: [Integer] -> [Integer] -> (Integer, Integer)
crt as ns =
  let n = product ns
      partial_ns = map (n `div`) ns
      ms = map (uncurry modinv) (zip ns partial_ns)
      x =
        foldl'
          (\acc (ai, mi, partial_ni) -> (acc + ai * mi * partial_ni) `mod` n)
          0
          (zip3 as ms partial_ns)
   in (x, n)

-- bruteforce find the order. for Testing Purposes. (like a functional specification)
ordm_ :: Integer -> Integer -> Integer
ordm_ a0 n = aux a0 1
  where
    aux 1 o = o
    aux a o = aux ((a*a0) `mod` n) (o+1)

