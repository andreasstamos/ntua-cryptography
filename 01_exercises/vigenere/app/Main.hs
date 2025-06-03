import Data.Char (chr, isAlpha, ord)
import Data.List (group, minimumBy, sort, sortBy, transpose)
import Data.Ord (comparing)

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Text.Printf (printf)

english_props :: [Double]
english_props =
  [ 0.08167082
  , 0.01492015
  , 0.02782028
  , 0.04253043
  , 0.12702127
  , 0.02228022
  , 0.0201502
  , 0.06094061
  , 0.0696607
  , 0.00153002
  , 0.00772008
  , 0.0402504
  , 0.02406024
  , 0.06749067
  , 0.07507075
  , 0.01929019
  , 0.00095001
  , 0.0598706
  , 0.06327063
  , 0.09056091
  , 0.02758028
  , 0.0097801
  , 0.02360024
  , 0.00150002
  , 0.0197402
  , 0.00074001
  ]

calc_freqs :: String -> [Int]
calc_freqs s =
  let counts =
        map (\xs -> (ord (head xs) - ord 'a', length xs)) $ group (sort s)
      go [] n = repeat 0
      go l@((i, val):xs) n
        | n < i = 0 : go l (n + 1)
        | otherwise = val : go xs (n + 1)
   in take 26 $ go counts 0

calc_props :: String -> [Double]
calc_props s =
  let freqs = calc_freqs s
   in map (\x -> fromIntegral x / fromIntegral (sum freqs)) freqs

smoothing :: Double
smoothing = 1e-10

distribution_dist :: [Double] -> [Double] -> Double
{-kl_div ps qs =
	let qs_smooth = map (max smoothing) qs
        in
	sum $ map (\(p,q) -> p * log (p/q)) $ zip ps qs_smooth
-}
distribution_dist expected observed =
  foldl' (+) 0 $ zipWith metric expected observed
  where
    metric e o = e * log (e / (max o smoothing))

ceasar :: String -> (Char, String)
ceasar cipher =
  let props = calc_props cipher
      shifts = take 26 (iterate (\(x:xs) -> xs ++ [x]) props)
      offset =
        snd
          $ minimumBy
              (comparing (\(qs, _) -> distribution_dist english_props qs))
          $ zip shifts [0 ..]
   in ( chr (ord 'a' + offset)
      , map
          (\c ->
             if isAlpha c
               then (chr $ (ord c - ord 'a' - offset) `mod` 26 + ord 'a')
               else c)
          cipher)

ic :: String -> Double
ic s =
  fromIntegral (sum $ map (\f -> f * (f - 1)) (calc_freqs s))
    / fromIntegral (length s * (length s + 1))

strips :: [a] -> Int -> [[a]]
--strips l k  = [ [x | (x,i) <- zip l [0..], i `mod` k == j] | j <- [0..k-1] ]
strips l k =
  runST $ do
    vec <- MV.replicate k id
    let go [] _ = return ()
        go (x:xs) i = do
          let idx = i `mod` k
          currDList <- MV.read vec idx
          MV.write vec idx (currDList . (x :))
          go xs (i + 1)
    go l 0
    frozenVec <- V.freeze vec
    return $ map ($ []) (V.toList frozenVec)

try_keylen :: String -> Int -> Double
try_keylen s k =
  let cols = strips s k
      ave_ic = (sum (map ic cols)) / (fromIntegral k)
      len = fromIntegral $ length s
   in abs (ave_ic - 0.0655)

vigenere_keylens :: String -> Int -> [Int]
vigenere_keylens s ub = sortBy (comparing (try_keylen s)) [1 .. ub]

decrypt_vigenere :: String -> Int -> (String, String)
decrypt_vigenere s keylen =
  let cols = strips s keylen
      plain_key = map ceasar cols
      plain = concat $ transpose $ map snd plain_key
      key = map fst plain_key
   in (key, plain)

reconstruct :: String -> String -> String
reconstruct (c:cs) ps
  | not (isAlpha c) = c : (reconstruct cs ps)
reconstruct (_:cs) (p:ps) = p : (reconstruct cs ps)
reconstruct _ _ = []

main :: IO ()
main = do
  cipher <- getContents
  let cipher' = filter isAlpha cipher
      keylens = take 5 $ vigenere_keylens cipher' (length cipher' `div` 30)
      plains = [decrypt_vigenere cipher' keylen | keylen <- keylens]
      printResult (key, plain) = do
        putStrLn
          $ key
              ++ " "
              ++ reconstruct cipher plain
              ++ " "
              ++ (printf "%.4f" (ic plain))
  mapM_ printResult plains
