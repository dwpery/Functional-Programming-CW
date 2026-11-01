import Data.List (nub)

main :: IO ()
main
  = print ( filter selector4 generator4 )

generator4 :: [String]
generator4
  = [ [d1, d2, d3]
  | d1 <- "13579"
  , d2 <- "13579"
  , d3 <- "13579"
  , let n    = read [d1, d2, d3] :: Int
  , let revN = read [d3, d2, d1] :: Int
  , prime n 
  , not (prime revN)
  ]

prime :: Int -> Bool
prime n
  = factors n == [1,n] && n > 1
    where
    factors n
      = [f | f <- [1..n], n `mod` f == 0]

-- Generate all three-digit reversible primes
reversiblePrimes :: [String]
reversiblePrimes
  = [ n
    | a <- "13579"
    , b <- "13579"
    , c <- "13579"
    , let n = [a,b,c]
    , prime (read n)
    , prime (read (reverse n))
    ]

-- Construct possible George grids using valid rows
georgeGrids :: [String]
georgeGrids
  = [ r1 ++ r2 ++ r3
    | r1 <- reversiblePrimes
    , r2 <- reversiblePrimes
    , r3 <- reversiblePrimes
    , validGeorge (r1 ++ r2 ++ r3)
    ]

-- Extract the eight numbers from a grid
gridLines :: String -> [String]
gridLines [a,b,c,d,e,f,g,h,i]
  = [ [a,b,c]
    , [d,e,f]
    , [g,h,i]
    , [a,d,g]
    , [b,e,h]
    , [c,f,i]
    , [a,e,i]
    , [c,e,g]
    ]
gridLines _ = []

validGeorge :: String -> Bool
validGeorge g
  = let ns = gridLines g
    in length (nub ns) == 8
       && all reversiblePrime ns

-- Generate grids differing in exactly one square
georgeVariants :: String -> [String]
georgeVariants g
  = [ replaceAt i d g
    | i <- [0..8]
    , d <- "13579"
    , d /= g !! i
    ]

-- Replace a digit at a given position
replaceAt :: Int -> Char -> String -> String
replaceAt i c g
  = take i g ++ [c] ++ drop (i+1) g

-- Check whether a number is prime forwards
isPrimeNumber :: String -> Bool
isPrimeNumber n
  = prime (read n)

-- Check whether a number is prime forwards and backwards
reversiblePrime :: String -> Bool
reversiblePrime n
  = isPrimeNumber n
    && isPrimeNumber (reverse n)

selector4 :: String -> Bool
selector4 n
  = length n == 3
    && all (`elem` "13579") n
    && isPrimeNumber n
    && not (isPrimeNumber (reverse n))
    && any (validMartha n) georgeGrids

validMartha :: String -> String -> Bool
validMartha n g
  = any check (georgeVariants g)
    where
      check m
        = let ns = gridLines m
          in all isPrimeNumber ns
             && length (nub ns) == 8
             && filter (not . reversiblePrime) ns == [n]
