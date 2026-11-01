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
