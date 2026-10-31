main :: IO ()
main
  = print x_generator4

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
  = factors n == [1,n]
    where
    factors n
      = [f | f <- [1..n], n `mod` f == 0]

x_generator4 :: Int
x_generator4
  = length [ t | t <- [ t1 , t2 , t3 , t4 , t5 ] , t ]
  where
    t1 = "331" `elem` generator4
    t2 = "557" `elem` generator4
    t3 = "593" `elem` generator4
    t4 = "151" `notElem` generator4
    t5 = "991" `notElem` generator4