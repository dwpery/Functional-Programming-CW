-- Tele PIN + Card PIN both 4 digits
-- Tele PIN is perfect squre
-- Root from last 2 didigts of card pin
-- Card & Tele digits are different, Card 

main :: IO ()
main = print x_generator2

-- Generates 
generator2 :: [(String, String)]
generator2
  = [(c, t) 
  -- Generates digits of Card PIN 
  | d1 <- ['0'..'9']
  , d2 <- ['0'..d1]
  , d2 /= d1
  , d3 <- ['0'..d2]
  , d3 /= d2
  , d4 <- ['0'..d3]
  , d4 /= d3
  , let c = [d1, d2, d3, d4]
  -- Generates digits of Tele PIN
  , t1 <- ['0'..'9']
  , t2 <- ['0'..'9']
  , t3 <- ['0'..'9']
  , t4 <- ['0'..'9']
  , let t =[t1, t2, t3, t4]
  -- Ensure no digits are shared
  , not (any (`elem` t) c)
  ]

{-
-- Generates Tele PIN from last 2 digits of Card PIN
  , let root = read [d3, d4] :: Int
  , let square = root * root
  , square >= 1000 && square <= 9999
  , let t = show square
-}

x_generator2 :: Int
x_generator2
  = length [ t | t <- [ t1 , t2 , t3 , t4 , t5 ] , t ]
  where
    t1 = ( "5430" , "2661" ) `elem` generator2
    t2 = ( "6531" , "7424" ) `elem` generator2
    t3 = ( "8740" , "2161" ) `elem` generator2
    t4 = ( "9280" , "6461" ) `notElem` generator2
    t5 = ( "1232" , "5563" ) `notElem` generator2