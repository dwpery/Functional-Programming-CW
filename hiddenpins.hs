main :: IO ()
main
  = print ( filter selector2 generator2 )

-- Generates All possibole sets of PINs
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

selector2 :: (String, String) -> Bool
selector2 (c, t)
  = square == telePIN && square >= 1000 && square <= 9999
  where
    telePIN = read t :: Int
    -- Generates Tele PIN from last 2 digits of Card PIN
    root = read (drop 2 c) :: Int
    square = root * root