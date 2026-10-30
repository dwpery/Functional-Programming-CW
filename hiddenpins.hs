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

x_generator2 :: Int
x_generator2
  = length [ t | t <- [ t1 , t2 , t3 , t4 , t5 ] , t ]
  where
    t1 = ( "5430" , "2661" ) `elem` generator2
    t2 = ( "6531" , "7424" ) `elem` generator2
    t3 = ( "8740" , "2161" ) `elem` generator2
    t4 = ( "9280" , "6461" ) `notElem` generator2
    t5 = ( "1232" , "5563" ) `notElem` generator2

selector2 :: (String, String) -> Bool
selector2 (c, t)
  = square == telePIN && square >= 1000 && square <= 9999
  where
    telePIN = read t :: Int
    -- Generates Tele PIN from last 2 digits of Card PIN
    root = read (drop 2 c) :: Int
    square = root * root

x_selector2 :: Int
x_selector2
  = length [ t | t <- [ t1 , t2 , t3 , t4 , t5 ] , t ]
  where
    t1 = selector2 ( "7653" , "2809" ) == True
    t2 = selector2 ( "1193" , "7542" ) == False
    t3 = selector2 ( "3647" , "8210" ) == False
    t4 = selector2 ( "3166" , "9750" ) == False
    t5 = selector2 ( "0394" , "8765" ) == False