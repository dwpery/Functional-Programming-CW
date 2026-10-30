main :: IO ()
main = print x_selector1

-- Generates halves of 98-inch canes
generator1 :: [(Int, Int, Int, Int)]
generator1
  = [(a, b, c, d) | a <- [1..49], c <- [1..49],
    let b = 98 - a,
    let d = 98 - c
]

-- Test for generator1
x_generator1 :: Int
x_generator1
  = length [ t | t <- [ t1 , t2 , t3 , t4 , t5 ] , t ]
    where
    t1 = ( 3 , 95 , 33 , 65) `elem` generator1
    t2 = ( 6 , 92 , 16 , 82) `elem` generator1
    t3 = (13 , 85 , 43 , 55) `elem` generator1
    t4 = (18 , 87 , 12 , 76) `notElem` generator1
    t5 = (28 , 54 , 7 , 91) `notElem` generator1

--             A    B    C    D
selector1 :: (Int, Int, Int, Int) -> Bool
  =

-- Test for selector1
x_selector1 :: Int
x_selector1
  = length [ t | t <- [ t1 , t2 , t3 , t4 , t5 ] , t ]
    where
      t1 = selector1 (20 ,78 ,42 ,56) == True
      t2 = selector1 (12 ,56 ,78 ,42) == False
      t3 = selector1 (12 ,78 , 9 ,89) == False
      t4 = selector1 (17 , 4 ,13 ,13) == False
      t5 = selector1 (41 ,57 ,16 ,88) == False