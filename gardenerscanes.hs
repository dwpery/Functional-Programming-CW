main :: IO ()
main = print (filter selector1 generator1)

-- Generates halves of 98-inch canes
generator1 :: [(Int, Int, Int, Int)]
generator1
  = [(a, b, c, d) | a <- [1..49], c <- [1..49]
  , let b = 98 - a
  , let d = 98 - c
  ]

-- List insert function
insert :: Ord a => a -> [a] -> [a]
insert e (x:xs)
  | e < x     = e : x : xs
  | otherwise = x : insert e xs
insert e []
  = [e]

-- Insertion Sort function
sort :: Ord a => [a] -> [a]
sort (x:xs)
  = insert x (sort xs)
sort []
  = []

-- Calculates area of triangle with Heron's formula
integerArea :: Int -> Int -> Int -> Maybe Int
integerArea x y z
  | x + y > z && x + z > y && y + z > x && even perim =
      let s      = perim `div` 2
          sqArea = s * (s - x) * (s - y) * (s - z)
          r      = round (sqrt (fromIntegral sqArea :: Double))
      in if sqArea >= 0 && r * r == sqArea
         then Just r
         else Nothing
  | otherwise = Nothing
  where
    perim = x + y + z

selector1 :: (Int, Int, Int, Int) -> Bool
selector1 (a, b, c, d)
  = case sort [a, b, c, d] of
    [p1, p2, p3, p4] ->
      case (integerArea 70 p1 p4, integerArea 70 p2 p3) of
        (Just area1, Just area2) -> abs (area1 - area2) == 504
        _                        -> False
    _ -> False

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