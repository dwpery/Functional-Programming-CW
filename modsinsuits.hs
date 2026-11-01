import Data.List (nub)

main :: IO ()
main
  = print x_selector3

generator3 :: [(((Char, Int), (Char, Int)), ((Char, Int), (Char, Int)))]
generator3
  = [(h1, h2)
  | h1@((s1, v1), (s2, v2)) <- hands
  , h2@((s3, v3), (s4, v4)) <- hands

  , s1 /= s2 && s1 /= s3 && s1 /= s4
  , s2 /= s1 && s2 /= s3 && s2 /= s4
  , s3 /= s1 && s3 /= s2 && s3 /= s4

  , let joScore = scoreHand h1
  , let boScore = scoreHand h2
  , joScore == fromIntegral (floor joScore)
  , boScore == fromIntegral (floor boScore)
  , joScore > 0
  , joScore == -boScore
  ]
  where
    deck = [(s, v) | s <- ['C', 'D', 'H', 'S'], v <- [1..13]]
    hands = [(c1, c2) | c1 <- deck, c2 <- deck, c1 /= c2]

modify :: (Char, Int) -> Int -> Double
modify (s, v) otherV
  = case s of
    'C' -> fromIntegral (-v)
    'D' -> fromIntegral v / fromIntegral otherV
    'H' -> fromIntegral v / 2.0
    'S' -> fromIntegral (v * v)

scoreHand :: ((Char, Int), (Char, Int)) -> Double
scoreHand ((s1, v1), (s2, v2))
  | v1 == v2  = 0.0
  | otherwise = modify (s1, v1) v2 + modify (s2, v2) v1

selector3 :: (((Char, Int), (Char, Int)), ((Char, Int), (Char, Int))) -> Bool
selector3 (h1@((s1, v1), (s2, v2)), h2@((s3, v3), (s4, v4))) =
  let joScore = scoreHand h1
      boScore = scoreHand h2
      whole x = x == fromIntegral (round x)
      deck = [(s, v) | s <- ['C', 'D', 'H', 'S'], v <- [1..13]]
      dealt = [(s1, v1), (s2, v2), (s3, v3), (s4, v4)]
      remDeck = [c | c <- deck, not (c `elem` dealt)]
      validInitial =
        length (nub [s1, s2, s3, s4]) == 4
        && length (nub [v1, v2, v3, v4]) == 4
        && joScore > 0
        && boScore < 0
        && whole joScore
        && whole boScore
        && joScore == -boScore

      validExchange (c2', c4') =
        let newH1 = ((s1, v1), c2')
            newH2 = ((s3, v3), c4')

            suits = [s1, fst c2', s3, fst c4']

        in scoreHand newH1 == joScore
           && scoreHand newH2 == boScore
           && length (nub suits) == 4

  in validInitial &&
     any validExchange
       [(c2', c4') |
          c2' <- remDeck,
          c4' <- remDeck,
          c2' /= c4']

x_selector3 :: Int
x_selector3
  = length [ t | t <- [ t1 , t2 , t3 , t4 , t5 ] , t ]
  where
    t1 = selector3 ((( 'D' ,7) ,( 'S' ,7) ) ,(( 'C' ,1) ,('H' ,2) ) ) == False
    t2 = selector3 ((( 'D' ,3) ,( 'H' ,3) ) ,(( 'C',3) ,('S' ,3) ) ) == False
    t3 = selector3 ((( 'S' ,1) ,( 'D' ,4) ) ,(( 'C', 7) ,('H' ,4) ) ) == False
    t4 = selector3 ((( 'D' ,10) ,( 'H' ,2) ) ,(( 'C' ,10) ,('S' ,2) ) ) == False
    t5 = selector3 ((( 'D' ,2) ,( 'H' ,2) ) ,(( 'C' ,13) ,('S' ,13) ) ) == False