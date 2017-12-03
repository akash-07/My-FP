import Data.List

type Point = (Double, Double)
data Direction = LeftTurn | RightTurn | Straight
                 deriving (Show)

p0 = (0.0, 0.0)
p1 = (1.0, 1.0)
p2 = (2.0, 2.0)
p3 = (4.0, 4.0)
p4 = (0.0, 3.0)
p5 = (1.0, 2.0)
p6 = (3.0, 1.0)
p7 = (3.0, 3.0)

ps = [p0, p1, p2, p3, p4, p5, p6, p7]

ps1 = [(4.4, 14.0), (6.7, 15.25), (6.9, 12.8), (2.1, 11.1), (9.5, 14.9),
 (13.2, 11.9), (10.3, 12.3), (6.8, 9.5), (3.3, 7.7), (0.6, 5.1), (5.3, 2.4),
 (8.45, 4.7), (11.5, 9.6), (13.8, 7.3), (12.9, 3.1), (11.0, 1.1)]

getTurn :: Point -> Point -> Point -> Direction
getTurn (x1,y1) (x2,y2) (x3, y3) = if(z > 0)
         then LeftTurn
         else if(z < 0)
         then RightTurn
         else Straight
         where z = (x2 - x1)*(y3 - y1) - (y2 - y1)*(x3-x1)

findMin :: [Point] -> Point
findMin [p] = p
findMin (p:ps)
  | snd p < snd minps = p
  | snd p == snd minps && fst p <= fst minps = p
  | otherwise = minps
  where minps = (findMin ps)

dist p1 p2 = sqrt ((fst p1 - fst p2) * (snd p1 - snd p2))

compareAngle minp p1 p2  = case turn of
              LeftTurn -> LT
              RightTurn -> GT
              Straight -> let d1 = dist minp p1
                              d2 = dist minp p2
                          in if(d1 < d2) then LT else GT
              where turn = getTurn minp p1 p2

sortedPoints :: [Point] -> [Point]
sortedPoints ps = minp : (filter fn (sortBy (compareAngle minp) ps))
                  where minp = findMin ps
                        fn p = p /= minp

convex :: [Point] -> [Point] -> [Point]
convex [] accPs = accPs
convex leftPs accPs =
  let p2 = head accPs
      p1 = head (tail accPs)
      angle = getTurn p1 p2 (head leftPs)
  in case angle of
      LeftTurn -> convex(tail leftPs) ((head leftPs : accPs))
      _ -> convex (tail leftPs) ((head leftPs : tail accPs))

convexHull :: [Point] -> [Point]
convexHull [] = []
convexHull [x] = [x]
convexHull ps = convex xs [x1,x]
            where (x:x1:xs) = sortedPoints ps
