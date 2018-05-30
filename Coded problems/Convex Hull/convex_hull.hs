import Text.Printf

import Data.List

type Point = (Double, Double)

comparePoint :: Point -> Point -> Ordering
comparePoint (x1, y1) (x2, y2)
  | y1 < y2 || (y1 == y2 && x1 < x2) = LT
  | otherwise = GT

sortPoints :: [Point] -> [Point]
sortPoints = sortBy comparePoint

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2

compareAngle :: Point -> Point -> Point -> Ordering
compareAngle minP@(x1, y1) p1@(x2, y2) p2@(x3, y3)
  | z > 0 = LT
  | z < 0 = GT
  | otherwise =   if (distance minP p1) < (distance minP p2)
                  then LT
                  else GT
      where z = (x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1)

sortPointsByAngle :: [Point] -> [Point]
sortPointsByAngle ps = head ps : sortBy (compareAngle (head ps)) (tail ps)

dropSame :: Point -> [Point] -> [Point]
dropSame minP [p] = [p]
dropSame minP (p1: p2: ps)
  | z == 0 = if (distance minP p1) < (distance minP p2)
             then dropSame minP (p2: ps)
             else dropSame minP (p1: ps)
  | otherwise = p1 : dropSame minP (p2 : ps)
      where z = (fst p1 - fst minP) * (snd p2 - snd minP) - (fst p2 - fst minP) * (snd p1 - snd minP)

convexHull :: [Point] -> [Point]
convexHull ps =
  let sortedPs = sortPointsByAngle (sortPoints ps)
      p0 = head sortedPs
      pRest = dropSame p0 (tail sortedPs)
  in convex [head pRest, p0] (tail pRest)

getTurn :: Point -> Point -> Point -> Ordering
getTurn (x1, y1) (x2, y2) (x3, y3)
  | z > 0 = LT
  | z < 0 = GT
  | z == 0 = EQ
    where z = (x2 - x1) * (y3 - y2) - (y2 - y1) * (x3 - x2)

convex :: [Point] -> [Point] -> [Point]
convex [p] leftPs = convex (head leftPs: [p]) (tail leftPs)
convex accPs [] = reverse accPs
convex (p1 : p0 : ps) (p2 : restLeftPs) =
  case getTurn p0 p1 p2 of
    LT -> convex (p2 : p1 : p0 : ps) restLeftPs
    _ -> convex (p0 : ps) (p2 : restLeftPs)

solve :: [Point] -> Double
solve points = sum $ map (uncurry distance) $ zip points (drop 1 points ++ take 1 points)


main :: IO ()
main = do
  n <- readLn :: IO Int
  content <- getContents
  let
    points = map (\[x, y] -> (x, y)). map (map (read::String->Double)). map words. lines $ content
    ans = solve (convexHull points)
  printf "%.1f\n" ans
  --putStrLn $ show (convexHull points)
