{-
Example run:
missionaries = 3
cannibals = 3
boat capacity = 2
State {left = (0,0), right = (3,3), boatSide = True}
State {left = (0,2), right = (3,1), boatSide = False}
State {left = (0,1), right = (3,2), boatSide = True}
State {left = (0,3), right = (3,0), boatSide = False}
State {left = (0,2), right = (3,1), boatSide = True}
State {left = (2,2), right = (1,1), boatSide = False}
State {left = (1,1), right = (2,2), boatSide = True}
State {left = (3,1), right = (0,2), boatSide = False}
State {left = (3,0), right = (0,3), boatSide = True}
State {left = (3,2), right = (0,1), boatSide = False}
State {left = (3,1), right = (0,2), boatSide = True}
State {left = (3,3), right = (0,0), boatSide = False}
Minimum iterations of boat: 11
-}

module Main where
import Control.Monad
import Data.Map.Strict as Map hiding(filter)

-- state representation
-- Tuple (x,y) => x missionaries, cannibals
-- Last Int is the boat side, 0 - Left, 1 - Right
data State = State {
  left :: (Int, Int),
  right :: (Int, Int),
  boatSide :: Bool
}
  deriving (Show, Eq, Ord)

data Move = Move Int Int deriving Show

getMoves :: State -> Int -> [Move]
getMoves (State l r boat) cap = do
    x <- [0.. (fst side)]
    y <- [0.. (snd side)]
    guard $ x + y <= cap && (y <= x || x == 0) && x + y > 0
    return $ Move x y
  where side = if (not boat)
               then l else r

next :: State -> [Move] -> [State]
next (State (m1, c1) (m2, c2) boat) moves = do
  (Move m c) <- moves
  let newState = if not boat
                 then (State (m1 - m, c1 - c) (m2 + m, c2 + c) (not boat))
                 else (State (m1 + m, c1 + c) (m2 - m, c2 - c) (not boat))
  guard $ isValid newState
  return newState

isValid :: State -> Bool
isValid (State (m1, c1) (m2, c2) boat) =
  if ((c1 <= m1 || m1 == 0) && (c2 <= m2 || m2 == 0))
  then True else False

test :: State -> Int -> Int -> Bool
test (State (0, 0) (m2, c2) True) m c =
  if (m2 == m && c2 == c) then True else False
test _ _ _ = False

insertIntoMap :: [State] -> State -> Map State State -> Map State State
insertIntoMap [] _ parentMap = parentMap
insertIntoMap (x:xs) parent parentMap =
  insertIntoMap xs parent (Map.insert x parent parentMap)

mainLoop :: Int -> Int -> Int -> [State] -> [State] -> Map State State -> Maybe (State, Map State State)
mainLoop m c cap [] _ parentMap = Nothing
mainLoop m c cap omega@(x:xs) chi parentMap
  | test x m c = Just (x, parentMap)
  | otherwise = let moves = getMoves x cap
                    newStates = next x moves
                    isNew = \s -> not (s `elem` omega) && not (s `elem` chi)
                    filtered = filter isNew newStates
                    newMap = insertIntoMap filtered x parentMap
                in  mainLoop m c cap (xs ++ filtered) (x:chi) newMap

printAns :: State -> Map State State -> Int -> IO ()
printAns state parentMap count =
  case Map.lookup state parentMap of
    Just parent -> do putStrLn $ show parent
                      printAns parent parentMap (count + 1)
    Nothing -> do putStrLn $ "Minimum iterations of boat: " ++ show count
                  return ()

ans :: Maybe (State, Map State State) -> IO ()
ans (Just (final, parentMap)) = do
  putStrLn $ show final
  printAns final parentMap 0
ans _ = putStrLn "No answer found."

main :: IO ()
main = do m <- readLn
          c <- readLn
          cap <- readLn
          ans $ mainLoop m c cap [(State (m,c) (0,0) False)] [] Map.empty
