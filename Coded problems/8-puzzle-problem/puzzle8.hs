{-
Example run:

Enter initial state:
12356 784
Enter final state:

123586 74
1 2 3
5 8 6
  7 4

1 2 3
5 8 6
7   4

1 2 3
5   6
7 8 4

1 2 3
5 6
7 8 4

Minimum path length: 3

-}

import Data.List
import Data.List.Split
import Data.Map as Map hiding (map, filter)

swap :: Int -> Int -> [Char] -> [Char]
swap i j xs = let (i', j') = if i > j then (j ,i) else (i, j)
                  left = take i' xs
                  middle = drop (i'+1) $ take j' xs
                  i_elem = xs !! i'
                  right = drop (j'+1) xs
                  j_elem = xs !! j'
              in left ++ [j_elem] ++ middle ++ [i_elem] ++ right

printGrid :: [Char] -> IO()
printGrid xs = let [x,y,z] = chunksOf 6 $ intersperse ' ' xs
               in do putStrLn x
                     putStrLn y
                     putStrLn z
                     putStrLn ""

type State = [Char]

getMoves :: State -> [Char]
getMoves xs = case ' ' `elemIndex` xs of
  Nothing -> error "Empty block not found"
  Just n -> let l = n `elem` [1,4,7,2,5,8]
                r = n `elem` [0,3,6,1,4,7]
                d = n `elem` [0..5]
                u = n `elem` [3..8]
                pairs = zip [l,r,d,u] ['L','R','D','U']
                filtered = filter (\x -> fst x) pairs
            in map snd filtered

next :: State -> [Char] -> [State]
next state cs = case ' ' `elemIndex` state of
  Nothing -> error "Empty block not found"
  Just n -> do c <- cs
               return $ case c of
                         'L' -> swap n (n-1) state
                         'R' -> swap n (n+1) state
                         'U' -> swap n (n-3) state
                         'D' -> swap n (n+3) state

test :: State -> State -> Bool
test xs ys = xs == ys

-- loop :: finalState -> open -> closed -> accmulated parentMap -> parentMap
loop :: State -> [State] -> [State] -> Map State State -> Maybe (State, Map State State)
loop final [] _ _ = Nothing
loop final open@(x:xs) closed parentMap = if test final x
  then Just (x, parentMap)
  else let moves = getMoves x
           nextStates = next x moves
           filter_fn = \x -> not (x `elem` open || x `elem` closed)
           filtered = filter filter_fn nextStates
           newMap = insertIntoMap filtered x parentMap
       in loop final (xs ++ filtered) (x:closed) newMap

insertIntoMap :: [State] -> State -> Map State State -> Map State State
insertIntoMap [] _ parentMap = parentMap
insertIntoMap (x:xs) parent parentMap =
 insertIntoMap xs parent (Map.insert x parent parentMap)

printAns :: State -> Map State State -> Int -> IO ()
printAns state parentMap count =
 case Map.lookup state parentMap of
   Just parent -> do printGrid parent
                     printAns parent parentMap (count + 1)
   Nothing -> do putStrLn $ "Minimum path length: " ++ show count
                 return ()

ans :: Maybe (State, Map State State) -> IO ()
ans (Just (final, parentMap)) = do
 printGrid final
 printAns final parentMap 0
ans _ = putStrLn "No answer found."

main :: IO ()
main = do putStrLn "Enter initial state: "
          start <- getLine
          putStrLn "Enter final state: "
          final <- getLine
          ans $ loop final [start] [] Map.empty
