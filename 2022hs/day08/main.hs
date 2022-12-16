import Data.Set qualified as Set

parse :: [String] -> [[Int]]
parse = map (map (read . (: [])))

countVisible :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> [(Int, Int)]
countVisible grid curHeight xDir yDir x y =
  if y < 0 || x < 0 || y >= length grid || x >= length (head grid)
    then []
    else
      let thisHeight = grid !! y !! x
          rest = countVisible grid (max curHeight thisHeight) xDir yDir (x + xDir) (y + yDir)
       in if thisHeight > curHeight then (x, y) : rest else rest

viewingDist :: [[Int]] -> Maybe Int -> Int -> Int -> Int -> Int -> Int
viewingDist grid treeHeight xDir yDir x y =
  if y < 0 || x < 0 || y >= length grid || x >= length (head grid)
    then 0
    else
      let thisHeight = grid !! y !! x
       in case treeHeight of
            Just height ->
              if thisHeight >= height
                then 1
                else 1 + viewingDist grid treeHeight xDir yDir (x + xDir) (y + yDir)
            Nothing -> viewingDist grid (Just thisHeight) xDir yDir (x + xDir) (y + yDir)

main = do
  contents <- readFile "input.txt"
  let grid = parse $ lines contents
  let xDim = length $ head grid
  let yDim = length grid

  let seenRows = concatMap (\y -> countVisible grid (-1) 1 0 0 y ++ countVisible grid (-1) (-1) 0 (xDim - 1) y) [0 .. yDim - 1]
  let seenCols = concatMap (\x -> countVisible grid (-1) 0 1 x 0 ++ countVisible grid (-1) 0 (-1) x (yDim - 1)) [0 .. xDim - 1]
  print $ length $ Set.fromList (seenRows ++ seenCols)

  let scenicScores = concatMap (\y -> map (\x -> product $ map (\(xDir, yDir) -> viewingDist grid Nothing xDir yDir x y) [(0, 1), (0, -1), (1, 0), (-1, 0)]) [0 .. xDim - 1]) [0 .. yDim - 1]
  print $ maximum scenicScores

  return ()