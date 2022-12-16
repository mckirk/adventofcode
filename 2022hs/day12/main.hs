import Data.Char (ord)
import Data.List (elemIndices)
import Data.Map qualified as Map
import GHC.Data.Maybe (mapMaybe)

type Pos = (Int, Int)

type Path = Int

type HeightMap = [[Int]]

translHeight :: Char -> Int
translHeight 'S' = 0
translHeight 'E' = 25
translHeight c = ord c - ord 'a'

checkDims :: (Int, Int) -> (Int, Int) -> Bool
checkDims (x, y) (xDim, yDim) = x >= 0 && x < xDim && y >= 0 && y < yDim

findPath :: HeightMap -> Pos -> Pos -> Maybe Path
findPath heightMap end start = backtrack end
  where
    dim@(xDim, yDim) = (length (head heightMap), length heightMap)

    getHeight (x, y) = heightMap !! y !! x

    getNeighbors p@(x, y) =
      let height = getHeight p
       in mapMaybe
            ( \(xd, yd) ->
                let p'@(x', y') = (x + xd, y + yd)
                    height' = getHeight p'
                 in if checkDims p' dim && height' - height <= 1
                      then Just p'
                      else Nothing
            )
            [(-1, 0), (1, 0), (0, -1), (0, 1)]

    buildDirBack :: Map.Map Pos Pos -> [Pos] -> Map.Map Pos Pos
    buildDirBack m [] = m
    buildDirBack m (p : ps) =
      let newNeighbors = filter (`Map.notMember` m) $ getNeighbors p
          newM = foldl (\m neighbor -> Map.insert neighbor p m) m newNeighbors
       in buildDirBack newM (ps ++ newNeighbors)

    dirBack = buildDirBack Map.empty [start]

    backtrack :: Pos -> Maybe Int
    backtrack from
      | from == start = Just 0
      | otherwise = do
          prev <- Map.lookup from dirBack
          (1 +) <$> backtrack prev

main = do
  ls <- lines <$> readFile "input.txt"
  let heightMap = map (map translHeight) ls

  let start = head $ findPos ls 'S'
  let end = head $ findPos ls 'E'
  print $ findPath heightMap end start

  let startPoints = findPos heightMap 0
  print $ minimum $ mapMaybe (findPath heightMap end) startPoints
  where
    findPos ls c = concatMap (\(l, i) -> (,i) <$> elemIndices c (ls !! i)) $ zip ls [0 ..]