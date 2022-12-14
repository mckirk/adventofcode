{-# LANGUAGE GADTs #-}

import Control.Lens
import Data.List
import Data.Map qualified as M
import Data.Maybe
import Data.Void
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

type GridPos = (Int, Int)

type Grid = M.Map GridPos Bool

parseLines :: Parser [[GridPos]]
parseLines = some parseLine <* eof
  where
    parseInt = read <$> some digitChar
    parseTuple = (,) <$> parseInt <* char ',' <*> parseInt
    parseLine = parseTuple `sepBy1` string " -> " <* newline

posLens :: Ord k1 => Ord k2 => (k1, k2) -> Lens' (M.Map (k1, k2) v) (Maybe v)
posLens (x, y) = at (x, y)

addPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

move (x1, y1) (x2, y2) = (x1 + signum (x2 - x1), y1 + signum (y2 - y1))

addRock :: Grid -> [GridPos] -> Grid
addRock grid (p1 : p2 : ps) = addRock (drawLine grid p1 p2) (p2 : ps)
  where
    setRock p = posLens p ?~ True
    drawLine grid p1 p2 =
      let drawn = setRock p1 grid
       in if p1 == p2
            then drawn
            else drawLine drawn (move p1 p2) p2
addRock grid _ = grid

addSand :: Maybe Int -> Grid -> Maybe Grid
addSand floor grid = (\p -> posLens p ?~ True $ grid) <$> finalPos
  where
    emptyPos p@(_, y) = Just y /= floor && maybe True not (grid ^. posLens p)
    simSand p@(_, y) = case (shouldAbort, firstEmpty) of
      (True, _) -> Nothing
      (False, Nothing) -> Just p
      (False, Just firstEmpty') -> simSand firstEmpty'
      where
        dirs = [(0, 1), (-1, 1), (1, 1)]
        candidates = map (addPos p) dirs
        firstEmpty = find emptyPos candidates
        belowGrid = y > maximum (map snd $ M.keys grid)
        shouldAbort = case floor of
          Nothing -> belowGrid
          Just _ -> not $ emptyPos p
    finalPos = simSand (500, 0)

iterateSand :: Maybe Int -> Grid -> (Int, Grid)
iterateSand floor grid = iterate' (0, grid)
  where
    iterate' (count, grid) =
      case addSand floor grid of
        Just newGrid -> iterate' (succ count, newGrid)
        Nothing -> (count, grid)

main :: IO ()
main = do
  contents <- readFile "input.txt"

  rockLines <-
    ( case parse parseLines "input.txt" contents of
        Left err -> print err >> return []
        Right rockLines -> return rockLines
      )

  let highestY = maximum $ concatMap (map snd) rockLines

  let startGrid = M.empty
  let rockGrid = foldl addRock startGrid rockLines

  let (count, _) = iterateSand Nothing rockGrid
  print count

  let (count, _) = iterateSand (Just $ highestY + 2) rockGrid
  print count
