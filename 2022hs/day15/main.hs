{-# LANGUAGE RecordWildCards #-}

import Control.Lens
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Set qualified as S
import Data.Void
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

type GridPos = (Int, Int)

manDist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

type Interval = (Int, Int)

countElems :: [Interval] -> Int
countElems is = countElems' start sorted
  where
    sorted = sort is
    start = minimum $ map fst sorted
    countElems' _ [] = 0
    countElems' lowerBound ((s, e) : is) =
      max 0 (e - max s lowerBound) + countElems' (max lowerBound e) is

isIn :: Int -> Interval -> Bool
x `isIn` (start, end) = x >= start && x < end

intervalAround :: Int -> Int -> Maybe Interval
intervalAround center dist
  | dist < 0 = Nothing
  | otherwise = Just (center - dist - 1, center + dist)

findGap :: Interval -> [Interval] -> Maybe Int
findGap _ [] = Nothing
findGap (s, e) ((i1, i2) : is)
  | i1 >= e = Nothing
  | i1 > s = Just i1
  | otherwise = findGap (max s i2, e) is

data Scan = Scan
  { sensor :: GridPos,
    dist :: Int,
    beacon :: GridPos
  }

parseLines :: Parser [Scan]
parseLines = some parseLine <* eof
  where
    decimal :: Parser Char
    decimal = char '-' <|> digitChar
    parseInt :: Parser Int
    parseInt = read <$> some decimal
    parseTuple :: Parser GridPos
    parseTuple = do
      string "x="
      x <- parseInt
      string ", y="
      y <- parseInt
      return (x, y)
    parseLine :: Parser Scan
    parseLine = do
      string "Sensor at "
      sensor <- parseTuple
      string ": closest beacon is at "
      beacon <- parseTuple
      let dist = manDist sensor beacon
      newline
      return $ Scan {..}

getBlocked :: [Scan] -> Int -> [Interval]
getBlocked [] _ = []
getBlocked (Scan {..} : ss) row = toList interval ++ rest
  where
    (sensorX, sensorY) = sensor
    distToClosest = abs $ row - sensorY
    remainingDist = dist - distToClosest
    interval = intervalAround sensorX remainingDist
    rest = getBlocked ss row

getBeacons :: [Scan] -> Int -> [Int]
getBeacons [] _ = []
getBeacons (Scan {..} : ss) row = if snd beacon == row then fst beacon : rest else rest
  where
    rest = getBeacons ss row

countBlocked :: [Scan] -> Int -> Int
countBlocked scans row = countElems blockedIntervals - length countedBeacons
  where
    blockedIntervals = getBlocked scans row
    beacons = getBeacons scans row
    countedBeacons = S.fromList $ filter (\b -> any (b `isIn`) blockedIntervals) beacons

findDistressBeacon :: [Scan] -> Int -> GridPos
findDistressBeacon scans limit = findIt blockedIntervals
  where
    searchIval = (0, limit)
    blockedIntervals = map (\y -> (y, getBlocked scans y)) [0 .. limit]
    findIt ((y, is) : r) = case findGap searchIval (sort is) of Just x -> (x, y); Nothing -> findIt r
    findIt [] = undefined

main :: IO ()
main = do
  contents <- readFile "input.txt"

  let (row, limit) = (2000000, 4000000)

  case parse parseLines "input.txt" contents of
    Left err -> print err
    Right scans -> do
      print $ countBlocked scans row

      let b@(beaconX, beaconY) = findDistressBeacon scans limit
      print b
      print $ beaconX * 4000000 + beaconY
