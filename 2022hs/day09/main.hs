import Data.Foldable (foldlM)
import Data.Set qualified as Set

type Knot = (Int, Int)
type Rope = [Knot]

move :: Knot -> String -> Knot
move (x, y) instr =
  case instr of
    "R" -> (x + 1, y)
    "L" -> (x - 1, y)
    "U" -> (x, y + 1)
    "D" -> (x, y - 1)

opp "R" = "L"
opp "L" = "R"
opp "U" = "D"
opp "D" = "U"

dist :: Knot -> Knot -> Int
dist (x1, y1) (x2, y2) = max (abs (x1 - x2)) (abs (y1 - y2))

drag :: Knot -> Knot -> Knot
drag (hx, hy) (kx, ky) =
  let xDelta = hx - kx
      yDelta = hy - ky
   in if abs xDelta > abs yDelta
        then (hx - signum xDelta, hy)
        else
          if abs xDelta < abs yDelta
            then (hx, hy - signum yDelta)
            else (hx - signum xDelta, hy - signum yDelta)

check :: [Knot] -> [Knot]
check (headPos : knotPos : rest) =
  let d = dist headPos knotPos
   in if d > 1
        then let newKnotPos = drag headPos knotPos in (headPos : check (newKnotPos : rest))
        else headPos : check (knotPos : rest)
check r = r

sim1 :: Rope -> String -> Rope
sim1 (headPos : rest) instr =
  let newHeadPos = move headPos instr
   in check (newHeadPos : rest)
sim1 r _ = r

sim :: Rope -> String -> Int -> (Rope, [(Int, Int)])
sim pos instr 0 = (pos, [])
sim pos instr n =
  let newPos = sim1 pos instr
      (finalPos, newTrail) = sim newPos instr (n - 1)
   in (finalPos, last newPos : newTrail)

main = do
  contents <- readFile "input.txt"
  let ls = lines contents
  let (_, trail) = foldl (\(pos, trail) [instr, count] -> let (newPos, newTrail) = sim pos instr (read count) in (newPos, trail ++ newTrail)) (replicate 2 (0, 0), []) $ map words ls
  print $ length $ Set.fromList trail

  (_, trail2) <-
    foldlM
      ( \(pos, trail) [instr, count] -> do
          let (newPos, newTrail) = sim pos instr (read count)
          -- print newPos
          return (newPos, trail ++ newTrail)
      )
      (replicate 10 (0, 0), [])
      $ map words ls
  print $ length $ Set.fromList trail2

  return ()