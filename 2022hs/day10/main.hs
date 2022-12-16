import Data.Foldable (foldlM)
import Data.Set qualified as Set

data Instr = Noop | Addx Int deriving (Show)

parse l =
  parse' $ words l
  where
    parse' ["noop"] = Noop
    parse' ["addx", n] = Addx $ read n

exec (c, x) Noop = (c + 1, x)
exec (c, x) (Addx i) = (c + 2, x + i)

draw' grid (c, x) =
  let yPos = (c-1) `div` 40
      xPos = (c-1) `mod` 40
   in if abs (xPos - x) <= 1
        then map (\y' -> map (\x' -> if xPos == x' && yPos == y' then '#' else grid !! y' !! x') [0 .. 39]) [0 .. 5]
        else grid

draw grid (c, x) Noop =
  draw' grid (c, x)
draw grid (c, x) (Addx _) =
  draw' (draw' grid (c, x)) (c + 1, x)

main = do
  contents <- readFile "input.txt"
  let ls = lines contents
  let importantCycles = [20, 60, 100, 140, 180, 220]
  let grid = replicate 6 (replicate 40 '.')
  (_, _, out, outGrid) <-
    foldlM
      ( \(s@(c, x), ic, out, grid) l -> do
          let instr = parse l
          let newS@(newC, newX) = exec s instr
          let newGrid = draw grid s instr
          -- print newS
          return
            ( case ic of
                count : rest ->
                  if newC > count
                    then (newS, rest, (count * x) : out, newGrid)
                    else (newS, count : rest, out, newGrid)
                r -> (newS, r, out, newGrid)
            )
      )
      ((1, 1), importantCycles, [], grid)
      ls
  print $ sum out
  mapM_ print outGrid