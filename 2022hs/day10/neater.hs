data Instr = Noop | Addx Int deriving (Show)

data ProgramState = State
  { cpu :: (Int, Int),
    signalCycles :: [Int],
    signalStrengths :: [Int],
    screen :: [[Char]]
  }

parse :: String -> Instr
parse l =
  parse' $ words l
  where
    parse' ["noop"] = Noop
    parse' ["addx", n] = Addx $ read n

modGrid :: [[a]] -> Int -> Int -> a -> [[a]]
modGrid grid x y v =
  map (\y' -> map (\x' -> if x == x' && y == y' then v else grid !! y' !! x') [0 .. length (head grid) - 1]) [0 .. length grid - 1]

draw :: [[Char]] -> (Int, Int) -> [[Char]]
draw grid (c, x) =
  if abs (xPos - x) <= 1
    then modGrid grid xPos yPos '#'
    else grid
  where
    xDim = length (head grid)
    xPos = (c - 1) `mod` xDim
    yPos = (c - 1) `div` xDim

exec :: ProgramState -> Instr -> ProgramState
exec s instr =
  case signalCycles s of
    signalCycle : rest ->
      if newC > signalCycle
        then
          newS
            { signalCycles = rest,
              signalStrengths = (signalCycle * x) : signalStrengths s
            }
        else newS
    _ -> newS
  where
    (c, x) = cpu s

    exec' Noop = (c + 1, x)
    exec' (Addx i) = (c + 2, x + i)

    drawHelper Noop = draw (screen s) (c, x)
    drawHelper (Addx _) = draw (draw (screen s) (c, x)) (c + 1, x)

    newCPU@(newC, newX) = exec' instr
    newS = s {cpu = newCPU, screen = drawHelper instr}

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let ls = lines contents
  let initCPU = (1, 1)
  let initCycles = [20, 60, 100, 140, 180, 220]
  let initScreen = replicate 6 (replicate 40 '.')
  let initState = State {cpu = initCPU, signalCycles = initCycles, signalStrengths = [], screen = initScreen}
  let finalState = foldl exec initState (map parse ls)
  print $ sum $ signalStrengths finalState
  mapM_ print (screen finalState)