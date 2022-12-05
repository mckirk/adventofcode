import System.IO (readFile)
import Data.List (transpose)
import Data.Text (pack,strip,unpack)

type Stack = [Char]

applyMove :: (String -> String) -> (Int,Int,Int) -> [Stack] -> [Stack]
applyMove mod (count,from,to) stacks =
    let taken = mod $ take count (stacks !! (from - 1)) in
    zipWith
        (\i s ->
            if i == from then drop count s
            else if i == to then taken ++ s
            else s)
        [1..] stacks

processInstrs :: (String -> String) -> [String] -> [Stack] -> [Stack]
processInstrs mod (i:is) =
    let ws = words i in
    let [count,from,to] = map (read . (ws !!)) [1,3,5] in
    processInstrs mod is . applyMove mod (count,from,to)
processInstrs _ [] = id

main = do
    contents <- readFile "input.txt"
    let ls = lines contents
    let stackLs = take 8 ls
    let transp = transpose stackLs
    let stacks = map (\i -> unpack $ strip $ pack (transp !! (1 + 4*i))) [0..8]
    let instrLs = drop 10 ls
    let newStack1 = processInstrs reverse instrLs stacks
    let newStack2 = processInstrs id instrLs stacks
    print $ map head newStack1
    print $ map head newStack2