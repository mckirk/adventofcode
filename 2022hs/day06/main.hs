import System.IO (readFile)
import qualified Data.Set as Set

detect num s@(c:cs) count =
    if length (Set.elems $ Set.fromList $ take num s) == num
        then count + num
        else detect num cs (count + 1)
detect _ _ _ = undefined

main = do
    contents <- readFile "input.txt"
    let ls = lines contents 
    print $ detect 4 (head ls) 0
    print $ detect 14 (head ls) 0