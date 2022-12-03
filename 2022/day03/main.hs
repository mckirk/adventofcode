import System.IO (readFile)
import Data.List (sortBy)
import Text.Printf (printf)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char (ord)


charScore :: Char -> Int
charScore c =
    let code = ord c in
    if code >= ord 'a' && code <= ord 'z'
        then 1 + code - ord 'a'
        else 27 + code - ord 'A'


processLines1 [] score = score
processLines1 (l:ls) score =
    if l == "" then
        processLines1 ls score
    else
        let halfLen = length l `div` 2 in
        let (first, second) = splitAt halfLen l in
        let both = Set.elemAt 0 $ Set.fromList first `Set.intersection` Set.fromList second in
        processLines1 ls (score + charScore both)


processLines2 (l1:l2:l3:ls) score =
    let all = Set.elemAt 0 $ foldl1 Set.intersection $ map Set.fromList [l1,l2,l3] in
    processLines2 ls (score + charScore all)
processLines2 _ score = score

main = do
    contents <- readFile "input.txt"
    let ls = lines contents
    let score1 = processLines1 ls 0
    let score2 = processLines2 ls 0
    printf "1: %d\n" score1
    printf "2: %d\n" score2