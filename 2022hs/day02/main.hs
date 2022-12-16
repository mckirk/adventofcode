import System.IO (readFile)
import Control.Monad (forM_)
import Data.List (sortBy)
import Text.Printf (printf)

shapeScore "A" = 1
shapeScore "B" = 2
shapeScore "C" = 3

playScore "A" "B" = 6
playScore "B" "C" = 6
playScore "C" "A" = 6
playScore elf you = if elf == you then 3 else 0

transl1 "X" = "A"
transl1 "Y" = "B"
transl1 "Z" = "C"

gameScore1 elf you =
    let translated = transl1 you in
    let sScore = shapeScore translated in
        playScore elf translated + sScore


transl2 "A" "X" = "C"
transl2 "B" "X" = "A"
transl2 "C" "X" = "B"
transl2  x  "Y" =  x
transl2 "A" "Z" = "B"
transl2 "B" "Z" = "C"
transl2 "C" "Z" = "A"

gameScore2 elf you =
    let translated = transl2 elf you in
    let sScore = shapeScore translated in
        playScore elf translated + sScore


processLines _ [] score = score
processLines gameScore (l:ls) score =
    if l == "" then
        processLines gameScore ls score
    else
        let [elf, you] = words l in
        processLines gameScore ls (score + gameScore elf you)

main = do
    contents <- readFile "input.txt"
    let ls = lines contents
    let score1 :: Integer = processLines gameScore1 ls 0
    let score2 :: Integer = processLines gameScore2 ls 0
    printf "1: %d\n" score1
    printf "2: %d\n" score2