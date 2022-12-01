import System.IO
import Control.Monad (forM_)
import Data.List (sortBy)
import Text.Printf (printf)


processLines :: (Num t, Read t) => [String] -> [t] -> t -> [t]
processLines [] numsSoFar acc = numsSoFar ++ [acc]
processLines (l:ls) numsSoFar acc =
    if l == "" then
        processLines ls (numsSoFar ++ [acc]) 0
    else
        processLines ls numsSoFar (acc + read l)

main = do
    withFile "input" ReadMode (\handle -> do
        contents <- hGetContents handle
        let nums :: [Integer] = processLines (lines contents) [] 0
        printf "1: %d\n" (maximum nums)
        let sorted = sortBy (flip compare) nums
        printf "2: %d\n"(sum $ take 3 sorted))