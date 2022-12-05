import System.IO (readFile)
import Data.List (sortBy)
import Text.Printf (printf)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char (ord)


-- I import qualified so that it's clear which
-- functions are from the parsec library:
import qualified Text.Parsec as Parsec

-- I am the error message infix operator, used later:
import Text.Parsec ((<?>))

-- Imported so we can play with applicative things later.
-- not qualified as mostly infix operators we'll be using.
import Control.Applicative

-- Get the Identity monad from here:
import Control.Monad.Identity (Identity)

-- alias Parsec.parse for more concise usage in my examples:
parse rule = Parsec.parse rule "(source)"


-- myParser :: Parsec.Parsec String () (String,String)
readNum :: Parsec.Parsec String () Int
readNum = read <$> Parsec.many1 Parsec.digit

myParser :: Parsec.Parsec String () ((Int, Int), (Int, Int))
myParser = do
    e1Start <- readNum
    Parsec.char '-'
    e1End <- readNum
    Parsec.char ','
    e2Start <- readNum
    Parsec.char '-'
    e2End <- readNum

    return ((e1Start, e1End), (e2Start, e2End))

fromRight (Right x) = x
fromRight _ = undefined

processLines1 :: [String] -> Int -> Int
processLines1 [] score = score
processLines1 (l:ls) score =
    if l == "" then
        processLines1 ls score
    else
        let ((e1Start, e1End), (e2Start, e2End)) = fromRight $ parse myParser l in
        let contained = (e1Start >= e2Start && e1End <= e2End) || (e2Start >= e1Start && e2End <= e1End) in
        processLines1 ls (score + if contained then 1 else 0)

processLines2 :: [String] -> Int -> Int
processLines2 [] score = score
processLines2 (l:ls) score =
    if l == "" then
        processLines2 ls score
    else
        let ((e1Start, e1End), (e2Start, e2End)) = fromRight $ parse myParser l in
        let overlap = (e1Start >= e2Start && e1Start <= e2End) || (e2Start >= e1Start && e2Start <= e1End) in
        processLines2 ls (score + if overlap then 1 else 0)

main = do
    contents <- readFile "input.txt"
    let ls = lines contents
    let score1 = processLines1 ls 0
    printf "1: %d\n" score1
    let score2 = processLines2 ls 0
    printf "2: %d\n" score2