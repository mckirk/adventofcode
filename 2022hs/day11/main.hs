{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Lens
import Data.List
import Data.Void
import GHC.Plugins (last2)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

data Op = Add Integer | Mul Integer | Sq
  deriving (Show)

doOp :: Integer -> Op -> Integer
doOp n (Add i) = n + i
doOp n (Mul i) = n * i
doOp n Sq = n * n

data Monkey = Monkey
  { items :: [Integer],
    op :: Op,
    testNum :: Integer,
    trueMonkey :: Int,
    falseMonkey :: Int,
    inspectedItems :: Int
  }
  deriving (Show)

itemsLens :: ([Integer] -> Identity [Integer]) -> Monkey -> Identity Monkey
itemsLens = lens items (\x y -> x {items = y})

parseOp :: Parser Op
parseOp = try parseAdd <|> try parseMul <|> parseSq
  where
    parseAdd = do
      string "new = old + "
      num <- read <$> some digitChar
      return $ Add num
    parseMul = do
      string "new = old * "
      num <- read <$> some digitChar
      return $ Mul num
    parseSq = do
      string "new = old * old"
      return Sq

parseMonkey :: Parser Monkey
parseMonkey = do
  string "Monkey "
  alphaNumChar
  printChar
  parseLine "Starting items:"
  items <- map read <$> (space *> some digitChar) `sepBy1` char ','
  parseLine "Operation: "
  op <- parseOp
  parseLine "Test: divisible by "
  testNum <- parseNum
  parseLine "If true: throw to monkey "
  trueMonkey <- parseNum
  parseLine "If false: throw to monkey "
  falseMonkey <- parseNum
  some spaceChar

  let inspectedItems = 0

  return $ Monkey {..}
  where
    parseNum :: Read a => Parser a
    parseNum = read <$> some digitChar
    parseLine s = some spaceChar *> string s

parseMonkeys :: Parser [Monkey]
parseMonkeys = some parseMonkey

monkeyDo :: (Integer -> Integer) -> Monkey -> (Monkey, [(Int, Integer)])
monkeyDo levelMod m@(Monkey {..}) = (m {items = [], inspectedItems = inspectedItems + length items}, itemAssignment)
  where
    itemAssignment =
      map
        ( \level ->
            let newLevel = levelMod $ doOp level op
                newMonkey = if newLevel `mod` testNum == 0 then trueMonkey else falseMonkey
             in (newMonkey, newLevel)
        )
        items

playRound :: (Integer -> Integer) -> [Monkey] -> [Monkey]
playRound levelMod initMonkeys = foldl processMonkey initMonkeys [0 .. length initMonkeys - 1]
  where
    processMonkey monkeys i =
      let monkey = monkeys !! i
          (newMonkey, assignment) = monkeyDo levelMod monkey
          newMonkeys1 = monkeys & ix i .~ newMonkey
          newMonkeys2 = foldl processAssignment newMonkeys1 assignment
       in newMonkeys2
    processAssignment monkeys (monkeyIdx, level) =
      monkeys & (ix monkeyIdx . itemsLens %~ (level :))

playRounds :: (Integer -> Integer) -> Integer -> [Monkey] -> [Monkey]
playRounds levelMod n initMonkeys = foldl (\monkeys _ -> playRound levelMod monkeys) initMonkeys [1 .. n]

main = do
  contents <- readFile "input.txt"
  monkeys <-
    ( case parse parseMonkeys "input.txt" contents of
        Left _ -> return []
        Right monkeys -> return monkeys
      )
  let modulo = product $ map testNum monkeys

  play (`div` 3) 20 monkeys
  play (`mod` modulo) 10000 monkeys
  where
    play levelMod rounds monkeys = do
      let finalMonkeys = playRounds levelMod rounds monkeys
      let timesInspected = map inspectedItems finalMonkeys
      print $ uncurry (*) $ last2 $ sort timesInspected
