import Data.List
import Data.Maybe
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

data Packet = Num Int | List [Packet] deriving (Eq, Show)

type Pair = (Packet, Packet)

instance Ord Packet where
  compare :: Packet -> Packet -> Ordering
  compare (Num il) (Num ir) = il `compare` ir
  compare (List (l : ls)) (List (r : rs)) =
    case compare l r of
      EQ -> compare (List ls) (List rs)
      o -> o
  compare (List []) (List []) = EQ
  compare (List []) (List _) = LT
  compare (List _) (List []) = GT
  compare l@(Num _) r = compare (List [l]) r
  compare l r@(Num _) = compare l (List [r])

parsePacket :: Parser Packet
parsePacket = parseNum <|> parseList
  where
    parseNum = Num . read <$> some digitChar
    parseList = List <$> (char '[' *> (parsePacket `sepBy` char ',') <* char ']')

parsePacketPairs :: Parser [Pair]
parsePacketPairs = some parsePacketPair
  where
    parsePacketPair = do
      first <- parsePacket
      some spaceChar
      second <- parsePacket
      some spaceChar
      return (first, second)

main :: IO ()
main = do
  contents <- readFile "input.txt"

  packetPairs <-
    ( case parse parsePacketPairs "input.txt" contents of
        Left _ -> return []
        Right pairs -> return pairs
      )

  print $ sum $ map succ $ findIndices (uncurry (<)) packetPairs

  let keyPackets = [List [Num 2], List [Num 6]]
  let packets = sort $ keyPackets ++ concatMap (\(l, r) -> l : [r]) packetPairs

  print $ product $ map (succ . (fromJust . (`elemIndex` packets))) keyPackets
