import Data.Map.Strict qualified as Map

data Node = File Int | Dir (Map.Map String Node) deriving (Show)

parseDirLines :: [String] -> Node -> ([String], Node)
parseDirLines (l : ls) curDir@(Dir m) =
  case words l of
    ["$", "ls"] -> parseDirLines ls curDir
    ["dir", _] -> parseDirLines ls curDir
    ["$", "cd", ".."] -> (ls, curDir)
    ["$", "cd", path] ->
      let (rem, newDir) = parseDirLines ls (Dir Map.empty)
       in parseDirLines rem (Dir $ Map.insert path newDir m)
    [sz, name] -> parseDirLines ls (Dir $ Map.insert name (File $ read sz) m)
parseDirLines [] d = ([], d)

nodeSize :: Node -> Int
nodeSize (File sz) = sz
nodeSize (Dir m) = sum $ map nodeSize $ Map.elems m

findDirs :: (Int -> Bool) -> Node -> [Int]
findDirs pred (File sz) = []
findDirs pred n@(Dir m) =
  let rest = concatMap (findDirs pred) $ Map.elems m
      subSize = nodeSize n
   in if pred subSize then subSize : rest else rest

main = do
  contents <- readFile "input.txt"
  let (_, tree) = parseDirLines (lines contents) (Dir Map.empty)
  let sz1 = sum $ findDirs (<= 100000) tree
  print sz1

  let totalSize = nodeSize tree
  let free = 70000000 - totalSize
  let needed = 30000000 - free
  let sz2 = minimum $ findDirs (>= needed) tree
  print sz2