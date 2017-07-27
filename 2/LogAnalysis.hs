module LogAnalysis where

import Log

data Tree = L
            | N Tree Integer Tree
  deriving (Show, Eq)


main :: IO()
main = putStrLn $ show (parseMessage "E 2 562 help help")

parseMessage :: String -> LogMessage
parseMessage x =
    let w = (words x)
        second = read(w!!1) in
    case head(w) of
        "E" -> LogMessage (Error second) (read(w!!2)) x
        "I" -> LogMessage Info second x
        "W" -> LogMessage Warning second x
        _ -> Unknown x

parse :: String -> [LogMessage]
parse x = parseLines $ lines x

parseLines :: [String] -> [LogMessage]
parseLines [] = []
parseLines (x:xs) = [parseMessage x] ++ parseLines xs

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert (LogMessage m t s) Leaf = Node Leaf (LogMessage m t s) Leaf
insert input@(LogMessage _ t1 _) (Node left (LogMessage m2 t2 s2) right)
    | t1 < t2 = Node (insert input left) (LogMessage m2 t2 s2) right
    | otherwise = Node left (LogMessage m2 t2 s2) (insert input right)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

insertTree :: Integer -> Tree -> Tree
insertTree x L = N L x L
insertTree x1 (N left x2 right)
    | x1 < x2 = N (insertTree x1 left) x2 right
    | otherwise = N left x2 (insertTree x1 right)

buildTree :: [Integer] -> Tree
buildTree [] = L
buildTree (x:xs) = insertTree x (buildTree xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) = (inOrder left) ++ [m] ++ (inOrder right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = extractMessage . inOrder . build $ filterMessages xs
    where filterMessages = filter isGreatError

extractMessage :: [LogMessage] -> [String]
extractMessage [] = []
extractMessage ((Unknown s) :xs) = [s] ++ extractMessage xs
extractMessage ((LogMessage _ _ s) :xs) = [s] ++ extractMessage xs

isGreatError :: LogMessage -> Bool
isGreatError (Unknown _) = False
isGreatError (LogMessage (Error x ) _ _) = x >= 50
isGreatError (LogMessage _ _ _) = False
