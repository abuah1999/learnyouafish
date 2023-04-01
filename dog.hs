import System.IO

main = do
    contents <- readFile "data/girlfriend.txt"
    putStr contents
