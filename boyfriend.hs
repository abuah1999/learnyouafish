import System.IO

main = do 
    withFile "data/girlfriend.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)
