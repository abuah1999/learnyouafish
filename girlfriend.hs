import System.IO  
  
main = do  
    handle <- openFile "data/girlfriend.txt" ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle 
