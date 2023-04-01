import Control.Monad  
import Data.Char  
  
main = forever $ do  
    putStr "Give me some input: "  
    contents <- getContents  
    putStrLn $ map toUpper contents  
