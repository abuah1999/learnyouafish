import System.Environment
import System.IO
import System.IO.Error
import Control.Exception

main = toTry `catch` handler

toTry :: IO ()
toTry = do (fileName:_) <- getArgs
           contents <- readFile fileName
           putStrLn $ (show . length . lines) contents

handler :: IOError -> IO ()
handler e 
    | isDoesNotExistError e = case ioeGetFileName e of
                                  Just path -> putStrLn $ "File not found at " ++ path
                                  Nothing -> putStrLn "File not found."
    | otherwise = ioError e
