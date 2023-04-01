main = interact respondPalindrome

respondPalindrome :: String -> String
respondPalindrome = unlines . map (\x -> if isPal x then "Pal" else "No pal") . lines
    where isPal xs = xs == reverse xs
