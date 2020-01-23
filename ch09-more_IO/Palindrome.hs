--palindrome is string that is same 
--whether you read it left to right or right to left
respondPalindromes :: String -> String
respondPalindromes = unlines .
    map (\xs -> if isPal xs then "palindrome" else "not a palindrome") .
    lines

isPal :: String -> Bool
isPal xs = xs == reverse xs

{-
    *Main> respondPalindromes "palindrome"
    "not a palindrome\n"
    *Main> respondPalindromes "eye\nsomething\nsoos"
    "palindrome\nnot a palindrome\npalindrome\n"
-}

main = interact respondPalindromes
{-
    hehe
    not a palindrome
    palindrome
    not a palindrome
    peep
    palindrome
    cookies
    not a palindrome
-}


