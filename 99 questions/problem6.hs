
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome xs =
        if head xs == last xs
            then isPalindrome $ tail $ init xs
            else False
