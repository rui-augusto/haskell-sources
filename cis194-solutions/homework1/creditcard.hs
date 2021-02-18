-- This is the first part of the CIS 194: Homework I
-- Founded in https://www.cis.upenn.edu/~cis194/spring15/hw/01-intro.pdf
-- Let me know if you found anything wrong or that could be better


-- converts an integer to a list of their digits
-- it has to be positive numbers, otherwise it will return a empty list 
toDigits :: Integer -> [Integer]
toDigits n
        | n > 0     = toDigits (n `div` 10) ++ [n `mod` 10]
        | otherwise = [] 


-- converts an integer to a reversed list
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits


-- double every second number of the list
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . interpersedDouble . reverse
        where
            interpersedDouble [] = []
            interpersedDouble [x] = [x]
            interpersedDouble (x : y : zs) = (2 * y) : doubleEveryOther zs

-- sum all the elements of the list
-- if the element has two digits, it will be divided in two different numbers
-- [6,12,5,16] == 6 + 1 + 2 + 5 + 1 + 6 = 21   
sumDigits :: [Integer] -> Integer
sumDigits = sum . correctList
        where
            correctList [] = []
            correctList (x : xs)
                | x >= 10 = x `div` 10 : x `mod` 10 : correctList xs
                | otherwise = x : correctList xs

-- validate the credit card number
-- if the sum of all elements of the list divided by 10 remainds 0, then it is a valid number 
luhn :: Integer -> Bool
luhn n = (sumDigits . doubleEveryOther . toDigits $ n) `mod` 10 == 0  

