--Author: A.D. Gonzalez
--Updated: 12/2016

--Exercise 1
toDigits :: Integer -> [Integer]
--maps a lambda function (sorry if that is strictly python related language
--that reads (or casts an input string to a different type) in a string and converts
--to an integer TO each character in the string version of whatever number was input.
toDigits x = if x <= 0
    then []
    else map (\x -> read[x]::Integer) (show x)


toDigitsRev:: Integer -> [Integer]
--same as toDigits, but reverses the output.
toDigitsRev x = reverse (toDigits x)


--Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
--Takes in a list of integers and doubles every other digit starting from the next to last
--uses cycle to loop between two functions and zips them to reversed integer list.  Reverses again.
doubleEveryOther xs = reverse (zipWith ($) (cycle [id, (*2)]) (reverse xs))


--Exercise 3
sumDigits :: [Integer] -> Integer
--sumDigits [16,7,12,5] = 1 + 6 + 7 + 1+ 2 + 5 = 22
sumDigits xs = sum (map sum (map toDigits xs))


--Exercise 4
validate :: Integer -> Bool
--Divide integer by 10.  If 0 remainder then True, else False
--I used Guards here instead of If/Else.
validate x
    | x == 0 = False
    | x `mod` 10 == 0 = True
    | x `mod` 10 /= 0 = False


--Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
--Haanoi should return list of moves to be performed to move from first peg to second
--hanoi 2 from_peg to_peg spare_peg
hanoi 0 a b c = []
hanoi x a b c = hanoi (x-1) a c b ++ [(a,b)] ++ hanoi (x-1) c b a


--Exercise 6 (Optional)
--TODO
