--Comment is like this
--input one int and return double of it
doubleMe x = x + x
--Function does not need braces: priotity is higher then +-*/
doubleUs x y = doubleMe x + doubleMe y

--if else: use then, else is necessary
doubleSmallNum x = if x > 100
    then x
    else doubleMe x

--We can use ' in variable and function name.
doubleSmallNum' x = (if x > 100 then x else doubleMe x) + 1


--multi line comment
{-
list
    *Main> let nums = [1, 2, 3, 4, 5]
    *Main> nums
    [1,2,3,4,5]
combine
    *Main> [1, 2, 3, 4] ++ [5, 6, 7, 8]
    [1,2,3,4,5,6,7,8]
string is list
    *Main> "Hello" ++ " " ++ "World"
    "Hello World"
add element in head
    *Main> 0:[1,2,3,4]
    [0,1,2,3,4]
access element by index
    *Main> nums !! 4
    5
    *Main> nums !! 3
    4
compare lists
    *Main let num0 = [1, 2, 3, 4, 5, 6, 7, 8]
    *Main> let num1 = [2, 3, 4, 5, 6, 7, 8, 9]
    *Main> num0 > num1
    False
    *Main> num0 < num1
    True
other functions
    Prelude> let nums = [1, 2, 3, 4, 5]
    Prelude> nums
    [1,2,3,4,5]
    Prelude> head nums
    1
    Prelude> tail nums
    [2,3,4,5]
    Prelude> last nums
    5
    Prelude> init nums
    [1,2,3,4]
    Prelude> length nums
    5
    Prelude> null nums
    False
    Prelude> reverse nums
    [5,4,3,2,1]
    Prelude> nums
    [1,2,3,4,5]
    Prelude> take 3 nums
    [1,2,3]
    Prelude> drop 3 nums
    [4,5]
    Prelude> maximum nums
    5
    Prelude> sum nums
    15
    Prelude> product nums
    120
list from range
    Prelude> [1..20]
    [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
can use arithmetic sequence
    Prelude> [2,4..20]
    [2,4,6,8,10,12,14,16,18,20]
    Prelude> [3,6..20]
    [3,6,9,12,15,18]
    Prelude> [20..1]
    []
    Prelude> [20,19..1]
    [20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
infinite list
    Prelude> take 24 [13,26..]
    [13,26,39,52,65,78,91,104,117,130,143,156,169,182,195,208,221,234,247,260,273,286,299,312]
float may not be accurate
    Prelude> [0.3,0.4..1]
    [0.3,0.4,0.5,0.6,0.7,0.7999999999999999,0.8999999999999999,0.9999999999999999]
list comprehesion: just like set-builder notation
    Prelude> [x * x | x <- [1..100]]
    [1,4,9,16,25,36,49,64,81,100,121,144,169,196,225,256,289,324,361,400,441,484,529,576,625,676,729,784,841,900,961,1024,1089,1156,1225,1296,1369,1444,1521,1600,1681,1764,1849,1936,2025,2116,2209,2304,2401,2500,2601,2704,2809,2916,3025,3136,3249,3364,3481,3600,3721,3844,3969,4096,4225,4356,4489,4624,4761,4900,5041,5184,5329,5476,5625,5776,5929,6084,6241,6400,6561,6724,6889,7056,7225,7396,7569,7744,7921,8100,8281,8464,8649,8836,9025,9216,9409,9604,9801,10000]
    Prelude> [x * x | x <- [1..20], x * 2 > 20]
    [121,144,169,196,225,256,289,324,361,400]
    Prelude> boombang xs = [if x < 10 then "Boom" else "Bang" | x <- xs, odd x]
    Prelude> boombang [7..13]
    ["Boom","Boom","Bang","Bang"]
    Prelude> [x | x <- [10..20], x /= 13, x /= 15, x /= 17]
    [10,11,12,14,16,18,19,20]
    Prelude> [x * y | x <- [1, 3, 5, 7], y <- [10, 30, 50, 70]]
    [10,30,50,70,30,90,150,210,50,150,250,350,70,210,350,490]
    Prelude> removeLower st = [c | c <- st, elem c ['a'..'z']]
    Prelude> removeLower "Let's Remove Lower Cases"
    "etsemoveowerases"
-}
--tuple
{-
    Prelude> (1,3)
    (1,3)
can use anything    
    Prelude> (2, 'a', [1, 2, 3, 5])
    (2,'a',[1,2,3,5])
you can make list of tuples of same length
    Prelude> [(1, 2), (3,4), (5,6)]
    [(1,2),(3,4),(5,6)]
this cause error: tuple of different length are treated as different type
    Prelude> [(1,2),(3,4,5),(6,7)]
    Prelude> let triples = [(a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10]]
    Prelude> let rightTriples = [(a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10], a^2 + b^2 == c^2]
    Prelude> rightTriples
    [(4,3,5),(3,4,5),(8,6,10),(6,8,10)]
-}