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