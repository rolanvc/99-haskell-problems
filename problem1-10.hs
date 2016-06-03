--get last element
myLast::[a]->a
myLast []=error "Empty List!"
myLast [x] = x
myLast (_:xs) = myLast xs 

-- get 2nd to last element
myButLast::[a]->a
myButLast []=error "Empty List!"
myButLast (x:[y]) = x
myButLast (_:xs) = myButLast xs

-- find kth element of list
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) k = elementAt xs (k-1)


-- find number of elements of a list
-- myLength :: [a] -> Int
myLength [] = 0
myLength (x:[]) = 1
myLength (x:xs) = 1+ myLength xs

--reverse a list
--
myReverse :: [a] -> [a]

