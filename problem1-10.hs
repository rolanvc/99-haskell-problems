import Data.List
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
myReverse (a:[]) = [a]
myReverse (x:xs) = myReverse xs ++ [x] 

--check if palindrome
isPalindrome ::(Eq a) => [a] -> Bool
isPalindrome x = x == myReverse x

--flatten a nested list structure
data NestedList a= Elem a | List [NestedList a]
myflatten:: NestedList a ->[a]
myflatten (Elem x) = [x]
myflatten( List []) = []
myflatten (List (x:xs)) = myflatten x ++ myflatten (List xs)
 
--compress consecutive repeating elements into one element
lastelem :: [a] ->a
lastelem (a:[]) = a
lastelem (x:xs) = lastelem xs
compresscore ::(Eq a) => [a] ->[a] -> [a]
compresscore a [] = a
compresscore a (x:xs) = if lastelem a == x then compresscore a xs else compresscore (a++[x]) xs

compress ::(Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = compresscore [x] xs

packCore ::(Eq a)=>  [a] -> [a] -> [a]
packCore l (h:t) = let lastelem = myLast l 
		   in	if lastelem== h 
			then l ++[h]
			else l 

--pack consecutive duplicates of list items into sublists.
pack:: (Eq a) =>  [a] -> [[a]]
pack xs = foldl (\acc x -> if not(null acc) && [x] `Data.List.isPrefixOf` (last acc)
	then (init acc) ++ [(last acc) ++ [x]]
	else acc ++ [[x]])
	[] xs

--run-length encode a list. consecutive duplicates of elements are encoded as (N E) where N is the number of duplicates of element E.
encode:: (Eq a) => [a] -> [(Int, a)]
encodeList:: [a] -> (Int,a)
encodeList x=(myLength x, head x)
encode x = map encodeList $ pack x
