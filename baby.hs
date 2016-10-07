doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber' x = succ (if x > 100 then x else x*2)
lostNumbers = [[1],[1,2],[1,2,3],[1],[1,2,32],[1,2,3]]
lostNumbers2 = [9,6,5,4,8,2,8]
--tamano (x:xs)= length(x:xs)
--tamano::[[Integer]]->[Integer]
--tamano [[]]=0
--tamano (x:xs) = length(xs):tamano x
--tamano [] = []
--tamano (x:xs) = [length x]++(tamano xs)
tamano [] = []
tamano (x:xs) = if (length x)>=2 then [x]++tamano xs else tamano xs