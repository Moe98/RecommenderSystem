data User = U String deriving (Eq,Show)
data Item = I String deriving (Eq,Show)
data (Fractional a) => Rating a = NoRating | R a deriving (Eq,Show)

dis :: Eq a => [a] -> [a]
dis [] = []
dis (x:xs) | elem x xs = dis xs
	   | otherwise = x: dis xs

fromRatingsToItems :: Eq b => [(a,b,c)] -> [b]
fromRatingsToItems l = dis (helpFRTI [] l)

helpFRTI as [] = as
helpFRTI as ((_,x,_):xs) = helpFRTI (as ++ [x]) xs

fromRatingsToUsers :: Eq a => [(a,b,c)] -> [a]
fromRatingsToUsers l = dis (helpFRTU [] l)

helpFRTU as [] = as
helpFRTU as ((x,_,_):xs) = helpFRTU (as ++ [x]) xs

hasRating :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> Bool
hasRating _ _ [] = False
hasRating x y ((a,b,_):xs) | x==a && y==b = True
                           | otherwise = hasRating x y xs

getRating :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> c
getRating _ _ [] = error "No given rating"
getRating x y ((a,b,c):xs) | x==a && y==b = c
			   | otherwise = getRating x y xs

formMatrixUser :: (Eq a, Eq b, Fractional c) => b -> [a] -> [(b,a,c)] -> [Rating c]
formMatrixUser _ [] _ = []
formMatrixUser x (y:ys) xs = (helpFMU x y xs) : (formMatrixUser x ys xs)

helpFMU _ _ [] = NoRating
helpFMU x y ((a,b,c):xs) | x==a && y==b = (R c)
			 | otherwise = helpFMU x y xs

formMatrix :: (Eq a, Eq b, Fractional c) => [b] -> [a] -> [(b,a,c)] -> [[Rating c]]
formMatrix [] _ _ = []
formMatrix (x:xs) y z = (formMatrixUser x y z) : (formMatrix xs y z)

numberRatingsGivenItem :: (Fractional a, Num b) => Int -> [[Rating a]] -> b
numberRatingsGivenItem _ [] = 0
numberRatingsGivenItem y (x:xs) | y>=length x = numberRatingsGivenItem y xs
				| x!!y==NoRating = numberRatingsGivenItem y xs
				| otherwise = 1+ numberRatingsGivenItem y xs

differeneRatings :: Fractional a => Rating a -> Rating a -> a
differeneRatings NoRating _ = 0.0
differeneRatings _ NoRating = 0.0
differeneRatings (R x) (R y) = x-y

-- matrixPairs :: Num a => a -> [(a,a)]
matrixPairs 0 = []
matrixPairs x = helpMP 0 0 (x-1)

helpMP x y z | x==y && y==z = [(x,y)]
	     | x<=z && y<z = (x,y):helpMP x (y+1) z
	     | x<z && y==z = (x,y):helpMP (x+1) 0 z

fs [] _ =[]
fs (x:xs) l = (fsHelper x l) ++ (fs xs l)
fsHelper _ [] =[]
fsHelper x (y:ys) = if( (x /=NoRating ) && (y /=NoRating))
			then [1] ++ (fsHelper x ys)
			else [0] ++ (fsHelper x ys)
freqMatrix :: (Num a, Fractional b) => [[Rating b]] -> [a]
freqMatrix [] = []
freqMatrix (x:xs) = freqMatrixHelper (fs x x) xs
freqMatrixHelper l [] = l
freqMatrixHelper l (x:xs) =freqMatrixHelper (addTwoLists l (fs x x)) xs

ds [] _ =[]
ds (x:xs) l = dsHelper  x l ++(ds xs l)
dsHelper _ [] = []
dsHelper y (x:xs) = [(differeneRatings y x)] ++ dsHelper y xs
addTwoLists [] []=[]
addTwoLists (x:xs) (y:ys) =(x+y):(addTwoLists xs ys)
dMatrix :: Fractional a => [[Rating a]] -> [a]
dMatrix (x:xs)=dMatrixHelper (ds x x) xs
dMatrixHelper l [] = l
dMatrixHelper l (x:xs) = dMatrixHelper (addTwoLists l (ds x x)) xs



diffFreqMatrix :: Fractional a => [[Rating a]] -> [a]
diffFreqMatrix l = diffFreqMatrixHelper (dMatrix l) (freqMatrix l)

diffFreqMatrixHelper [] [] = []
diffFreqMatrixHelper (x:xs) (y:ys) = (x/y) : (diffFreqMatrixHelper xs ys)

predict :: (Eq a, Eq b, Fractional c) => [(a,b,c)] -> Int -> Int -> c
predict [] _ _ = 0.0
predict l u i = if hasRating ((fromRatingsToUsers l)!!u) ((fromRatingsToItems l)!!i) l
		then getRating ((fromRatingsToUsers l)!!u) ((fromRatingsToItems l)!!i) l
		else f /fromIntegral((length m))
		  where f = foldr (+) 0.0 z
			m = formMatrix (fromRatingsToUsers l) (fromRatingsToItems l) l
			z = zipWith (+) (map (\x -> differeneRatings x (R 0.0)) (m!!u)) y
			y = take ((i+1)*(length (m!!0))) (drop (i*(length (m!!0))-1) d)
			d = diffFreqMatrix ((take u m)++(drop (u+1) m))

divide :: Fractional a => a -> a -> a
divide x y = x/y