import DataFile


uniqueBigrams :: [String] -> [(String,String)]
uniqueBigrams [] = []
uniqueBigrams [x] = [] 
uniqueBigrams (x:y:xs) | not (elem (x,y) (uniqueBigrams (y:xs))) = (x,y):uniqueBigrams (y:xs)
					   |otherwise = uniqueBigrams (y:xs)

uniqueTrigrams :: [String] -> [(String,String,String)]
uniqueTrigrams [] = []
uniqueTrigrams [x] = []
uniqueTrigrams [x,y] = []
uniqueTrigrams (x:y:z:xs) |not (elem (x,y,z) (uniqueTrigrams (y:z:xs))) = (x,y,z):uniqueTrigrams (y:z:xs)
						  |otherwise = uniqueTrigrams (y:z:xs)

notUniqueBigrams [] =[]
notUniqueBigrams [x] = []
notUniqueBigrams (x:y:xs) = (x,y):notUniqueBigrams (y:xs)

notUniqueTrigrams [] = []
notUniqueTrigrams [x] = []
notUniqueTrigrams [x,y] = []
notUniqueTrigrams (x:y:z:xs) = (x,y,z):notUniqueTrigrams(y:z:xs)

splitallhlpr [] = ([],[])
splitallhlpr x = splitAt (charcounthlpr x) x

charcounthlpr [] = 0
charcounthlpr (x:xs) = if (not (occursIn x punct)) then 1 + charcounthlpr xs
																	 else 0

expandallhlpr [] = []
expandallhlpr ((x,y):xs) = if y == "" then x:expandallhlpr xs
									  else x:y:expandallhlpr xs
									  

occursIn :: Eq a => a -> [a] -> Bool
occursIn x [] = False
occursIn a (x:xs) = if a == x then True
							  else occursIn a xs

wordToken:: String -> [String]
wordToken [] = []
wordToken l = expandallhlpr (map splitallhlpr (words l))


expandlists [] = []
expandlists (x:y:xs) = x ++ y ++ expandlists xs

wordTokenList :: [String] -> [String]
wordTokenList [] = []
wordTokenList l = expandlists (map (wordToken) l)


count _ [] = 0
count x (y:ys) | x==y = 1+count x ys
			   |otherwise=0+count x ys


getBigramCount z (x,y) = ((x,y),count (x,y) z)

getTrigramCount m (x,y,z) = ((x,y,z),count (x,y,z) m)


bigramsFreq :: Num a => [String] -> [((String,String),a)]
bigramsFreq x = map (getBigramCount (notUniqueBigrams x)) (uniqueBigrams x)

trigramsFreq:: Num a => [String] -> [((String,String,String),a)]
trigramsFreq x = map (getTrigramCount (notUniqueTrigrams x)) (uniqueTrigrams x)



getFreq :: (Eq a, Fractional b) => a -> [(a,b)] -> b
getFreq _ [] = 0
getFreq y ((x1,x2):xs) |y==x1 = x2
					   |otherwise = getFreq y xs



generateOneProb :: Fractional a => ((String,String,String),a) ->[((String,String),a)] -> a
generateOneProb ((x,y,z),i) l = i / (getFreq (x,y) l)


genProbPairs :: Fractional a => [((String,String,String),a)] ->[((String,String),a)] -> [((String,String,String),a)]
genProbPairs [] l = []
genProbPairs (((x,y,z),i):xs) l = ((x,y,z),(generateOneProb ((x,y,z),i) l)):(genProbPairs xs l)
















