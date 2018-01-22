quicksort :: [Int] -> [Int]
quicksort [ ] = [ ] 
quicksort (x:xs) = quicksort [y | y <- xs, y < x] ++ [x] ++ quicksort [y | y <- xs, y >= x]

const1 :: Int-- constante para resoluçao
const1 = 1

qaux2 :: [Int] -> Int -> [(Int, Int)]-- funçao auxiliar para contador
qaux2 [ ] a = [ ]
qaux2 (x:[ ]) a = [(x,a)]
qaux2 (x:xs) a | x /= head xs = [(x,a)] ++ qaux2 xs const1
			   | otherwise = qaux2 xs (a+1)

tuplaQuant :: [Int] -> [(Int ,Int)]--QUESTÃO 2
tuplaQuant xs = qaux2  (quicksort xs) const1