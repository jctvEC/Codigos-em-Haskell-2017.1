import Data.List--usar o group

quicksort :: [Int] -> [Int]
quicksort [ ] = [ ] 
quicksort (x:xs) = quicksort [y | y <- xs, y < x] ++ [x] ++ quicksort [y | y <- xs, y >= x]

reduz1 :: [Int] -> [Int]--QUESTÃO 1
reduz1 [ ] = [ ]
reduz1 [xs] = [ ]
reduz1 (x:xs) = concat [tail y | y <- group (quicksort (x:xs))]

reduzfacil :: [Int] -> [Int]--'NÃO FAZ PARTE DA QUESTÃO' alteraçao no quicksort que já reduz 1
reduzfacil [ ] = [ ] 
reduzfacil (x:xs) = reduzfacil [y | y <- xs, y < x] ++ [y | y <- xs, y == x] ++ reduzfacil [y | y <- xs, y > x]