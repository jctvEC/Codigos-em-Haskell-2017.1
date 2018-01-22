func = [ soma , succ ]
soma :: Int -> Int
soma x = x + 2

{-teste :: [(Int->Int)] -> Int -> [Int]
teste [] y = []
teste (x:xs) y = (x y) : teste xs y -}

--1)
isSorted :: Ord t => [t] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:ys) = (x <= y) && isSorted (y:ys)

--2)
bSort :: Ord t => [t] -> [t]
bSort [] = []
bSort [x] = [x]
bSort x | isSorted x = x
        | otherwise = bSort (sort x)

sort :: Ord t => [t] -> [t]
sort [] = []
sort [x] = [x]
sort (x:y:ys) | x > y = y : (sort (x:ys))
              | otherwise = x : (sort (y:ys))

--3) Todos pegavam, mostrar execucao ou detalhar com palavras. Mostrar resultado
--4)Meu errado e tentativa de refazer

data Tree t = Node t (Tree t) (Tree t) | Leaf t deriving Show

{-
isSortedTree :: Ord t => Tree t -> Bool
isSortedTree (Leaf x) = True
isSortedTree (Node x y z) | y == (Leaf w) = (isSortedTree y) && (w <= x) && (isSortedTree z >= x)
                          | z == (Leaf w) = (isSortedTree y <=x) && (w >= x) && (isSortedTree z)
                          | (y == (Leaf w)) && (z == (Leaf p)) = (isSortedTree y) && (w <= x) && (p >= x) && (isSortedTree z)
                          | otherwise = (isSortedTree y <=x) && (isSortedTree z >= x)

isSortedTree (Node x y z) = (isSortedTree (teste y) <=x) && (isSortedTree (teste z) >= x) 

teste :: Tree t -> Tree t
teste (Leaf x) = (Node x (x-1) (x+1))
teste (Node x y z) = (Node x y z)
-}

--Gabarito 4)
{-
isSortedTree :: Ord t => Tree t -> Bool
isSortedTree (Leaf _) = True
isSortedTree (Node x y z) = ((filter (>x) (transToList y) ++ filter (<x) (transToList z)) == []) && isSortedTree y && isSortedTree z

transToList :: Tree t -> [t]
transToList (Leaf x) = [x]
transToList (Node x y z) = x:transToList y ++ transToList z
-}
--Forma alternativa e mais facil

isSortedTree :: Ord t => Tree t -> Bool
isSortedTree x = isSorted(transToList x)

transToList :: Tree t -> [t]
transToList (Leaf x) = [x]
transToList (Node x y z) = transToList y ++ [x] ++ transToList z


testeOrdenado :: Tree Int

testeOrdenado = Node 10 (Node 5 (Leaf 3) (Leaf 6)) (Node 15 (Leaf 14) (Leaf 17))

testeNaoOrdenado :: Tree Int

testeNaoOrdenado = Node 10 (Node 5 (Leaf 3) (Leaf 6)) (Node 15 (Leaf 16) (Leaf 17))