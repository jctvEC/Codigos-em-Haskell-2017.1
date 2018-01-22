import Data.Char -- nao precisa

-- Q1
ordenaPeso :: (a -> Int) -> [a] -> [a]
ordenaPeso f l = quicksort' f l

quicksort' :: (a -> Int) -> [a] -> [a]
quicksort' f [ ] = [ ]
quicksort' f (a:as) = quicksort' f [x | x<-as, (f x)<(f a)] ++ [a] ++ quicksort' f [x | x<-as, (f x)>=(f a)]

-- Q2
and' l = foldr (&&) True l
or' l = foldr (||) False l
concat' l = foldr (++) [] l
sum' l = foldr (+) 0 l

-- Q3
classificar :: String -> String
classificar a = map (toUpper) (filter (\x -> x>='a' && x<='z') a) ++ (filter (\x -> x>='A' && x<='Z') a) ++ filter (<='9') a
--classificar a = map (\x -> toEnum(fromEnum x - 32)::Char) (filter (\x -> x>='a' && x<='z') a) ++ (filter (\x -> x>='A' && x<='Z') a) ++ filter (<='9') a

-- Q4
{-
a) tail.head 
b) tail.head.head 

Dados: 
( . ) :: (b -> c) -> (a -> b) -> a -> c 
head :: [a] -> a
tail :: [a] -> [a]
-}

{-
a)
tail.head

Analisando os tipos das fun��es:
tail  :: [x] -> [x]
( . ) :: (b -> c) -> (a -> b) -> a -> c 
head  :: [y] -> y

Da aplica��o do primeiro argumento da fun��o ( . ), temos que: 
(b -> c) = [x] -> [x]
b = [x]
c = [x]

Da aplica��o do segundo argumento da fun��o ( . ), temos que: 
(a -> b) = [y] -> y
a = [y]
b = y

Como b = [x] e b = y, ent�o:
y = [x]

Logo, o retorno da composi��o de fun��es �:
a -> c
[y] -> [x]
[[x]] -> [x]

Tipo final: [[x]] -> [x]



b)
tail.head.head = tail.(head.head) = (tail.head).head

Como foi determinado anteriormente:
tail.head :: [[x]] -> [x]

Ent�o, analisando os tipos das fun��es:
tail.head :: [[x]] -> [x]
( . )     :: (b -> c) -> (a -> b) -> a -> c
head      :: [y] -> y

Da aplica��o do primeiro argumento da fun��o ( . ), temos que: 
(b -> c) = [[x]] -> [x]
b = [[x]]
c = [x]

Da aplica��o do segundo argumento da fun��o ( . ), temos que: 
(a -> b) = [y] -> y
a = [y]
b = y

Como b = [[x]] e b = y, ent�o:
y = [[x]]

Logo, o retorno da composi��o de fun��es �:
a -> c
[y] -> [x]
[[[x]]] -> [x]

Tipo final: [[[x]]] -> [x]
-}