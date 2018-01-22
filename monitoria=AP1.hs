--Resolução das Questões da 1ª Aula Prática de PLC: Tipos Básicos e Estruturados em Haskell
--Monitoria de Paradigmas de Linguagens Computacionais 2017.1
--Monitor: Gustavo Oliveira (gof@cin.ufpe.br)
--Disponível em: 23/03/2017



{-
R1. Crie uma função: primo :: Int -> Bool, que dado um inteiro positivo, informe se ele é, ou não, um número primo.

Exemplos:
Main> primo 1
False

Main> primo 251
True

Main> primo 621
False
-}

primo :: Int -> Bool
primo 0 = False
primo 1 = False
primo n = primo' n 2
    where
        primo' n t | (n==t) = True
                   | otherwise = (mod n t)/=0 && (primo' n (t+1))



{-
R2. Implemente a função: divisaoEuclidiana :: Int -> Int -> (Int, Int), que calcula o quociente e o resto da divisão 
de dois números inteiros utilizando subtrações sucessivas. Na entrada, o primeiro número passado será o dividendo e
o segundo, o divisor. Na saída, o primeiro número da tupla será o quociente e o segundo, o resto.

Exemplos:
Main> divisaoEuclidiana 24 2
(12, 0)

Main> divisaoEuclidiana 47 6
(7, 5)
-}

divisaoEuclidiana :: Int -> Int -> (Int, Int)
divisaoEuclidiana n d | n < d = (0, n)
                      | otherwise = somaTuplas (1, 0) (divisaoEuclidiana (n-d) (d))
					  
somaTuplas :: (Int, Int) -> (Int, Int) -> (Int, Int)
somaTuplas (x1, y1) (x2, y2) = (x1+x2, y1+y2)



{-	
R3. Um palíndromo é uma sequência de unidades que pode ser lida tanto da direita para a esquerda 
como da esquerda para a direita. Crie uma função: palindromo :: String -> Bool, que dada uma 
cadeia de caracteres de entrada, retorne se ela é um palíndromo ou não.

Exemplos:
Main> palindromo “12321”
True

Main> palindromo “subinoonibus”
True
-}

palindromo :: String -> Bool
palindromo s = (s == reverse s)



{-
R4. Crie uma função: sort :: [Int] -> [Int], que ordene uma lista de inteiros desordenada.

Exemplo:
Main> sort [9, 8, 9, 7, 11, 0, 2]
[0, 2, 7, 8, 9, 9, 11]
-}

sort = quicksort

quicksort :: [Int] -> [Int]
quicksort [ ] = [ ] 
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

{-
R5. Implemente a função doisAdois :: Int -> [(Int, Int)] que receba um número
inteiro n e retorne uma lista de tuplas com todas as combinações
dois a dois (Considere que (x,y) = (y,x)) do cojunto dos inteiros
de 0 à n.
Exemplos:
Main> doisAdois 3
[(0, 0), (0, 1), (0, 2), (0, 3), (1, 1), (1, 2), (1, 3), (2, 2), (2, 3), (3, 3)]
Main> doisAdois 0
[(0,0)]
-}

doisAdois :: Int -> [(Int, Int)]
doisAdois x = [(a,b) | a <- [0..x], b <- [0..x], a < b || a == b]