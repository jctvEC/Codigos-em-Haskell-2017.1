--Resolução das Questões da 3ª Aula Prática de PLC
--Monitoria de Paradigmas de Linguagens Computacionais 2017.1
--Monitor: Gustavo Oliveira (gof@cin.ufpe.br)

-- Q2
type Ponto = (Float, Float, Float)

distancia :: Ponto -> Ponto -> Float
distancia (a,b,c) (e,f,g) = sqrt ((e-a)^2 + (f-b)^2 + (g-c)^2)

-- Q3
type Nome = String
type Tamanho = Int
data Diretorio = Pasta Nome [Diretorio] | Arquivo Nome Tamanho

-- Forma modular de fazer
size :: Diretorio -> (Tamanho, Int)
size (Arquivo a b) = (b, 1)
size (Pasta a b)   = (size' b)

size' :: [Diretorio] -> (Tamanho, Int)
size' []  = (0, 0)
size' ((Arquivo a b):z) = somaTupla (b, 1) (size' z) 
size' ((Pasta a b):z)   = somaTupla (size' b) (size' z)

somaTupla :: (Int, Int) -> (Int, Int) -> (Int, Int)
somaTupla (a, b) (c, d) = (a+c, b+d)

-- Forma semi modular
sizeAlternativo :: Diretorio -> (Tamanho, Int)
sizeAlternativo (Arquivo a b) = (b, 1)
sizeAlternativo (Pasta a b)   = (size'' b)

size'' :: [Diretorio] -> (Tamanho, Int)
size'' []  = (0, 0)
size'' ((Arquivo a b):z)   = (b+c1, 1+c2)  
							 where (c1,c2) = (size'' z)									
size'' ((Pasta a b):z)     = (y+y1, w+w1)
                             where (y,w)=(size'' b);
                                   (y1,w1)= (size'' z);

-- Forma reduzida								   
size''' :: Diretorio -> (Tamanho, Int)
size''' (Arquivo _ t) = (t, 1)
size''' (Pasta _ cd) = foldr sumt (0, 0) (map size''' cd)
	where 
		sumt (a, b) (c, d) = (a+c, b+d)

-- Q4
data Complexo = Imag Float Float
instance Show Complexo where
 show (Imag a b) | b < 0 = (show a)++" "++"-j"++(show (-b))
                 | otherwise = (show a)++" "++"+"++"j"++(show b)