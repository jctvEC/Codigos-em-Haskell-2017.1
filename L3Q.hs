


data Complexo = Imaginario Float Float

instance Show Complexo where
    show (Imaginario a b) | b < 0 = (show a) ++ " - " ++ (show (-b)) ++ ".j"
						  | otherwise = (show a) ++ " + " ++ (show b) ++ ".j"

instance Eq Complexo where
	(==) (Imaginario a b) (Imaginario x y) = (x == a) && (y == b)
	(/=) (Imaginario a b) (Imaginario x y) = (x /= a) || (y /= b)
	
instance Num Complexo where
	(+) (Imaginario a b) (Imaginario x y) = (Imaginario (a+x) (b+y))
	(-) (Imaginario a b) (Imaginario x y) = (Imaginario (a-x) (b-y))
	(*) (Imaginario a b) (Imaginario x y) = (Imaginario ((a*x)-(b*y)) ((a*y)+(b*x)))
	negate (Imaginario a b) = Imaginario a (-b)
	abs (Imaginario a b) = (Imaginario (sqrt((a*a)+(b*b))) 0)--módulo = raiz da soma dos quadrados
	signum (Imaginario a b) = (Imaginario (acos(a/(sqrt((a*a)+(b*b))))) (asin(b/(sqrt((a*a)+(b*b))))))--ANGULO DO NUMERO EM RADIANOS, DO TIPO: Imaginario( acos(a/r)  asin(b/r) ) sempre o resultado é o mesmo
	fromInteger (n) = (Imaginario (fromInteger n) 0)-- convertendo para float apenas parte real

	
	
--Questão 2 imcompleta	
data LDisjointSet = LDS [[Int]] deriving (Eq, Show)
data TDisjointSet = Void |
					TDS [Int] TDisjointSet deriving (Eq, Show)

class OprDisjointSet t where
	makeSet :: Int -> t -> t
	union :: Int -> Int -> t -> t
	find :: Int -> t -> Maybe Int

instance OprDisjointSet LDisjointSet where
	find x (a:xs) | (elem x (a:xs)) == True = Just a
                  | otherwise = Nothing
	union a b [(x:xs):(y:ys)] | (elem a (x:xs)) && (elem b (y:ys)) = [(x:ys)] --Não consegui concluir a lógica da questão
	
--instance OprDisjointSet TDisjointSet where
