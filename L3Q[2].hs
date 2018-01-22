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
	
instance OprDisjointSet TDisjointSet where
	