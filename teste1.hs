{-type Nome = String
type Tamanho = Int

data Diretorio = Arquivo Nome Tamanho | Pasta Nome [Diretorio]


size :: Diretorio -> (Tamanho, Int)
size (Arquivo x y) = (y, 1)
size (Pasta x y) =  (size' y)

size' :: [Diretorio] -> (Tamanho, Int)
size' []  = (0, 0)
size' ((Arquivo a b):z) = somaTupla (b, 1) (size' z) 
size' ((Pasta a b):z)   = somaTupla (size' b) (size' z)

somaTupla :: (Int, Int) -> (Int, Int) -> (Int, Int)
somaTupla (a, b) (c, d) = (a+c, b+d)

instance (Show (a -> a)) where
 show (a -> a) = show a ++ " com " ++ show a
-}

data Complexo = Imag Float Float

instance Show Complexo where
 show (Imag a b) | b < 0 = (show a) ++ " " ++ "-j" ++ (show (-b))
                 | otherwise = (show a) ++ " " ++ "+" ++ (show b) -- ++ ".j"

instance Eq Complexo where
 (==) (Imag a b) (Imag x y) = (x == a) && (y == b)
 (/=) (Imag a b) (Imag x y) = (x /= a) | (y /= b)

instance Num Complexo where
--

data Tree t = Leaf Int | Node Int (Tree t) (Tree t)
 deriving Show

isSortedTree :: Tree t -> Bool --Ord t => 
isSortedTree Tree f
 | Leaf f = True
 | (Node (f) (Tree e) (Tree d) && f>e && f<d) = isSortedTree (Tree e) && isSortedTree (Tree d)
 | otherwise = False