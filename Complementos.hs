fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n | n > 1 = fib (n - 2) + fib (n - 1)

type Val = Int
data Arvore = Null | ArvNo Arvore Val Arvore
                     --deriving Show
myArv :: Arvore
myArv = (ArvNo (ArvNo Null 1 Null) 10 Null)

{- ,-}--VISITAS EM ORDEM
instance Show Arvore where 
 show Null = "_"
 show (ArvNo n1 a n2) = "{" ++ show (n1) ++ " " ++ show a ++ " " ++ show (n2) ++ "}"
{-, -}

--insert :: Arvore -> 


{- ,}
showArvNo :: Arvore -> String
showArvNo Null = " N "
showArvNo (ArvNo n1 a n2) = showArvNo (n1) ++ " Val: " ++ show a ++ showArvNo (n2)

f :: [Int] -> [Int -> Int]
f = map ( + )

{, -}

divEucli :: Int -> Int -> (Int, Int)
divEucli x y = (div x y , mod x y)
                --where a = div x y 
                     --b = mod x y

index :: Int -> [Int] -> [Int]
index a [] = []
index a xs = contaIndex 1 a xs

contaIndex :: Int -> Int -> [Int] -> [Int]
contaIndex n a [] = []
contaIndex n a (x:xs) | (a == x) = concat [[n], contaIndex (n+1) a xs]
                      | otherwise = contaIndex (n+1) a xs

ordenaPeso :: (a -> Int) -> [a] -> [a]
ordenaPeso f [] = []
ordenaPeso f (x:xs) = ordenaPeso f [y | y <- xs, f(y) < f(x)] ++ [x] ++ ordenaPeso f [y | y <- xs, f(y) >= f(x)]

aplica :: [(a -> a)] -> [a] -> [a]
aplica [] xs = xs
aplica fs [] = []
aplica (f:fs) xs = aplica fs (map f (xs))

funcParc :: [(a -> a -> a)] -> [a] -> [(a -> a)]
funcParc [] xs = []
funcParc xs [] = []
funcParc (f:fs) (x:xs) = concat [[f x], funcParc (fs) (xs)]

usarFuncP :: [(a -> a)] -> [a] -> [a]
usarFuncP [] xs = xs
usarFuncP fs [] = []
usarFuncP (f:fs) (x:xs) = concat [[f x], usarFuncP (fs) (xs)]

data Expr = Literal Int | Soma Expr Expr | Subtrai Expr Expr | Variavel String

instance Show Expr  where--Perfect
 show (Literal a)  | a >= 0 = (show a)
                   | a < 0 = "(" ++ (show a) ++ ")"
 show (Soma a b)  = "(" ++ (show a) ++ " + " ++ (show b) ++ ")"
 show (Subtrai a b) = "(" ++ (show a) ++ " - " ++ (show b) ++ ")"
 show (Variavel a) = (show a)

avaliar :: [(String, Int)] -> Expr -> Int
avaliar l (Literal k) = k
avaliar l (Soma e1 e2) = (avaliar l e1) + (avaliar l e2)
avaliar l (Subtrai e1 e2) = (avaliar l e1) - (avaliar l e2)
avaliar l (Variavel x) = snd (head [(s, i) | (s, i) <- l, s==x])

type Chave = [(Char, Char)]

rot13parcial :: Chave -- troca 'a' por 'n', 'b' por 'o', etc.
rot13parcial = [('a', 'n'), ('b', 'o'), ('c', 'p'), ('d', 'q'), ('e', 'r'), ('f', 's'),
 ('g', 't'), ('h', 'u'), ('i', 'v'), ('j', 'w'), ('k', 'x'), ('l', 'y'), ('m', 'z')] --apenas duplicar com tuplas trocadas para completar "rot13completa"
