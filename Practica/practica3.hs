-- EJERCICIO 10

{-
Definición a)
def zero = \ x . false
def suc = \ n . pair true n

Queremos que:
isNotZero zero =\beta false
isNotZero (suc n) =\beta true

def isNotZero = \ n . n true -- isNotZero == fst
def isZero = not isNotZero

Queremos que:
pred zero = zero
pred (suc n) = n

def pred = \ n . (isZero n) n (snd n)

-}

-- EJERCICIO 13

{-
λn.λf x.snd (n H (pair 1 x))
H ≡ λp. if (isZero (fst p)) then (pair 1 (f (snd p))) else (pair 0 (snd p))

λ n f x . snd (n ( λ p . if (isZero (fst p)) then (pair 1 (f (snd p))) else (pair 0 (snd p)) ) (pair 1 x))

aplica H n veces a (pair 1 x)

que hace H?

aplica f (floor (n/2)) veces a x
-}

-- EJERCICIO 14

data BinTree a = Leaf | Bin a (BinTree a) (BinTree a) deriving Show

foldBin :: BinTree a -> b -> (a -> b -> b -> b) -> b 
foldBin Leaf l b = l
foldBin (Bin a t u) l b = b a (foldBin t l b) (foldBin u l b)


-- a)

-- i
isLeaf :: BinTree a -> Bool 
isLeaf t = foldBin t True (\ _ _ _ -> False)

-- ii
mapBin :: (a -> b) -> BinTree a -> BinTree b
mapBin f t = foldBin t Leaf (Bin . f)

-- iii
heightBin :: BinTree a -> Int 
heightBin t = foldBin t 0 (\ _ x y -> max x y + 1)

-- iv
mirrorBin :: BinTree a -> BinTree a
mirrorBin t = foldBin t Leaf (\ a l r -> Bin a r l)


-- b)

{- 
λ-terminos para Leaf, Bin, y foldBin

foldBin Leaf l b = l <=> foldBin Leaf = \ l b -> l

foldBin (Bin a t u) l b = b a (foldBin t l b) (foldBin u l b) 
<=> 
foldBin (Bin a t u) = \ l b -> b a (foldBin t l b) (foldBin u l b)


Definimos foldBin como:
def foldBin = \ x . x

Luego queda que:
def Leaf = \ l b . l
def Bin = \ raiz left right . (\ l b . b raiz (left l b) (right l b))
-}


-- c)

{-
def max = \ x y . (lt x y) y x

def isLeaf = \ t . t true (\ x y z . false)
def mapBin = \ f t . t leaf (\ x . bin (f x))
def heightBin = \ t . t 0 (\ z x y . suc (max x y))
def mirrorBin = \ t . t leaf (\ a l r . bin a r l)
-}


