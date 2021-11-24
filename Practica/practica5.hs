-- Ejercicio 1

-- a)

-- A newtype guarantees that your data will have exactly the same representation at runtime,
-- as the type that you wrap. While data declares a brand new data structure at runtime.

data Pair a = P (a,a)

fmapPair :: (a -> b) -> Pair a -> Pair b
fmapPair f (P (x,y)) = P (f x, f y)

{-
Probamos el buen comportamiento:

fmap id = id : (functor.1)
fmap f . fmap g = fmap (f . g) : (functor.2)

functor.1: 

fmapPair id (P (x,y)) 
= {def.fmapPair}
P (id x,id y)
= {def.id}
P (x,y)
= {def.id}
id (P (x,y))

functor.2:

(fmapPair f . fmapPair g) (P (x,y))
= {def.(.)}
fmapPair f (fmapPair g (P (x,y)))
= {def.fmapPair}
fmapPair f (P (g x,g y))
= {def.fmapPair}
P (f (g x),f (g y))
= {def.(.)}
P ((f . g) x,(f . g) y)
= {def.fmapPair}
fmapPair (f . g) (P (x,y))

-}

-- b)
data Tree a = Empty | Branch a (Tree a) (Tree a)

fmapTree :: (a -> b) -> Tree a -> Tree b
fmapTree f Empty = Empty
fmapTree f (Branch x l r) = Branch (f x) (fmapTree f l) (fmapTree f r)

{-
Probamos el buen comportamiento:

fmap id = id : (functor.1)
fmap f . fmap g = fmap (f . g) : (functor.2)

functor.1: 
Por inducción estructural sobre el funtor

fmapTree id Empty
= {def.fmapTree}
Empty
= {def.id}
id Empty

fmapTree id (Branch x l r)
= {def.fmapTree}
Branch (id x) (fmapTree id l) (fmapTree id r)
= {HI}
Branch (id x) (id l) (id r)
= {def.id}
Branch x l r
= {def.id}
id (Branch x l r)

functor.2:
Por inducción estructural sobre el funtor

(fmapTree f . fmapTree g) Empty 
= {def.(.)}
fmapTree f (fmapTree g Empty)
= {def.fmapTree}
fmapTree f Empty
= {def.fmapTree}
Empty
= {def.fmapTree}
fmapTree (f . g) Empty

(fmapTree f . fmapTree g) (Branch x l r)
= {def.(.)}
fmapTree f (fmapTree g (Branch x l r))
= {def.fmapTree}
fmapTree f (Branch (g x) (fmapTree g l) (fmapTree g r))
= {def.fmapTree}
Branch (f (g x)) (fmapTree f (fmapTree g l)) (fmapTree f (fmapTree g r))
= {def.(.)}
Branch ((f . g) x) ((fmapTree f . fmapTree g) l) ((fmapTree f . fmapTree g) r)
= {HI}
Branch ((f . g) x) (fmapTree (f . g) l) (fmapTree (f . g) r)
= {def.fmapTree}
fmapTree (f . g) (Branch x l r)

-}

-- c)
data GenTree a = Gen a [GenTree a]

fmapGenTree :: (a -> b) -> GenTree a -> GenTree b
fmapGenTree f (Gen x xs) = Gen (f x) (map (fmapGenTree f) xs)

{-
Probamos el buen comportamiento:

fmap id = id : (functor.1)
fmap f . fmap g = fmap (f . g) : (functor.2)

functor.1: 
Por inducción estructural sobre el funtor

fmapGenTree id (Gen x xs)
= {def.fmapGenTree}
Gen (id x) (map (fmapGenTree id) xs)
= {HI}
Gen (id x) (map id xs)
= {suponemos (o podriamos probar) que las listas tambien son functores y 
por lo tanto su funcion map cumple las propiedades del map de funtores}
Gen (id x) (id xs)
= {def.id}
Gen x xs
= {def.id}
id (Gen x xs)

functor.2:
Por inducción estructural sobre el funtor

(fmapGenTree f . fmapGenTree g) (Gen x xs)
= {del.(.)}
fmapGenTree f (fmapGenTree g (Gen x xs))
= {def.fmapGenTree}
fmapGenTree f (Gen (g x) (map (fmapGenTree g) xs))
= {def.fmapGenTree}
Gen (f (g x)) (map (fmapGenTree f) (map (fmapGenTree g) xs))
= {def.(.)}
Gen ((f . g) x) (((map (fmapGenTree f) . (map (fmapGenTree g)) xs)
= {suponemos (o podriamos probar) que las listas tambien son functores y 
por lo tanto su funcion map cumple las propiedades del map de funtores}
Gen ((f . g) x) (map (fmapGenTree f . fmapGenTree g) xs)
= {HI}
Gen ((f . g) x) (map (fmapGenTree (f . g)) xs)
= {def.fmapGenTree}
fmapGenTree (f . g) (Gen x xs)

-}

-- d)
data Cont a = C ((a -> Int) -> Int)

-- i :: b -> Int 
-- g :: (a -> Int) -> Int
-- f :: a -> b

fmapCont :: (a -> b) -> Cont a -> Cont b
fmapCont f (C g) = C (\ i -> g (i . f))

{-
Probamos el buen comportamiento:

fmap id = id : (functor.1)
fmap f . fmap g = fmap (f . g) : (functor.2)

functor.1:
fmapCont id (C g)
= {def.fmapCont}
C (\ i -> g (i . id))
= {def.id}
C (\ i -> g i)
= {f == \i -> g i <=> f i == g i <=> f == g}
C g
= {def.id}
id (C g)

functor.2:
(fmapCont f . fmapCont h) (C g)
= {def.(.)}
fmapCont f (fmapCont h (C g))
= {def.fmapCOnt}
fmapCont f (C (\ i -> g (i . h)))
= {def.fmapCOnt}
C (\ i -> (\ j -> g (j . h)) (i . f))
= {def.\}
C (\ i -> g ((i . f) . h))
= {(.) es asociativo}
C (\ i -> g (i . (f . h)))
= {def.fmapCont}
fmapCont (f . h) (C g)

-}

-- Ejercicio 2

data Func a = Func (a -> a)

instance Functor Func where 
    fmap g (Func h) = Func id

{-
La instancia no es correcta ya que no cumple con la segunda ley de funtores.
fmap f . fmap g = fmap (f . g) : (functor.2)

(fmap f . fmap g) (Func )

-}


data Br b a = B b (a,a)

instance Functor (Br b) where
    fmap f (B x (y,z)) = B x (f z,f y)