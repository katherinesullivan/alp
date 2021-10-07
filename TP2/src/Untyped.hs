module Untyped where

import           Control.Monad
import           Data.List
import           Data.Maybe

import           Common
import Language.Haskell.TH (InjectivityAnn)
-- import StringBuffer (StringBuffer(len))

----------------------------------------------
-- Sección 2  
-- Ejercicio 2: Conversión a términos localmente sin nombres
----------------------------------------------

-- Función que convierte términos de λ-cálculo a términos equivalentes en la representación localmente sin nombres.
conversion :: LamTerm -> Term
conversion x = conversionAux x []

conversionAux :: LamTerm -> [String] -> Term
conversionAux (App term1 term2) xs = conversionAux term1 xs :@: conversionAux term2 xs
conversionAux (Abs var term) xs = Lam (conversionAux term (var:xs))
conversionAux (LVar var) xs = if idx == -1 then Free (Global var) else Bound idx
                              where idx = lookfor xs var

-- Dada una lista de variables xs y una variable var, retorna el índice de la primera ocurrencia de var en xs, o -1 si no está.
lookfor :: [String] -> String -> Int
lookfor xs x = lookforAux xs x 0

lookforAux :: [String] -> String -> Int -> Int
lookforAux [] var idx = -1
lookforAux (x:xs) var idx = if var == x then idx else lookforAux xs var (idx+1)

-------------------------------
-- Sección 3 - Evaluación
-- Ejercicios 3 y 4
-------------------------------

vapp :: Value -> Value -> Value
vapp (VLam abs) val = abs val
vapp (VNeutral neu) val = VNeutral (NApp neu val)

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

-- como hacer las 2 apps 
-- como agregar en E-ABS
eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii) (_, lEnv) = lEnv !! ii
--eval' (t1@(t1' :@: t1'') :@: t2) (e, lEnv) = eval' (eval' t1 (e,lEnv)
-- modif de alguna manera el environment y dsp eval de t2 but how
eval' (t1 :@: t2) (e, lEnv) = vapp (eval' t1 (e,lEnv)) (eval' t2 (e,lEnv))
eval' (Lam t) (e, lEnv) = VLam (eval' t (e,:lEnv)) --agg la variable binding cual sera?
eval' ((Lam t1) :@: t2) (e, lEnv) = sust t1 t2 0
eval' (Free name) (e, lEnv) = case find (\(a,_) -> a == name) e of
                                  Nothing -> perror "variable global inexistente"
                                  Just (_,v) -> v

-- fc aparte app


-- sustitucion

shift :: Int -> Int -> Term -> Term
shift i c (Bound ii) = if ii < c then Bound ii else Bound ii+i
shift i c (Lam t) = Lam (shift i (c+1) t)
shift i c (t1 :@: t2) = shift i c t1 :@: shift i c t2
shift i c (Free name) = Free name

sust :: Term -> Term -> Int -> Term 
sust (Bound n) t1 m = if n == m then t1 else Bound n
sust (Lam t) t1 m = Lam (sust t (shift 1 0 t) (m+1))
sust (t :@: t') t1 m = sust t t1 m :@: sust t' t1 m
sust (Free name) _ _ = Free name
--------------------------------
-- Sección 4 - Mostrando Valores
-- Ejercicio 5
--------------------------------

quote :: Value -> Term
quote val = replace (quoteAux val 0) 0

quoteAux :: Value -> Int -> Term
quoteAux (VNeutral (NFree var)) idx = Free var
quoteAux (VNeutral (NApp neu val)) idx = quoteAux (VNeutral neu) idx :@: quoteAux val idx
quoteAux (VLam fun) idx = Lam (quoteAux (fun (VNeutral (NFree (Quote idx)))) (idx+1))

-- quoteAux (VLam fun) idx = let term = quoteAux (fun (VNeutral (NFree (Quote idx)))) (idx+1)
                        --   in Lam (replace term 1)

-- replace :: Term -> Term
-- replace (Free (Quote k)) = Bound (i - k - 1) -- me falta el i
-- replace x = x


-- quote :: Value -> Term
-- quote val = fst (quote2 val 0)

-- quote2 :: Value -> Int -> (Term, Int)
-- quote2 (VNeutral (NFree var)) idx = (Free var, idx)
-- quote2 (VNeutral (NApp neu val)) idx = (fst (quote2 (VNeutral neu) idx) :@: fst (quote2 val idx), idx)
-- quote2 (VLam abs) idx = let (term, total) = quote2 (abs (VNeutral (NFree (Quote idx)))) (idx+1)
--                         in (Lam (replace term 1), total)

-- replace :: Term -> Int -> Term
-- replace (Free (Quote k)) i = Bound (i - k - 1)
-- replace x _ = x

replace :: Term -> Int -> Term 
replace (Free (Quote k)) i = Bound (i - k - 1)
replace (Free x) _ = Free x
replace (t1 :@: t2) i = replace t1 i :@: replace t2 i
replace (Lam t) i = Lam (replace t (i+1))
replace x _ = x -- caso x == Bound n, no deberia ocurrir


ejemplo :: Value -> Value 
-- ejemplo (VLam fun) = VLam (\ x -> VNeutral (NApp (x) (VLam (\ y -> VNeutral (NApp (x) y)))))
ejemplo (VNeutral x) = VNeutral (NApp x (VLam (ejemplo2 x)))


ejemplo2 :: Neutral -> Value -> Value
ejemplo2 x y = VNeutral (NApp x y)




esPrimo :: Int -> Bool 
esPrimo n = [x | x <- [1..n], mod n x == 0] == [1,n]

isprime :: Int -> Bool
isprime 1 = False
isprime 2 = True
isprime n = isprimerec n (n-1)

isprimerec :: Int -> Int -> Bool
isprimerec _ 1 = False
isprimerec n t = if (n `rem` t) == 0 then False else isprimerec n (n-1)

{-
esPrimo :: Int -> Bool 
esPrimo n = [x | x <- [1..n], mod n x == 0] == [1,n]


isprime :: Int -> Bool
isprime n = isprimerec n (n-1) -- el n-1 podria ser un (floor (sqrt n))

isprimerec :: Int -> Int -> Bool
isprimerec n t = if t == 1 || n == 1 then True else if (n mod t) == 0 then False else isprimerec n (t-1)


En lambda calculo:

-- el pred n podria ser floor (srqt n)
def isprime = \ n . isprimerec n (pred n)

def isprimerec = Y (\ f n t . ( or (is0 (pred t)) (is0 (pred n)) ) true ( (is0 (mod n t)) false (f n (pred t)) ))


def resta = \ n m . m pred n

myMod :: Int -> Int -> Int 
myMod x y = if x <= y then x else myMod (x - y) y

lt :: Int -> Int -> Bool -- x <= y
lt x y = if x == 0 then True else if y == 0 then False else lt (x-1) (y-1)



def mod = Y ( \ f x y . ( lt x y ) x ( f (resta x y) y ) )

def lt = Y ( \ f x y . (is0 x) true ( (is0 y) false ( f (pred x) (pred y) ) ) )
-}
