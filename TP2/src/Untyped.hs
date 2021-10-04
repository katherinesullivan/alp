module Untyped where

import           Control.Monad
import           Data.List
import           Data.Maybe

import           Common

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
vapp = undefined

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii) (_, lEnv) = lEnv !! ii
eval' _          _         = undefined


--------------------------------
-- Sección 4 - Mostrando Valores
-- Ejercicio 5
--------------------------------

quote :: Value -> Term
quote (VNeutral (NFree var)) = Free var
quote (VNeutral (NApp neu val)) = quote (VNeutral neu) :@: quote val
quote (VLam fun) = undefined






