module Simplytyped
  ( conversion
  ,    -- conversion a terminos localmente sin nombre
    eval
  ,          -- evaluador
    infer
  ,         -- inferidor de tipos
    quote          -- valores -> terminos
  )
where

import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( (>>=) )
import           Text.PrettyPrint.HughesPJ      ( render )
import           PrettyPrinter
import           Common

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion' []

conversion' :: [String] -> LamTerm -> Term
conversion' b (LVar n    ) = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b (LApp t u  ) = conversion' b t :@: conversion' b u
conversion' b (LAbs n t u) = Lam t (conversion' (n : b) u)
conversion' b (LLet x t u) = Let (Global x) (conversion' b t) (conversion' b u)
conversion' b LZero = Zero
conversion' b (LSuc n) = Suc (conversion' b n)
conversion' b (LR t1 t2 n) = R (conversion' b t1) (conversion' b t2) (conversion' b n)

-----------------------
--- eval
-----------------------

sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n   )           = Free n
sub i t (u   :@: v)           = sub i t u :@: sub i t v
sub i t (Lam t'  u)           = Lam t' (sub (i + 1) t u)
sub i t (Let n t1 t2)         = Let n (sub i t t1) (sub i t t2)
sub i t Zero                  = Zero 
sub i t (Suc n)               = Suc (sub i t n)
sub i t (R t1 t2 n)           = R (sub i t t1) (sub i t t2) (sub i t n)

-- evaluador de términos
eval :: NameEnv Value Type -> Term -> Value
eval _ (Bound _             ) = error "variable ligada inesperada en eval"
eval e (Free  n             ) = fst $ fromJust $ lookup n e
eval _ (Lam      t   u      ) = VLam t u
eval e (Lam _ u  :@: Lam s v) = eval e (sub 0 (Lam s v) u)
eval e (Lam t u1 :@: u2) = let v2 = eval e u2 in eval e (sub 0 (quote v2) u1)
eval e (u        :@: v      ) = case eval e u of
  VLam t u' -> eval e (Lam t u' :@: v)
  _         -> error "Error de tipo en run-time, verificar type checker"
eval e (Let n u1 u2) = let v = eval e u1
                           t = infer e u1
                       in case t of
                            Right t' -> eval ((n, (v, t')):e) u2
                            Left s -> error "Error de tipo en run-time, verificar type checker"
eval e Zero = Num VZero 
eval e (Suc n) = case eval e n of
                    VLam _ _ -> error "Error de tipo en run-time, verificar type checker"
                    Num x -> Num (VSuc x)
eval e (R u1 u2 Zero) = eval e u1
eval e (R u1 u2 (Suc n)) = eval e (u2 :@: R u1 u2 n :@: n)    
eval e (R u1 u2 n) = case eval e n of
                        VLam _ _ -> error "Error de tipo en run-time, verificar type checker"
                        Num VZero -> eval e u1
                        Num (VSuc x) -> eval e (u2 :@: R u1 u2 (quote (Num x)) :@: quote (Num x))                      

-----------------------
--- quoting
-----------------------

quote :: Value -> Term
quote (VLam t f) = Lam t f
quote (Num VZero) = Zero
quote (Num (VSuc x)) = Suc (quote (Num x)) 

----------------------
--- type checker
-----------------------

-- type checker
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=)
  :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 =
  err
    $  "se esperaba "
    ++ render (printType t1)
    ++ ", pero "
    ++ render (printType t2)
    ++ " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free  n) = case lookup n e of
  Nothing     -> notfoundError n
  Just (_, t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt -> infer' c e u >>= \tu ->
  case tt of
    FunT t1 t2 -> if (tu == t1) then ret t2 else matchError t1 tu
    _          -> notfunError tt
infer' c e (Lam t u) = infer' (t : c) e u >>= \tu -> ret $ FunT t tu
infer' c e (Let n t1 t2) = infer' c e t1 >>= \tt1 -> infer' c ((n,(VLam EmptyT t1,tt1)) : e) t2 >>= ret -- el valor con el que crago la variable libre en el entorno puede ser cualquiera total no lo uso (?
infer' c e Zero = ret Nat
infer' c e (Suc t) = infer' c e t >>= \tt -> 
  case tt of 
    Nat -> ret Nat 
    _   -> matchError Nat tt
infer' c e (R t1 t2 t3) = infer' c e t1 >>= \tt1 -> infer' c e t2 >>= \tt2 ->
  case tt2 of 
    FunT u1 (FunT Nat u2) -> if u1 == tt1 && u2 == tt1 then infer' c e t3 >>= \tt3 -> case tt3 of
                                                                                            Nat -> ret tt1
                                                                                            _   -> matchError Nat tt3
                                                         else if u1 == tt1 then matchError tt1 u2
                                                                           else matchError tt1 u1 -- esta bien que solo digamos que esta mal u1?
    _ -> matchError (FunT tt1 (FunT Nat tt1)) tt2

      



----------------------------------
