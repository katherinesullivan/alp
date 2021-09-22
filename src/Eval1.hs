module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado nulo
-- Completar la definición
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Int
lookfor v s = s M.! v

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalua un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm Skip s = Skip :!: s
stepComm (Let v e) s = let n :!: s' = evalExp e s
                       in Skip :!: update v n s'
stepComm (Seq Skip c1) s = c1 :!: s
stepComm (Seq c0 c1) s = let c0' :!: s' = stepComm c0 s
                         in (Seq c0' c1) :!: s'
stepComm (IfThenElse b c0 c1) s = let nb :!: s' = evalExp b s
                                  in if nb then c0 :!: s' else c1 :!: s'
stepComm c0@(Repeat c b) s = (Seq c (IfThenElse b Skip c0)) :!: s

-- Evalua una expresion
-- Completar la definición
evalExp :: Exp a -> State -> Pair a State
evalExp (Const n) s = n :!: s
evalExp (Var v) s = lookfor v s :!: s
evalExp (UMinus e) s = let n :!: s' = evalExp e s
                       in -n :!: s'
evalExp (Plus e0 e1) s = let n0 :!: s' = evalExp e0 s
                             n1 :!: s'' = evalExp e1 s'
                         in n0 + n1 :!: s''
evalExp (Minus e0 e1) s = let n0 :!: s' = evalExp e0 s
                              n1 :!: s'' = evalExp e1 s'
                          in n0 - n1 :!: s''
evalExp (Times e0 e1) s = let n0 :!: s' = evalExp e0 s
                              n1 :!: s'' = evalExp e1 s'
                          in n0 * n1 :!: s''
evalExp (Div e0 e1) s = let n0 :!: s' = evalExp e0 s
                            n1 :!: s'' = evalExp e1 s'
                        in div n0 n1 :!: s'' -- no se evalua la condicion de n1!=0 pq la idea es dejar que el haskell maneje los errores
evalExp (EAssgn v e) s = let n :!: s' = evalExp e s
                         in n :!: update v n s'
evalExp (ESeq e0 e1) s = let n0 :!: s' = evalExp e0 s
                         in evalExp e1 s' -- == n1 :!: s''
evalExp BTrue s = True :!: s
evalExp BFalse s = False :!: s
evalExp (Eq e0 e1) s = let n0 :!: s' = evalExp e0 s
                           n1 :!: s'' = evalExp e1 s'
                       in (n0 == n1) :!: s''
evalExp (NEq e0 e1) s = let n0 :!: s' = evalExp e0 s
                            n1 :!: s'' = evalExp e1 s'
                        in (n0 /= n1) :!: s''
evalExp (Lt e0 e1) s = let n0 :!: s' = evalExp e0 s
                           n1 :!: s'' = evalExp e1 s'
                       in (n0 < n1) :!: s''
evalExp (Gt e0 e1) s = let n0 :!: s' = evalExp e0 s
                           n1 :!: s'' = evalExp e1 s'
                       in (n0 > n1) :!: s''
evalExp (Not e) s = let b :!: s' = evalExp e s
                    in not b :!: s'
evalExp (Or e0 e1) s = let b0 :!: s' = evalExp e0 s
                           b1 :!: s'' = evalExp e1 s'
                       in (b0 || b1) :!: s''
evalExp (And e0 e1) s = let b0 :!: s' = evalExp e0 s
                            b1 :!: s'' = evalExp e1 s'
                        in (b0 && b1) :!: s''

