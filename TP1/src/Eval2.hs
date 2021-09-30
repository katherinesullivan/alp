module Eval2
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
lookfor :: Variable -> State -> Either Error Int
lookfor v s = case M.lookup v s of
                Nothing -> Left UndefVar
                Just x -> Right x

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do (c' :!: s') <- stepComm c s
                         stepCommStar c' s'

-- Evalua un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm Skip s = Right ( Skip :!: s )
stepComm (Let v e) s = let r = evalExp e s
                       in case r of 
                            Left a -> Left a
                            Right (n :!: s') -> Right (Skip :!: update v n s')
stepComm (Seq Skip c1) s = Right ( c1 :!: s )
stepComm (Seq c0 c1) s = let r = stepComm c0 s
                         in case r of
                              Left a -> Left a
                              Right (c0' :!: s') -> Right ((Seq c0' c1) :!: s')
stepComm (IfThenElse b c0 c1) s = let r = evalExp b s
                                  in case r of
                                       Left a -> Left a
                                       Right (nb :!: s') -> Right (if nb then c0 :!: s' else c1 :!: s')
stepComm c0@(Repeat c b) s = Right (Seq c (IfThenElse b Skip c0) :!: s)


-- Evalua una expresion
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const n) s = Right (n :!: s)
evalExp (Var v) s = case lookfor v s of 
                      Left UndefVar -> Left UndefVar
                      Right x -> Right (x :!: s)
evalExp (UMinus e) s = let r = evalExp e s
                       in case r of
                            Left a -> Left a
                            Right (n :!: s') -> Right (-n :!: s')
evalExp (Plus e0 e1) s = let r = evalExp e0 s
                             r2 = case r of
                                    Left a -> (Left a, 0)
                                    Right (n0 :!: s') -> (evalExp e1 s', n0)
                         in case r2 of
                              (Left a, _) -> Left a
                              (Right (n1 :!: s''), n0) -> Right (n0 + n1 :!: s'')
evalExp (Minus e0 e1) s = let r = evalExp e0 s
                              r2 = case r of
                                     Left a -> (Left a, 0)
                                     Right (n0 :!: s') -> (evalExp e1 s', n0)
                          in case r2 of
                               (Left a, _) -> Left a
                               (Right (n1 :!: s''), n0) -> Right (n0 - n1 :!: s'')
evalExp (Times e0 e1) s = let r = evalExp e0 s
                              r2 = case r of
                                     Left a -> (Left a, 0)
                                     Right (n0 :!: s') -> (evalExp e1 s', n0)
                          in case r2 of
                               (Left a, _) -> Left a
                               (Right (n1 :!: s''), n0) -> Right (n0 * n1 :!: s'')
evalExp (Div e0 e1) s = let r = evalExp e0 s
                            r2 = case r of
                                   Left a -> (Left a, 0)
                                   Right (n0 :!: s') -> (evalExp e1 s', n0)
                        in case r2 of
                             (Left a, _) -> Left a
                             (Right (n1 :!: s''), n0) -> if n1 == 0 then Left DivByZero else Right (div n0 n1 :!: s'')
evalExp (EAssgn v e) s = let r = evalExp e s
                         in case r of
                              Left a -> Left a
                              Right (n :!: s') -> Right (n :!: update v n s')
evalExp (ESeq e0 e1) s = let r = evalExp e0 s
                         in case r of
                              Left a -> Left a
                              Right (n0 :!: s') -> evalExp e1 s'
evalExp BTrue s = Right (True :!: s)
evalExp BFalse s = Right (False :!: s)
evalExp (Eq e0 e1) s = let r = evalExp e0 s
                           r2 = case r of
                                  Left a -> (Left a, 0)
                                  Right (n0 :!: s') -> (evalExp e1 s', n0)
                       in case r2 of
                            (Left a, _) -> Left a
                            (Right (n1 :!: s''), n0) -> Right ((n0 == n1) :!: s'')
evalExp (NEq e0 e1) s = let r = evalExp e0 s
                            r2 = case r of
                                   Left a -> (Left a, 0)
                                   Right (n0 :!: s') -> (evalExp e1 s', n0)
                        in case r2 of
                             (Left a, _) -> Left a
                             (Right (n1 :!: s''), n0) -> Right ((n0 /= n1) :!: s'')
evalExp (Lt e0 e1) s = let r = evalExp e0 s
                           r2 = case r of
                                  Left a -> (Left a, 0)
                                  Right (n0 :!: s') -> (evalExp e1 s', n0)
                       in case r2 of
                            (Left a, _) -> Left a
                            (Right (n1 :!: s''), n0) -> Right ((n0 < n1) :!: s'')
evalExp (Gt e0 e1) s = let r = evalExp e0 s
                           r2 = case r of
                                  Left a -> (Left a, 0)
                                  Right (n0 :!: s') -> (evalExp e1 s', n0)
                       in case r2 of
                            (Left a, _) -> Left a
                            (Right (n1 :!: s''), n0) -> Right ((n0 > n1) :!: s'')
evalExp (Not e) s = let r = evalExp e s
                    in case r of
                         Left a -> Left a
                         Right (b :!: s') -> Right (not b :!: s')
evalExp (Or e0 e1) s = let r = evalExp e0 s
                           r2 = case r of
                                  Left a -> (Left a, True)
                                  Right (b0 :!: s') -> (evalExp e1 s', b0)
                       in case r2 of
                            (Left a, _) -> Left a
                            (Right (b1 :!: s''), b0) -> Right ((b0 || b1) :!: s'')
evalExp (And e0 e1) s = let r = evalExp e0 s
                            r2 = case r of
                                   Left a -> (Left a, True)
                                   Right (b0 :!: s') -> (evalExp e1 s', b0)
                        in case r2 of
                             (Left a, _) -> Left a
                             (Right (b1 :!: s''), b0) -> Right ((b0 && b1) :!: s'')
