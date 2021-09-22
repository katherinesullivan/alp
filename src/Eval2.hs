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
-- stepComm Skip s = Right ( Skip :!: s )
-- stepComm (Let v e) s = let n :!: s' = evalExp e
--                        in Skip :!: update v n s' -- analizar que retorna el update
-- stepComm (Seq Skip c1) s = Right ( c1 :!: s )
-- stepComm (Seq c0 c1) s = let c0' :!: s' = stepComm c0 s
--                          in Right ( (Seq c0' c1) :!: s' )
-- stepComm (IfThenElse b c0 c1) s = let nb :!: s' = evalExp b s
--                                   in if nb then c0 s' else c1 s'
-- stepComm c0@(Repeat c b) s = Right ( Seq c (IfThenElse b Skip c0) :!: s )
stepComm = undefined

-- Evalua una expresion
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp = undefined
