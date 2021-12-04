module Eval1
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
-- import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )
import GHC.Generics (C1)

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado
newtype State a = State { runState :: Env -> Pair a Env }

instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f = State (\s -> let (v :!: s') = runState m s in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 1.b: Implementar el evaluador utilizando la monada State

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (stepCommStar p) initEnv)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> m Comm
stepComm Skip = return Skip
stepComm (Let v e) = evalExp e >>= (update v) >>= (\ _ -> return Skip)
stepComm (Seq Skip c1) = return c1
stepComm (Seq c0 c1) = do c <- stepComm c0
                          return (Seq c c1)
stepComm (IfThenElse b c0 c1) = evalExp b >>= (\ b' -> if b' then return c0 else return c1) 
stepComm (Repeat c b) = return (Seq c (IfThenElse b Skip (Repeat c b)))

-- Evalua una expresion
evalExp :: MonadState m => Exp a -> m a
evalExp (Const n) = return n
evalExp (Var v) = lookfor v
evalExp (UMinus e) = evalExp e >>= (\ n -> return (-n))
evalExp (Plus e0 e1) = evalExp e0 >>= (\ n0 -> evalExp e1 >>= ( \ n1 -> return (n0 + n1)))
evalExp (Minus e0 e1) = evalExp e0 >>= (\ n0 -> evalExp e1 >>= ( \ n1 -> return (n0 - n1)))
evalExp (Times e0 e1) = evalExp e0 >>= (\ n0 -> evalExp e1 >>= ( \ n1 -> return (n0 * n1)))
evalExp (Div e0 e1) = evalExp e0 >>= (\ n0 -> evalExp e1 >>= ( \ n1 -> return (div n0 n1)))
evalExp BTrue = return True
evalExp false = return False
evalExp (Lt e0 e1) = evalExp e0 >>= (\ n0 -> evalExp e1 >>= ( \ n1 -> return (n0 < n1)))
evalExp (Gt e0 e1) = evalExp e0 >>= (\ n0 -> evalExp e1 >>= ( \ n1 -> return (n0 > n1)))
evalExp (And e0 e1) = evalExp e0 >>= (\ b0 -> evalExp e1 >>= ( \ b1 -> return (b0 && b1)))
evalExp (Or e0 e1) = evalExp e0 >>= (\ b0 -> evalExp e1 >>= ( \ b1 -> return (b0 || b1)))
evalExp (Not e) = evalExp e >>= (\ b -> return (! b))
evalExp (Eq e0 e1) = evalExp e0 >>= (\ n0 -> evalExp e1 >>= ( \ n1 -> return (n0 == n1)))
evalExp (NEq e0 e1) = evalExp e0 >>= (\ n0 -> evalExp e1 >>= ( \ n1 -> return (n0 != n1)))
evalExp (EAssgn v e) = evalExp e >>= (\ n -> do update v n
                                                return n)
evalExp (ESeq e0 e1) = do evalExp e0
                          evalExp e1