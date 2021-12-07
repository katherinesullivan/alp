module Eval2
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error (Pair a Env) }


-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

-- Ejercicio 2.a: Dar una instancia de Monad para StateError:
instance Monad StateError where
  return x = StateError (\ env -> Right (x :!: env))
  m >>= f = StateError (\ env -> case runStateError m env of
                                    Left er -> Left er
                                    Right (x :!: env') -> runStateError (f x) env')

-- Ejercicio 2.b: Dar una instancia de MonadError para StateError:
-- COMPLETAR
instance MonadError StateError where
  throw er = StateError (\ _ -> Left er)

-- Ejercicio 2.c: Dar una instancia de MonadState para StateError:
-- COMPLETAR
instance MonadState StateError where
  lookfor v = StateError (\ env -> case M.lookup v env of
                                      Nothing -> Left UndefVar
                                      Just x -> Right (x :!: env))
  update v i = StateError (\ env -> Right (() :!: update' v i env)) where update' = M.insert

-- Ejercicio 2.d: Implementar el evaluador utilizando la monada StateError.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error Env
eval p = case runStateError (stepCommStar p) initEnv of
            Left er -> Left er
            Right (x :!: env) -> Right env

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
stepComm Skip = return Skip
stepComm (Let v e) = evalExp e >>= (update v) >>= (\ _ -> return Skip)
stepComm (Seq Skip c1) = return c1
stepComm (Seq c0 c1) = do c <- stepComm c0
                          return (Seq c c1)
stepComm (IfThenElse b c0 c1) = evalExp b >>= (\ b' -> if b' then return c0 else return c1)
stepComm (While b c) = return (IfThenElse b (Seq c (While b c)) Skip)

-- Evalua una expresion
evalExp :: (MonadState m, MonadError m) => Exp a -> m a
evalExp (Const n) = return n
evalExp (Var v) = lookfor v
evalExp (UMinus e) = evalExp e >>= (\ n -> return (-n))
evalExp (Plus e0 e1) = evalExp e0 >>= (\ n0 -> evalExp e1 >>= ( \ n1 -> return (n0 + n1)))
evalExp (Minus e0 e1) = evalExp e0 >>= (\ n0 -> evalExp e1 >>= ( \ n1 -> return (n0 - n1)))
evalExp (Times e0 e1) = evalExp e0 >>= (\ n0 -> evalExp e1 >>= ( \ n1 -> return (n0 * n1)))
evalExp (Div e0 e1) = evalExp e0 >>= (\ n0 -> evalExp e1 >>= ( \ n1 -> if n1 == 0 then throw DivByZero
                                                                                  else return (div n0 n1)))
evalExp BTrue = return True
evalExp BFalse = return False
evalExp (Lt e0 e1) = evalExp e0 >>= (\ n0 -> evalExp e1 >>= ( \ n1 -> return (n0 < n1)))
evalExp (Gt e0 e1) = evalExp e0 >>= (\ n0 -> evalExp e1 >>= ( \ n1 -> return (n0 > n1)))
evalExp (And e0 e1) = evalExp e0 >>= (\ b0 -> evalExp e1 >>= ( \ b1 -> return (b0 && b1)))
evalExp (Or e0 e1) = evalExp e0 >>= (\ b0 -> evalExp e1 >>= ( \ b1 -> return (b0 || b1)))
evalExp (Not e) = evalExp e >>= (\ b -> return (not b))
evalExp (Eq e0 e1) = evalExp e0 >>= (\ n0 -> evalExp e1 >>= ( \ n1 -> return (n0 == n1)))
evalExp (NEq e0 e1) = evalExp e0 >>= (\ n0 -> evalExp e1 >>= ( \ n1 -> return (n0 /= n1)))
evalExp (EAssgn v e) = evalExp e >>= (\ n -> do update v n
                                                return n)
evalExp (ESeq e0 e1) = do evalExp e0
                          evalExp e1

