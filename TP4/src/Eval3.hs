module Eval3
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

-- Traza vacía
initTrace :: Trace
initTrace = ""

-- Ejercicio 3.a: Proponer una nueva mónada que  
-- lleve una traza de ejecución (además de manejar errores y estado).
-- y dar su instancia de mónada. Llamarla |StateErrorTrace|. 
-- COMPLETAR
newtype StateErrorTrace a = 
  StateErrorTrace { runStateErrorTrace :: Env -> Trace -> Either Error (Pair (Pair a Env) Trace) }

instance Monad StateErrorTrace where
  return x = StateErrorTrace (\ env trace -> Right ((x :!: env) :!: trace))
  m >>= f = StateErrorTrace (\ env trace -> case runStateErrorTrace m env trace of
                                              Left er -> Left er
                                              Right ((x :!: env') :!: trace') -> runStateErrorTrace (f x) env' trace')

-- Recuerde agregar las siguientes instancias para calmar al GHC:
instance Functor StateErrorTrace where
  fmap = liftM

instance Applicative StateErrorTrace where
  pure  = return
  (<*>) = ap

-- Ejercicio 3.b: Resolver en Monad.hs


-- Ejercicio 3.c: Dar una instancia de MonadTrace para StateErrorTrace.
-- COMPLETAR
instance MonadTrace StateErrorTrace where
  concatTrace newTrace = StateErrorTrace (\ env trace -> Right ((():!:env) :!: trace++newTrace))


-- Ejercicio 3.d: Dar una instancia de MonadError para StateErrorTrace.
-- COMPLETAR
instance MonadError StateErrorTrace where
  throw er = StateErrorTrace (\ env trace -> Left er)

-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorTrace.
-- COMPLETAR
instance MonadState StateErrorTrace where
  lookfor v = StateErrorTrace (\ env trace -> case M.lookup v env of
                                                Nothing -> Left UndefVar
                                                Just x -> Right ((x :!: env) :!: trace))
  update v i = StateErrorTrace (\ env trace -> Right ((() :!: update' v i env) :!: trace)) where update' = M.insert


-- Ejercicio 3.f: Implementar el evaluador utilizando la monada StateErrorTrace.
-- Evalua un programa en el estado nulo

eval :: Comm -> Either Error (Env, Trace)
eval p = case runStateErrorTrace (stepCommStar p) initEnv initTrace of
            Left er -> Left er
            Right ((x :!: env) :!: trace) -> Right (env,trace)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
-- stepCommStar :: [dar el tipo segun corresponda]
stepCommStar :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
-- stepComm :: [dar el tipo segun corresponda]
stepComm :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m Comm
stepComm Skip = return Skip
stepComm (Let v e) = evalExp e >>= (\ x -> do { update v x ; concatTrace ("Let "++v++" = "++show x++"\n") }) >>= (\ _ -> return Skip)
stepComm (Seq Skip c1) = return c1
stepComm (Seq c0 c1) = do c <- stepComm c0
                          return (Seq c c1)
stepComm (IfThenElse b c0 c1) = evalExp b >>= (\ b' -> if b' then return c0 else return c1)
-- stepComm (Repeat c b) = return (Seq c (IfThenElse b Skip (Repeat c b)))
stepComm (While b c) = return (IfThenElse b (Seq c (While b c)) Skip)

-- Evalua una expresion 
-- evalIntExp :: [dar el tipo segun corresponda]
evalExp :: (MonadState m, MonadError m, MonadTrace m) => Exp a -> m a
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
                                                concatTrace ("Let "++v++" = "++show n++"\n")
                                                return n)
evalExp (ESeq e0 e1) = do evalExp e0
                          evalExp e1
