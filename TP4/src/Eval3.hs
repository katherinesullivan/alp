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
  concat newTrace = StateErrorTrace (\ env trace -> Right ((():!:env) :!: trace++newTrace))


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
  update v i = StateError (\ env trace -> Right ((() :!: update' v i env) :!: trace) where update' = M.insert


-- Ejercicio 3.f: Implementar el evaluador utilizando la monada StateErrorTrace.
-- Evalua un programa en el estado nulo

eval :: Comm -> Either Error (Env, Trace)
eval = case runStateErrorTrace (stepCommStar p) initEnv initTrace of
            Left er -> Left er
            Right ((x :!: env) :!: trace) -> Right (env,trace)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
-- stepCommStar :: [dar el tipo segun corresponda]
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
-- stepComm :: [dar el tipo segun corresponda]
stepComm = undefined

-- Evalua una expresion 
-- evalIntExp :: [dar el tipo segun corresponda]
evalExp = undefined
