{-# LANGUAGE RankNTypes #-}

newtype Nat = forall x . (x -> x) -> x -> x

double :: forall a . (a -> a) -> a -> a
double = \ f -> \ x -> f (f x)

doubleNat :: (Nat -> Nat) -> Nat -> Nat
