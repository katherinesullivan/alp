{-# LANGUAGE RankNTypes #-}

type PairNat = forall x . (Int -> Int -> x) -> x

data Pair = P Int Int deriving Show

pairNat :: Int -> Int -> PairNat
pairNat x y = \ p -> p x y

fstNat :: PairNat -> Int
fstNat p = p (\x y -> x)