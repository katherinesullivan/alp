module PrettyPrinter
  ( printTerm
  ,     -- pretty printer para terminos
    printType     -- pretty printer para tipos
  )
where


import Common
import Data.Maybe
import Data.Char


import           Common
import           Text.PrettyPrint.HughesPJ
import           Prelude                 hiding ( (<>) )
-- lista de posibles nombres para variables
vars :: [String]
vars =
  [ c : n
  | n <- "" : map show [(1 :: Integer) ..]
  , c <- ['x', 'y', 'z'] ++ ['a' .. 'w']
  ]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos

pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k         ) = text (vs !! (ii - k - 1))
pp _  _  (Free  (Global s)) = text s

pp ii vs (i :@: c         ) = sep
  [ parensIf (isLam i || isNotAtom i) (pp ii vs i)
  , nest 1 (parensIf (isLam c || isApp c || isNotAtom c) (pp ii vs c))
  ]
pp ii vs (Lam t c) =
  text "\\"
    <> text (vs !! ii)
    <> text ":"
    <> printType t
    <> text ". "
    <> pp (ii + 1) vs c
pp ii vs (Let t1 t2) = text "Let "
                       <> text (vs !! ii)
                       <> text " = "
                       <> pp ii vs t1
                       <> text " in "
                       <> pp ii vs t2
pp ii vs Zero = text "0"
pp ii vs (Suc n) = printNum ii vs (Suc n) 0
pp ii vs (R t1 t2 t3) = text "R "
                        <>
                        sep [pp ii vs t1, pp ii vs t2, pp ii vs t3]
pp ii vs Nil = text "[]"
pp ii vs (Cons x xs) = sep [pp ii vs x, text ":", pp ii vs xs]
pp ii vs (RL t1 t2 t3) = text "RL "
                         <> 
                         sep [pp ii vs t1, pp ii vs t2, pp ii vs t3]


printNum :: Int -> [String] -> Term -> Int -> Doc
printNum _ _ Zero i = text (show i)
printNum ii vs (Suc n) i = printNum ii vs n (i+1)
printNum _ _ _ _ = error "printNum error" -- no deberia ingresar

isLam :: Term -> Bool
isLam (Lam _ _) = True
isLam _         = False

isApp :: Term -> Bool
isApp (_ :@: _) = True
isApp _         = False

isNotAtom :: Term -> Bool 
isNotAtom (Let _ _) = True
isNotAtom (R _ _ _) = True
isNotAtom (RL _ _ _) = True
isNotAtom (Cons _ _) = True
isNotAtom (Suc _) = True
isNotAtom _ = False

-- pretty-printer de tipos
printType :: Type -> Doc
printType EmptyT = text "E"
printType Nat = text "Nat"
printType ListNat = text "ListNat"
printType (FunT t1 t2) =
  sep [parensIf (isFun t1) (printType t1), text "->", printType t2]


isFun :: Type -> Bool
isFun (FunT _ _) = True
isFun _          = False

fv :: Term -> [String]
fv (Bound _         ) = []
fv (Free  (Global n)) = [n]
fv (t   :@: u       ) = fv t ++ fv u
fv (Lam _   u       ) = fv u
fv (Let t1 t2) = fv t1 ++ fv t2
fv Zero = []
fv (Suc n) = fv n
fv (R t1 t2 t3) = fv t1 ++ fv t2 ++ fv t3
fv Nil = []
fv (Cons x xs) = fv x ++ fv xs
fv (RL t1 t2 t3) = fv t1 ++ fv t2 ++ fv t3

---
printTerm :: Term -> Doc
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t

