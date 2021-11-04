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
  [ parensIf (isLam i || isLet i || isR i || isRL i) (pp ii vs i)
  , nest 1 (parensIf (isLam c || isApp c || isLet c || isR c || isRL c) (pp ii vs c))
  ]
pp ii vs (Lam t c) =
  text "\\"
    <> text (vs !! ii)
    <> text ":"
    <> printType t
    <> text ". "
    <> pp (ii + 1) vs c
pp ii vs (Let (Global s) t1 t2) = text "Let "
                                  <>
                                  text s
                                  <>
                                  text " = "
                                  <>
                                  pp ii vs t1
                                  <>
                                  text " in "
                                  <>
                                  pp ii vs t2
pp ii vs Zero = text "0"
pp ii vs (Suc n) = text "suc "
                   <> 
                   pp ii vs n -- aca imprimos numeros o que?
pp ii vs (R t1 t2 t3) = text "R "
                        <>
                        sep [pp ii vs t1, pp ii vs t2, pp ii vs t3]
pp ii vs Nil = text "[]"
pp ii vs (Cons x xs) = sep [pp ii vs x, text ":", pp ii vs xs]
pp ii vs (RL t1 t2 t3) = text "RL "
                         <> 
                         sep [pp ii vs t1, pp ii vs t2, pp ii vs t3]


isLam :: Term -> Bool
isLam (Lam _ _) = True
isLam _         = False

isApp :: Term -> Bool
isApp (_ :@: _) = True
isApp _         = False

isLet :: Term -> Bool 
isLet (Let _ _ _) = True 
isLet _           = False

isR :: Term -> Bool 
isR (R _ _ _) = True
isR _         = False

isRL :: Term -> Bool 
isRL (RL _ _ _) = True 
isRL _          = False

-- pretty-printer de tipos
printType :: Type -> Doc
printType EmptyT = text "E"
printType Nat = text "Nat"
printType ListNat = text "List Nat"
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
fv (Let (Global n) t1 t2) = [n] ++ fv t1 ++ fv t2
fv Zero = []
fv (Suc _) = [] -- aca no haria falta la llamada recursiva pq suponga que a esta altura tipa
fv (R t1 t2 t3) = fv t1 ++ fv t2 ++ fv t3 -- si tipa, fv t3 se puede tirar
fv Nil = []
fv (Cons _ _) = [] -- aca no haria falta la llamada recursiva pq suponga que a esta altura tipa
fv (RL t1 t2 t3) = fv t1 ++ fv t2 ++ fv t3 -- si tipa, fv t3 se puede tirar

---
printTerm :: Term -> Doc
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t


















----------------------------


data ParseResult a = Ok a | Failed String
                     deriving Show                     
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                         Ok a     -> k a s l
                         Failed e -> Failed e
                         
returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l

happyError :: P a
happyError = \ s i -> Failed $ "Línea "++(show (i::LineNumber))++": Error de parseo\n"++(s)

data Token = TVar String
               | TTypeE
               | TDef
               | TAbs
               | TDot
               | TOpen
               | TClose 
               | TColon
               | TArrow
               | TEquals
               | TEOF
               | TLet
               | TIn
               deriving Show

----------------------------------
lexer :: (Token -> String -> LineNumber -> ParseResult a2) -> String -> LineNumber -> ParseResult a2
lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s)  ->  \line -> lexer cont s (line + 1)
                    (c:cs)
                          | isSpace c -> lexer cont cs
                          | isAlpha c -> lexVar (c:cs)
                    ('-':('-':cs)) -> lexer cont $ dropWhile ((/=) '\n') cs
                    ('{':('-':cs)) -> consumirBK 0 0 cont cs
                    ('-':('}':cs)) -> \ line -> Failed $ "Línea "++(show line)++": Comentario no abierto"
                    ('-':('>':cs)) -> cont TArrow cs
                    ('\\':cs)-> cont TAbs cs
                    ('.':cs) -> cont TDot cs
                    ('(':cs) -> cont TOpen cs
                    -- ('-':('>':cs)) -> cont TArrow cs
                    (')':cs) -> cont TClose cs
                    (':':cs) -> cont TColon cs
                    ('=':cs) -> cont TEquals cs
                    unknown -> \line -> Failed $ 
                     "Línea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                    
                    
                    
                    where lexVar cs = case span isAlpha cs of
                              ("E",rest)    -> cont TTypeE rest
                              ("def",rest)  -> cont TDef rest
                              ("Let",rest)  -> cont TLet rest
                              ("in",rest)   -> cont TIn rest
                              (var,rest)    -> cont (TVar var) rest
                          consumirBK anidado cl cont s = case s of
                              ('-':('-':cs)) -> consumirBK anidado cl cont $ dropWhile ((/=) '\n') cs
                              ('{':('-':cs)) -> consumirBK (anidado+1) cl cont cs	
                              ('-':('}':cs)) -> case anidado of
                                                  0 -> \line -> lexer cont cs (line+cl)
                                                  _ -> consumirBK (anidado-1) cl cont cs
                              ('\n':cs) -> consumirBK anidado (cl+1) cont cs
                              (_:cs) -> consumirBK anidado cl cont cs    
                              [] -> \line -> Failed $ "Línea " ++ (show line) ++ ": Comentario no cerrado" -- esta ok agg?
                                           
-- stmts_parse s = parseStmts s 1
-- stmt_parse s = parseStmt s 1
-- term_parse s = term s 1