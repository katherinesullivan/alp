import Parsing
import Control.Monad
import Control.Applicative ((<|>))
import Data.Char

-- EJERCICIO 2
-- Hecho en teoria.hs

-- EJERCICIO 3

transformador :: Parser String -> Parser String
transformador p = p
                  <|>
                  do string "("
                     result <- p
                     string ")"
                     return result


-- EJERCICIO 4


-- CFG
-- expr → term (’+’ expr | ’-’ expr | ε) 
-- term → factor (’*’ term | ’/’ term | ε)
-- factor → digit | ’(’ expr ’)’
-- digit → ’0’ | ’1’ | · · · | ’9’

data Expr = Num Int | BinOp Op Expr Expr 
data Op = Add | Mul | Min | Div

-- Parsers de la gramatica
digito :: Parser Expr
digito = do d <- digit
            return (Num (digitToInt d))

factor :: Parser Expr
factor = do char '('
            e <- expr
            char ')'
            return e
         <|> digito

term :: Parser Expr
term = do f <- factor
          (do char '*'
              t <- term
              return (BinOp Mul f t))
           <|> 
           (do char '/'
               t <- term
               return (BinOp Div f t))
            <|>
            return f        

expr :: Parser Expr
expr = do t <- term 
          (do char '+'
              e <- expr
              return (BinOp Add t e)) 
           <|> 
           (do char '-'
               e <- expr
               return (BinOp Min t e))
           <|>
           return t

eval :: String -> Expr
eval xs = fst (head (parse expr xs))


-- EJERCICIO 5

type HetList = [Either Int Char]

either' :: Parser (Either Int Char)
either' = do x <- int
             return (Left x)
          <|>
          do string "'"
             x <- alphanum 
             string "'"
             return (Right x)

hetList :: Parser HetList
hetList = do char '['
             x <- either' 
             xs <- many (do{ token (char ','); either' }) -- comportamiento muy similar a lo que hace sepBy
             char ']'             
             return (x:xs)


-- EJERCICIO 6
data Basetype = DInt | DChar | DFloat deriving Show
type Hasktype = [Basetype]

tipoDato :: Parser Basetype
tipoDato = do string "Int"
              return DInt
           <|> 
           do string "Char" 
              return DChar
           <|> 
           do string "Float"
              return DFloat


tiposHaskell :: Parser Hasktype
tiposHaskell = do x <- tipoDato
                  xs <- many (do{ token (string "->"); tipoDato; })
                  return (x:xs)
                  

-- EJERCICIO 7

data Hasktype2 = HInt | HChar | Fun Hasktype2 Hasktype2

tipoDato2 :: Parser Hasktype2
tipoDato2 = do string "Int"
               return HInt
            <|> 
            do string "Char" 
               return HChar

tiposHaskell2 :: Parser Hasktype2
tiposHaskell2 = do x <- tipoDato2
                   token (string "->")
                   xs <- tiposHaskell2
                   return (Fun x xs)
                <|> tipoDato2


-- EJERCICIO 8

-- CFG
-- expr → expr (’+’ term | ’-’ term) | term 
-- term → term (’*’ factor | ’/’ factor) | factor 
-- factor → digit | ’(’ expr ’)’
-- digit → ’0’|’1’|...|’9’

-- Transformaciones que le aplicamos a la gramatica para eliminar la recursion a izquierda:

-- newexpr → term newexpr'
-- newexpr' → ε | (’+’ term | '-' term) newexpr' 

-- newterm → factor newterm'
-- newterm' → ε | ('*' term | '/' term) newterm' 

-- Parser de la gramatica:

newterm :: Parser Expr
newterm = do f <- factor
             t <- newterm'
             return (t f)

newterm' :: Parser (Expr -> Expr)
newterm' = do char '*'
              f <- factor
              t <- newterm'
              return (t . (\ x -> BinOp Mul x f))
           <|>
           do char '/'
              f <- factor
              t <- newterm'
              return (t . (\ x -> BinOp Div x f))
           <|>
           return id

newexpr :: Parser Expr
newexpr = do t <- term  
             f <- newexpr' 
             return (f t) 

newexpr' :: Parser (Expr -> Expr)
newexpr' = do char '+'         
              t <- term        
              e <- newexpr'      
              return (e . (\ x -> BinOp Add x t))
           <|> do char '-'
                  t <- term
                  e <- newexpr'
                  return (e . (\x -> BinOp Min x t))
               <|> return id   

eval2 :: String -> Expr
eval2 xs = fst (head (parse newexpr xs))


-- EJERCICIO 9

-- CFG

-- declaration → type_specifier declarator ’;’
-- declarator → ’*’ declarator | direct_declarator
-- direct_declarator → direct_declarator ’[’ constant_expression ’]’ | ’(’ direct_declarator ’)’ | identifier
-- type_specifier → ’int’ | ’char’ | ’float’
-- constant_expression → number

{-
Dada la gramática: A → A α | β (donde α no es vacío y β no empieza con A)
La transformamos a: A → β A'
                    A' → ε | α A'
-}

-- Transformaciones que le aplicamos a la gramatica para eliminar la recursion a izquierda:

-- direct_declarator -> (’(’ direct_declarator ’)’ | identifier) direct_declarator'
-- direct_declarator' -> ε | ’[’ constant_expression ’] direct_declarator'

-- Tipos de datos con el que vamos a representar la gramatica:
data TypeC = Pointer TypeC | Array TypeC Int | Identifier String deriving Show
data BaseType = CInt | CChar | CFloat deriving Show

typeSpecifier :: Parser BaseType
typeSpecifier = do symbol "int"
                   return CInt
                <|>
                do symbol "char"
                   return CChar
                <|>
                do symbol "float"
                   return CFloat

declarator' :: Parser (TypeC -> TypeC)
declarator' = do symbol "["
                 x <- int
                 symbol "]"
                 y <- declarator'
                 return (\ e -> Array (y e) x)
              <|>
              return id

declarator :: Parser TypeC
declarator = do symbol "*"
                x <- declarator
                return (Pointer x)
             <|>
             do symbol "("
                y <- declarator
                symbol ")"
                z <- declarator'
                return (z y)
             <|>
             do func <- identifier
                w <- declarator' 
                return (w (Identifier func))


declaration :: Parser (BaseType, TypeC)
declaration = do x <- typeSpecifier
                 y <- declarator
                 symbol ";"
                 return (x, y)

tipoC :: String -> (BaseType, TypeC)
tipoC [] = error "input erroneo"
tipoC s = fst (head (parse declaration s))