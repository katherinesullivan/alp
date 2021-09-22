module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST
import GHC.Float (integerLogBase)
import GHC.Err (undefined)

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do whiteSpace lis
                 t <- p
                 eof
                 return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "if", "else", "repeat", "skip", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ]
    }
  )


----------------------------------
--- Parser de expressiones enteras
-----------------------------------
intexp :: Parser (Exp Int)
intexp = try (chainl1 intexp2 (do{ reservedOp lis ","; return ESeq }))

intexp2 :: Parser (Exp Int)
intexp2 = try (chainl1 intexp3 ( do{ reservedOp lis "+"; return Plus} <|> do{ reservedOp lis "-"; return Minus} ))

intexp3 :: Parser (Exp Int)
intexp3 = try (chainl1 intatom ( do{ reservedOp lis "*"; return Times} <|> do{ reservedOp lis "/"; return Div} ))

intatom :: Parser (Exp Int)
intatom = do x <- natural lis
             return (Const (fromIntegral x :: Int))
          <|>
          do x <- identifier lis
             return (Var x)
          <|>
          do reservedOp lis "-"
             e <- intexp
             return (UMinus e)
          <|>     
          try ( do x <- identifier lis
                   reservedOp lis "="
                   y <- intexp
                   return (EAssgn x y) )
          <|> parens lis intexp


          
-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = try (chainl1 boolexpand (do{ reservedOp lis "||"; return Or }))

boolexpand :: Parser (Exp Bool)
boolexpand = try (chainl1 boolatom (do{ reservedOp lis "&&"; return And }))

boolatom :: Parser (Exp Bool)
boolatom = do reserved lis "true"
              return BTrue
           <|>
           do reserved lis "false"
              return BFalse
           <|>
           do reservedOp lis "!"
              x <- boolexp
              return (Not x)
           <|>
           do x <- intexp 
              ( try (do{ reservedOp lis "=="; y <- intexp; return (Eq x y) })
                <|>
                try (do{ reservedOp lis "!="; y <- intexp; return (NEq x y) })
                <|>
                try (do{ reservedOp lis "<"; y <- intexp; return (Lt x y) })
                <|> 
                try (do{ reservedOp lis ">"; y <- intexp; return (Gt x y) }) )
           <|>
           parens lis boolexp

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = chainl1 comm2 (do{ reservedOp lis ";"; return Seq })
       
comm2 :: Parser Comm
comm2 = try ( do reserved lis "skip"
                 return Skip )
        <|> 
        try ( do v <- identifier lis
                 reservedOp lis "="
                 e <- intexp
                 return (Let v e) ) 
        <|>
        try ( do reserved lis "repeat"
                 x <- braces lis comm -- agregamos lector de llaves
                 reserved lis "until"
                 b <- boolexp
               --   reserved lis "end"
                 return (Repeat x b) )
        <|>
        try ( do reserved lis "if"
                 b2 <- boolexp
                 y <- braces lis comm
                 ( try (do reserved lis "else"
                           y2 <- braces lis comm
                           return (IfThenElse b2 y y2))
                   <|>
                   return (IfThen b2 y)) )
        
        

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
