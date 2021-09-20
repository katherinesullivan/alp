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
                        , "," -- van aca agregados los corchetes para parsearlos?? NO
                        ]
    -- , identStart      = letter <|> char '_'
    -- , identLetter     = alphaNum <|> char '_'
    }
  )
-- Hay que completar todas las definiciones que faltan dentro del lis?
-- Que es el opLetter y opStart?


----------------------------------
--- Parser de expressiones enteras
-----------------------------------
intexp1 :: Parser (Exp Int)
intexp1 = try chainl1 intexp (do{ reservedOp lis ","; return ESeq })
          <|>
          try ( do y <- identifier lis
                   reservedOp lis "="
                   x <- intexp
                   return (EAssgn y x) ) -- entonces cada vez que uso el operador <|> de Parsec, el primer parser tiene que ir con un try???
          <|>
          try chainl1 intexp ( do{ reservedOp lis "+"; return Plus} <|> do{ reservedOp lis "-"; return Minus} )
          <|>
          try chainl1 intexp ( do{ reservedOp lis "*"; return Times} <|> do{ reservedOp lis "/"; return Div} )
          <|>
          do reservedOp lis "-"
             e <- intexp
             return (UMinus e)
          <|>
          do y <- natural
             return (Const y)
          <|>
          do z <- identifier lis
             return (Var z)

intexp :: Parser (Exp Int)
intexp = parens lis intexp <|> intexp1

-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

boolexp1 :: Parser (Exp Bool)
boolexp1 = try chainl1 boolexp (do{ reservedOp lis "||"; return Or })
           <|>
           try chainl1 boolexp (do{ reservedOp lis "&&"; return And })
           <|>
           try ( do reservedOp lis "!"
                    x <- boolexp
                    return (Not x) )
           <|>
           chainl1 intexp ( try (do{ reservedOp lis "=="; return Eq })
                             <|>
                             try (do{ reservedOp lis "!="; return NEq })
                             <|>
                             try (do{ reservedOp lis "<"; return Lt })
                             <|> 
                             try (do{ reservedOp lis ">"; return Gt }) )
           <|>
           do reserved lis "true"
              return BTrue
           <|>
           do reserved lis "false"
              return BFalse

boolexp :: Parser (Exp Bool)
boolexp = parens lis boolexp <|> boolexp1

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = try chainl1 comm (do{ reservedOp lis ";"; return Seq })
       <|>
       try ( do reserved lis "repeat"
                x <- comm
                reserved lis "until"
                b <- boolexp
                reserved lis "end"
                return (Repeat x b) )
       <|>
       try ( do reserved lis "if"
                b2 <- boolexp
                y <- braces lis comm
                (try (do reseved lis "else"
                         y2 <- braces lis comm
                         return IfThenElse b2 y y2)
                 <|>
                 return IfThen b2 y) ) -- pattern me define un sinonimo de tipo o que es?
       <|> 
       try ( do v <- identifier lis
                reservedOp lis "="
                e <- intexp
                return (Let v e) )
       <|>
       do reserved lis "skip"
          return Skip

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
