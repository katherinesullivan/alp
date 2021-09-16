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
    , identStart      = letter <|> char '_'
    , identLetter     = alphaNum <|> char '_'
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
          do y <- integer -- con que parser enteros???
             return (Const y)
          <|>
          do z <- identifier lis
             return (Var z)

intexp :: Parser (Exp Int)
intexp = parens lis intexp <|> intexp1

-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = undefined

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = undefined

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
