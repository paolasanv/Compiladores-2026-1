{
{-|
Module      : Analisis.Parser
Description : Analizador sintáctico simple.

Este módulo implementa un analizador sintáctico simple utilizando Happy.
-}
module Analisis.Parser where

import Analisis.Lexer  
}

%name parser
%tokentype { Token }
%error { parseError }

%token
    ident   { TIdent $$ } 
    num     { TNum $$ }
    '+'     { TSuma }
    '-'     { TResta }
    '*'     { TMult }
    '('     { TParenAbierto }
    ')'     { TParenCerrado }
    Asigna  { TAsiga }
%%

-- Definición de la gramática
S : E {$1}
    | ident Asigna E  {Asigna $1 $3}

E : E '+' E     {Suma $1 $3}
    | E '-' E   {Resta $1 $3}
    | T         {$1}

T : T '*' T     {Mult $1 $3}
    | F         {$1}

F : '(' E ')'   {$2}
    | num       {Num $1}
    | ident     {Ident $1}

{
    
parseError :: [Token] -> a
parseError _ = error "Parse error!"

data AS = 
      Asigna String AS
    | Suma AS AS
    | Resta AS AS
    | Mult AS AS
    | Num Int
    | Ident String
    deriving (Show)
}