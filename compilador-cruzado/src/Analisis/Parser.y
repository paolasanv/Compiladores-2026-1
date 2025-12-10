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
    Asigna  { TAsigna }
%%

-- Definición de la gramática
S : ident Asigna E  {Asigna $1 $3}

E : E '+' T     {Suma $1 $3}
    | E '-' T   {Resta $1 $3}
    | T         {$1}

T : T '*' F     {Mult $1 $3}
    | F         {$1}

F : '(' E ')'   {$2}
    | num       {Num $1}
    | ident     {Ident $1}
    | '-' F     {Uminus $2}

{
    
parseError :: [Token] -> a
parseError _ = error "Parse error!"

data AS =  Asigna String AS
    | Suma AS AS
    | Resta AS AS
    | Mult AS AS
    | Num Int
    | Uminus AS
    | Ident String
    deriving (Show)
}