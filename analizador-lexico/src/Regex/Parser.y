{
module Regex.Parser where
import Regex.Lexer
import Data.List

}

%name parser
%tokentype { Token }
%error { parseError }

%token
    empty       { EmptyS }
    epsilon     { EpsilonS }
    character   { CharacterS $$ }
    union       { UnionS }
    concat      { ConcatS }
    kleene      { KleeneS }
    '('         { OpenParenthesis }
    ')'         { CloseParenthesis }

%%

-- Símbolo start
RegEx :: { RegEx }
RegEx : R              { $1 }

-- Unión: R → U | R '+' U
R :: { RegEx }
R : U                  { $1 }
  | R union U          { Union $1 $3 }

-- Concatenación: U → C | U '.' C
U :: { RegEx }
U : C                  { $1 }
  | U concat C         { Concat $1 $3 }

-- Kleene: C → A | A '*'
C :: { RegEx }
C : A                  { $1 }
  | A kleene           { Kleene $1 }

-- Elementos básicos
A :: { RegEx }
A : empty              { Empty }
  | epsilon            { Epsilon }
  | character          { Character $1 }
  | '(' R ')'          { $2 }

{
parseError :: [Token] -> a
parseError _ = error "¡Error al especificar las expresiones regulares!"

data RegEx
    = Empty 
    | Epsilon
    | Character Char
    | Union RegEx RegEx
    | Concat RegEx RegEx
    | Kleene RegEx
    --deriving Show

instance Show RegEx where
	show Empty = "∅"
	show Epsilon = "ε"
	show (Character a) = [a]
	show (Union e1 e2) = "(" ++ show e1 ++ " | " ++ show e2 ++ ")"
	show (Concat e1 e2) = show e1  ++ show e2 
	show (Kleene e) =  "(" ++ show e ++ ")*"

}
