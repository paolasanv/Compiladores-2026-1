module MDD where

data TokenIMP -- Definir los tokens de IMP

lexer :: String -> [TokenIMP]
lexer = undefined

--toMDD :: RegEx -> MDD
--toMDD r = toDFAmin $ toDFA $ toDFA $ toNFA $ toNFA_E r

