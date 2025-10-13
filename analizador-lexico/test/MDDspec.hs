{-|
Module      : MDDspec
Description : Casos de prueba
Author: Leslie P. Sánchez V. y Alejandra Valentina Arias Villarroel

Este módulo pone a prueba la implementación de la máquina discriminadora determinista y el lexer de IMP.
-}

module MDDspec where

import Test.Hspec ( describe, it, shouldBe, Spec )
import MDD (lexerDo, TokenIMP (..)) 
import Automatas.DFA ( DFA(..) )
import qualified Data.Set as Set
import MDD (lexerIMP)


-- AFD que reconoce enteros positivos 
dfaPosInt :: DFA
dfaPosInt = DFA {
   states = Set.fromList [0, 1],
   alphabet = Set.fromList ['0'..'9'],
   transitions = Set.fromList $
       [ (0, c, 1) | c <- ['1'..'9'] ] ++
       [ (1, c, 1) | c <- ['0'..'9'] ],
   start = 0,
   final = [1]
}

-- AFD que acepta todas las cadenas no vacías de letras minúsculas 
dfaId :: DFA
dfaId = DFA {
   states = Set.fromList [0, 1],
   alphabet = Set.fromList ['a'..'z'],
   transitions = Set.fromList $
       [ (0, c, 1) | c <- ['a'..'z'] ] ++
       [ (1, c, 1) | c <- ['a'..'z'] ],
   start = 0,
   final = [1]
}

-- AFD que reconoce únicamente enteros negativos
dfaNegativeInt :: DFA
dfaNegativeInt = DFA {
    states = Set.fromList [0, 1, 2],
    alphabet = Set.fromList ('-':['0'..'9']),
    transitions = Set.fromList $
        (0, '-', 1) :
        [ (1, c, 2) | c <- ['1'..'9'] ] ++
        [ (2, c, 2) | c <- ['0'..'9'] ],
    start = 0,
    final = [2]
}

-- AFD que reconoce únicamente el símbolo de asignación ':='
dfaAssignmentSymbol :: DFA
dfaAssignmentSymbol = DFA {
    states = Set.fromList [0, 1, 2],
    alphabet = Set.fromList ":=",
    transitions = Set.fromList [
        (0, ':', 1),
        (1, '=', 2)
    ],
    start = 0,
    final = [2]
}

--AFD que reconoce los identificadores
dfaIdentifier :: DFA
dfaIdentifier = DFA {
    states = Set.fromList [0, 1, 2],
    alphabet = Set.fromList (['a'..'z'] ++ ['A'..'Z'] ++ ['1'..'9']),
    transitions = Set.fromList $
        [ (0, c, 1) | c <- ['a'..'z'] ++ ['A'..'Z'] ] ++
        [ (1, c, 1) | c <- ['a'..'z'] ++ ['A'..'Z'] ] ++
        [ (1, c, 2) | c <- ['1'..'9'] ] ++
        [ (2, c, 2) | c <- ['1'..'9'] ],
    start = 0,
    final = [1, 2]
}

-- AFD que reconoce exclusivamente la lista vacía "[]"
dfaEmptyList :: DFA
dfaEmptyList = DFA {
    states = Set.fromList [0, 1, 2],
    alphabet = Set.fromList "[]",
    transitions = Set.fromList [
        (0, '[', 1),
        (1, ']', 2)
    ],
    start = 0,
    final = [2]
}

spec :: Spec
spec = do
  describe "Expresión regular: (1|2|3|4|5|6|7|8|9)(0|1|2|3|4|5|6|7|8|9)*" $ do
    it "reconoce '234' como un número" $ do
      lexerDo dfaPosInt "234" `shouldBe` [TNumber 234]

    it "no acepta el cero pero sí los demás números como '023'" $ do
      lexerDo dfaPosInt "023" `shouldBe` [TError "Caracter no reconocido: 0", TNumber 23]  

    it "no acepta caracteres no numéricos pero sí los demás números como en '12a3'" $ do
      lexerDo dfaPosInt "12a3" `shouldBe` [TNumber 12, TError "Caracter no reconocido: a", TNumber 3]

    it "reconoce números independientes separados por espacios como en '1 34 566 3'" $ do
      lexerDo dfaPosInt "1 34 566 3" `shouldBe` [TNumber 1, TNumber 34, TNumber 566, TNumber 3]

  describe "Expresión regular: (a|b|...|z)(a|b|...|z)*" $ do
    it "reconoce 'hola' como identificador" $ do
      lexerDo dfaId "hola" `shouldBe` [TIdentifier "hola"]

    it "reconoce solo el identificador en minúsculas como en 'myID13'" $ do
      lexerDo dfaId "myID13" `shouldBe` [TIdentifier "my",TError "Caracter no reconocido: I", TError "Caracter no reconocido: D", TError "Caracter no reconocido: 1", TError "Caracter no reconocido: 3"]

    it "reconoce una sola letra como en 'a'" $ do
      lexerDo dfaId "a" `shouldBe` [TIdentifier "a"]
        
  describe "Expresión regular: -((1|2|3|4|5|6|7|8|9)(0|1|2|3|4|5|6|7|8|9)*)" $ do
     it "reconoce '-123' como un número negativo" $ do
       lexerDo dfaNegativeInt "-123" `shouldBe` [TNumber (-123)]

     it "no reconoce un número positivo como '45'" $ do
       lexerDo dfaNegativeInt "45" `shouldBe` [TError "Caracter no reconocido: 4", TError "Caracter no reconocido: 5"]

     it "no reconoce el cero '0'" $ do
       lexerDo dfaNegativeInt "0" `shouldBe` [TError "Caracter no reconocido: 0"]

  describe "Expresión regular: := " $ do
     it "reconoce ':='" $ do
       lexerDo dfaAssignmentSymbol ":=" `shouldBe` [TAssign]
   
     it "no reconoce ':' por sí solo" $ do
       lexerDo dfaAssignmentSymbol ":" `shouldBe` [TError "Caracter no reconocido: :"]
     
     it "no reconoce otros símbolos como ';'" $ do
       lexerDo dfaAssignmentSymbol ";" `shouldBe` [TError "Caracter no reconocido: ;"]

  describe "Expresión regular: ([a-zA-Z])([a-zA-Z0-9])*" $ do
    it "reconoce un identificador válido con solo letras como 'variable'" $ do
        lexerDo dfaIdentifier "variable" `shouldBe` [TIdentifier "variable"]

    it "reconoce un identificador válido con letras y números como 'valor99'" $ do
        lexerDo dfaIdentifier "valor99" `shouldBe` [TIdentifier "valor99"]

    it "no acepta cadenas que inician con un número como '9variable'" $ do
        lexerDo dfaIdentifier "9variable" `shouldBe` [TError "Caracter no reconocido: 9", TIdentifier "variable"]

    it "se toma como otro identificador si hay letras después de números como en 'id123a'" $ do
        lexerDo dfaIdentifier "id123a" `shouldBe` [TIdentifier "id123", TIdentifier "a"]

  describe "Expresión regular: \\[\\]" $ do
     it "reconoce '[]'" $ do
       lexerDo dfaEmptyList "[]" `shouldBe` [TList]
   
     it "no reconoce '[' por sí solo" $ do
       lexerDo dfaEmptyList "[" `shouldBe` [TError "Caracter no reconocido: ["]
   
     it "no reconoce un bracket de cierre suelto ']'" $ do
       lexerDo dfaEmptyList "]" `shouldBe` [TError "Caracter no reconocido: ]"]

  describe "Expresión regular: unión de las categorías léxicas de IMP" $ do
      it "reconoce listas como 1:2:[]" $ do
        lexerIMP "1:2:[]" `shouldBe` [TNumber 1, TCons, TNumber 2, TCons, TList]

      it "reconoce comandos como if x:=4 then true else false" $ do
        lexerIMP "if x:=4 then true else false" `shouldBe` [TIf, TIdentifier "x", TAssign, TNumber 4, TThen, TTrue, TElse, TFalse]      

      it "reconoce comandos como skip ; id4 := -45" $ do
        lexerIMP "skip ; id4 := -45" `shouldBe` [TSkip, TDotAndComa, TIdentifier "id4", TAssign, TNumber (-45)]      

