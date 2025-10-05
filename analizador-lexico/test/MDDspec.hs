module MDDspec where

import Test.Hspec ( describe, it, shouldBe, Spec )
import MDD (lexerDo, TokenIMP (..)) 
import Automatas.DFA ( DFA(..) )
import qualified Data.Set as Set

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

-- AFD que reconoce listas con un solo dígito dentro, ej: "[7]"
dfaSingleDigitList :: DFA
dfaSingleDigitList = DFA {
    states = Set.fromList [0, 1, 2, 3],
    alphabet = Set.fromList ('[':']':['0'..'9']),
    transitions = Set.fromList $
        (0, '[', 1) :
        (2, ']', 3) :
        [ (1, c, 2) | c <- ['0'..'9'] ],
    start = 0,
    final = [3]
}

--AFD que reconoce los IDs 
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
  describe "Utilizando un AFD mínimo que solo acepta números enteros positivos" $ do
    it "reconoce '234' como un número" $ do
      lexerDo dfaPosInt "234" `shouldBe` [TNumber 234]

    it "no acepta el cero pero sí los demás números como '023'" $ do
      lexerDo dfaPosInt "023" `shouldBe` [TError "Caracter no reconocido: 0", TNumber 23]  

    it "no acepta caracteres no numéricos pero sí los demás números como en '12a3'" $ do
      lexerDo dfaPosInt "12a3" `shouldBe` [TNumber 12, TError "Caracter no reconocido: a", TNumber 3]

    it "reconoce números independientes separados por espacios como en '1 34 566 3'" $ do
      lexerDo dfaPosInt "1 34 566 3" `shouldBe` [TNumber 1, TNumber 34, TNumber 566, TNumber 3]

  describe "Utilizando un AFD mínimo que solo acepta identificadores alfabéticos en minúsculas" $ do
    it "reconoce 'hola' como identificador" $ do
      lexerDo dfaId "hola" `shouldBe` [TIdentifier "hola"]

    it "reconoce solo el identificador en minúsculas como en 'myID13'" $ do
      lexerDo dfaId "myID13" `shouldBe` [TIdentifier "my",TError "Caracter no reconocido: I", TError "Caracter no reconocido: D", TError "Caracter no reconocido: 1", TError "Caracter no reconocido: 3"]

    it "reconoce una sola letra como en 'a'" $ do
      lexerDo dfaId "a" `shouldBe` [TIdentifier "a"]
        
  describe "AFD para Números Enteros Negativos" $ do
     it "reconoce '-123' como un número negativo" $ do
       lexerDo dfaNegativeInt "-123" `shouldBe` [TNumber (-123)]

     it "no reconoce un número positivo como '45'" $ do
       lexerDo dfaNegativeInt "45" `shouldBe` [TError "Caracter no reconocido: 4", TError "Caracter no reconocido: 5"]

     it "no reconoce el cero '0'" $ do
       lexerDo dfaNegativeInt "0" `shouldBe` [TError "Caracter no reconocido: 0"]

  describe "AFD para el Símbolo de Asignación" $ do
     it "reconoce ':='" $ do
       lexerDo dfaAssignmentSymbol ":=" `shouldBe` [TSpecialSymbol ":="]
   
     it "no reconoce ':' por sí solo" $ do
       lexerDo dfaAssignmentSymbol ":" `shouldBe` [TError "Cadena no reconocida: :"]
     
     it "no reconoce otros símbolos como ';'" $ do
       lexerDo dfaAssignmentSymbol ";" `shouldBe` [TError "Caracter no reconocido: ;"]

  describe "AFD para Listas de un Solo Dígito" $ do
     it "reconoce '[8]'" $ do
       lexerDo dfaSingleDigitList "[8]" `shouldBe` [TListSymbol "[8]"]
   
     it "no reconoce una lista vacía '[]'" $ do
       lexerDo dfaSingleDigitList "[]" `shouldBe` [TError "Cadena no reconocida: []"]
   
     it "no reconoce una lista con más de un dígito como '[12]'" $ do
       lexerDo dfaSingleDigitList "[12]" `shouldBe` [TError "Cadena no reconocida: [12]"]
  
  describe "Utilizando un AFD para IDs*" $ do
    it "reconoce un identificador válido con solo letras como 'variable'" $ do
        lexerDo dfaIdentifier "variable" `shouldBe` [TIdentifier "variable"]

    it "reconoce un identificador válido con letras y números como 'valor99'" $ do
        lexerDo dfaIdentifier "valor99" `shouldBe` [TIdentifier "valor99"]

    it "no acepta cadenas que inician con un número como '9variable'" $ do
        lexerDo dfaIdentifier "9variable" `shouldBe` [TError "Caracter no reconocido: 9", TIdentifier "variable"]

    it "no acepta letras después de números como en 'id123a'" $ do
        lexerDo dfaIdentifier "id123a" `shouldBe` [TIdentifier "id123", TError "Caracter no reconocido: a"]

  describe "AFD para Lista Vacía" $ do
     it "reconoce '[]'" $ do
       lexerDo dfaEmptyList "[]" `shouldBe` [TListSymbol "[]"]
   
     it "no reconoce '[' por sí solo" $ do
       lexerDo dfaEmptyList "[" `shouldBe` [TError "Cadena no reconocida: ["]
   
     it "no reconoce un bracket de cierre suelto ']'" $ do
       lexerDo dfaEmptyList "]" `shouldBe` [TError "Caracter no reconocido: ]"]

--   describe "Utilizando el AFD mínimo que acepta los tokens del lenguaje IMP" 
