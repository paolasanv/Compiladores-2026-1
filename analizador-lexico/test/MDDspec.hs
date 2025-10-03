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

--   describe "Utilizando el AFD mínimo que acepta los tokens del lenguaje IMP" 