module Sintesis.RI where

import Analisis.Parser(AS)

type Registro = String

data InstTresDir = IAsig Registro String   -- t = a
                 | ISuma Registro String String -- t = a + b
                 | IResta Registro String String -- t = a - b
                 | IMult Registro String String -- t = a * b
                 deriving (Show, Eq)

representacionI :: AS -> [InstTresDir] 
representacionI = undefined

