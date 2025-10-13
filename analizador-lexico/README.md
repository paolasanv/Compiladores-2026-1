# Proyecto 01: Analizador Léxico

Proyecto realizado en Haskell que implementa una de las fases del compilador: el analizador léxico.

## Equipo: Haskellcatl
### Integrantes:
- Arias Villarroel Alejandra Valentina 
- del Valle Vera Nancy Elena 
- Martínez Hidalgo Paola Mildred 
- Martínez Mejía Eduardo 
- Sánchez Victoria Leslie Paola


## Requisitos

- Stack >= 2.11.1
- GHC 9.6.4 

## Compilación & Ejecución

```bash
stack build
stack run
```

## Uso

1. Se proporciona un archivo de texto llamado _regex.txt_ que incluye por defecto las expresiones regulares de IMP. En caso de que se desee realizar modificaciones, se recomienda consultar el documento [specs/IMP.md](specs/IMP.md) 
 para asegurar una edición correcta.

2. En la terminal, a la altura de analizador-lexico/ escribir:

  ```bash
  stack build
  stack run
  ```

3. Llamar a la función _lexer_ con una cadena (con o sin comillas). La primera vez que se use _lexer_ con una nueva expresión regular el programa puede tardar unos minutos para entregar el resultado. Ejemplo: 

  ```bash
  $ stack build
  $ stack run
  Analizador Léxico :) 
  [Actualización] Expresión regular actualizada.
  > lexer 3 + 4                   -- Aquí puede tardar unos minutos
  [TNum 3, TAdd, TNum 4] 
  > lexer "3 - 5"                 -- Aquí no 
  [TNum 3, TMinus, TNum 5]
  ```

4. Sin tener que compilar el programa nuevamente, es posible actualizar el archivo de texto _regex.txt_ y usar _lexer_ con las nuevas expresiones regulares especificadas. 

  **Importante: Para hacer uso de esta funcionalidad, se debe editar regex.txt desde su editor de texto favorito. Evitar el bloc de notas ya que los cambios podrían no verse reflejados adecuadamente en la terminal.**

  ```bash
  $ stack build
  $ stack run
  Analizador Léxico :) 
  [Actualización] Expresión regular actualizada.
  > lexer 3 + 4
  [TNum 3, TAdd, TNum 4]

  Cambio detectado en regex.txt
  [Actualización] Expresión regular actualizada.
  > lexer 3 + 4
  [TError, TError, TError]
  ```

5. Para salir del programa puede usar **:q** o **Ctrl + C**.

  ```bash
  $ stack build
  $ stack run
  Analizador Léxico :) 
  [Actualización] Expresión regular actualizada.
  > lexer 3 + 4
  [TNum 3, TAdd, TNum 4]

  Cambio detectado en regex.txt
  [Actualización] Expresión regular actualizada.
  > lexer 3 + 4
  [TError, TError, TError]
  >:q
  Chao ;)
  ```
## Uso alternativo 

Es posible utilizar ghci para probar el programa con la expresión regular completa de IMP. En este contexto, las modificaciones hechas en el archivo de texto no influyen en el uso del analizador léxico (en este caso, la función se llama _lexerIMP_).

Ejemplo:

  ```bash
  $ stack ghci
  ghci> lexerIMP "2+3"
  [TNum 2, TPlus, TNum 3]
  ```

## Pruebas 

Se proveen casos de prueba dentro de analizador-lexico/test. Para verlas escribir:

  ```bash
  $ stack test
  ```