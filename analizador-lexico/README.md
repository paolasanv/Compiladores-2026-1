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

3. Llamar a la función _lexer_ con una cadena SIN comillas. Ejemplo: 

  ```bash
  $ stack build
  $ stack run
  Analizador Léxico :) 
  > Observando regex.txt por cambios... 
  > lexer 3 + 4
  [TNum 3, TAdd, TNum 4]
  ```

4. Sin tener que compilar el programa nuevamente, es posible actualizar el archivo de texto _regex.txt_ y usar _lexer_ con las nuevas expresiones regulares especificadas. 

  **Importante: Para hacer uso de esta funcionalidad, se debe editar regex.txt desde su editor de texto favorito. Evitar el bloc de notas ya que los cambios podrían no verse reflejados adecuadamente en la terminal.**

  ```bash
  $ stack build
  $ stack run
  Analizador Léxico :) 
  > Observando regex.txt por cambios... 
  > lexer 3 + 4
  [TNum 3, TAdd, TNum 4]
  > Archivo de texto actualizado 
  > Observando regex.txt por cambios... 
  > lexer 3 + 4
  [TError, TError, TError]
  ```

5. Para salir del programa puede usar **:q** o **Ctrl + C**.

    ```bash
  $ stack build
  $ stack run
  Analizador Léxico :) 
  > Observando regex.txt por cambios... 
  > lexer 3 + 4
  [TNum 3, TAdd, TNum 4]
  > Archivo de texto actualizado 
  > Observando regex.txt por cambios... 
  > lexer 3 + 4
  [TError, TError, TError]
  >:q
  Chao ;)
  ```

## Pruebas 

Se proveen casos de prueba dentro de analizador-lexico/test