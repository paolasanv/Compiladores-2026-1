# Especificaciones para modificar el archivo de texto

Este programa utiliza un archivo de texto llamado _regex.txt_, el cual contiene una lista de expresiones regulares utilizadas para construir una función lexer.

Las expresiones deben seguir una notación específica, basada en su definición teórica. A continuación se describen las reglas de escritura y los símbolos utilizados.


## Formato de las expresiones regulares

Cada línea en regex.txt representa una expresión regular. Todas las expresiones listadas serán unidas mediante el operador de unión (|), es decir, el contenido del archivo define la expresión

```bash
r₁ | r₂ | r₃ | ... | rₙ

```


## Reglas de sintaxis 

1. Vacío (∅):

	Se representa por una línea en blanco (salto de línea). Ejemplo:

	```bash


	```


2. Cadena vacía (ε):

	Representada por un los símbolos '?e'. Ejemplo:

	```bash
	'?e'
	```

	Esta representación funciona para cualquier sistema operativo, en el caso usar Linux tambien puede usar el símbolo unicode ε como como la cadena vacía. Ejemplo: 

	```bash
	(?e + ε)
	```


3. Operadores válidos:

	- Concatenación: R.S

	- Unión: R | S

	- Agrupación: ( R )

	- Cerradura de Kleene: R*

## Ejemplo

Si el archivo _regex.txt_ contiene las siguientes líneas:

```bash
A*
1
a | b | c
f.o.r
(a | ε).c
```

Se interpretará como la expresión regular compuesta:

```bash
A* | 1 | a | b | c | f.o.r | (a | ε).c
```

Que equivale a:

```bash
A* + 1 + a + b + c + for + (a + ε)c
```
