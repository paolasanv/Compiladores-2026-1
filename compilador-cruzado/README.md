# Proyecto 02: Compilador cruzado

Proyecto realizado en Haskell que simula el comportamiento de un compilador cruzado. 
A partir de un mismo código fuente se genera el código dependiente para dos arquitecturas distintas.

### Integrantes:
- del Valle Vera Nancy Elena 
- Martínez Hidalgo Paola Mildred 
- Sánchez Victoria Leslie Paola


## Requisitos

- Stack >= 2.11.1
- GHC 9.6.4 

## Gramática

Las expresiones que pueden ser procesadas por este compilador cruzado son aquellas que puedan ser derivadas de la siguiente gramática.

S ::= _id_ := E 

E ::= E + T | E - T | T 

T ::= T * F | F 

F ::= ( E ) | -F | _num_ | _id_

Donde:

- _num_ representa números naturales.

- _id_ corresponde a identificadores formados por uno o más caracteres alfabéticos, ya sean mayúsculas o minúsculas.


## Uso

1. En la terminal, a la altura de compilador-cruzado/ compilar el proyecto con

```bash
stack build
```

2. Ejecutar con 

```bash
stack run
```

3. Para utilizar el compilador cruzado basta escribir _compilador [arquitectura] [cadena]_. 

Las arquitecturas dispobibles son _ATnT32_ y _ARM64_. 

Ejemplo:

```bash
===== Compilador cruzado :) ======

> compilador "ATnT" "a:=4*3-(-4+1)"

Lenguaje ensamblador AT&T de 32 bits 

Código fuente: a:=4*(-6+1)

Código objeto (simulado):
movl $0 %eax;
subl $6 %eax;
movl %eax %ebx;
addl $1 %ebx;
movl $4 %ecx;
imull %ebx %ecx;
movl %ecx %edx;
movl %edx a;


> compilador "ARM64" "a:=4*3-(-4+1)"

Lenguaje ensamblador ARM64

Código fuente: a:=4*(-6+1)

Código objeto (simulado):
 mov x0, #6
 neg x0, x0
 mov x2, #1
 add x1, x0, x2
 mov x3, #4
 mul x2, x3, x1
 str x2, =a


```


## Uso alternativo 


1. En la terminal, a la altura de compilador-cruzado/ compilar el proyecto con

```bash
stack build
```

2. Ejecutar con  GHCi 

```bash
stack ghci
```

3. Para utilizar el compilador cruzado se debe escribir _compilador [arquitectura] [cadena]_.

Donde _arquitectura_ puede ser ATnT32 o ARM64. 

La salida es una lista de intrucciones dependiente de la arquitectura elegida.


Ejemplo.

```bash
ghci> compilador ATnT32 "hola:=3-5*5"

Left [movl $5 %eax;,imull $5 %eax;,movl $3 %ebx;,subl %eax %ebx;,movl %ebx %ecx;,movl %ecx hola;]

ghci>  compilador ARM64 "hola:=3-5*5"

Right [ mov x0, \#5, mov x1, \#5, mul x0, x0, x1, mov x2, \#3, sub x1, x2, x0, str x1, =hola]


```
