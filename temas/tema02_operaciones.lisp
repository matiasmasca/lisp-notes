;;; # Funciones Aritméticas Básicas

;; # SUMA
;; Si proporciona sólo un argumento número, esta función devuelve el resultado de sumarlo a cero. Ningún argumento, devuelve 0.
;; (+ [arg arg arg] ...)
(+ 1 2 3) ; $> 6
(+ 1.0 2 3) ; $> 6.0

;; # RESTA
;; devuelve el resultado de restar del primer número la suma de todos los números desde el segundo hasta el último.
;; (- [arg arg arg] ...)
(- 10 1 2 3) ; $> 4

(- 10 1 2.0 3) ; $> 4.0

;; # PRODUCTO
;; (* [arg arg arg] ...)
(* 1 2 3) ; $> 6
(* 1 2 3.0) ; $> 6.0 

;; # DIVISION 
;; devuelve como resultado un valor obtenido de la división del primer elemento con todos los demás.
;; (/ [arg arg arg] ...)
(/ 30 2 4) ; $> 3
(/ 30 2.0 4) ; $> 3.75

;;; ##############################
;;; Mas Funciones Aritméticas.
;; # Truncate
;; Return number (or number/divisor) as an integer, rounded toward 0. The second returned value is the remainder.
;; Te devuelve 2 valores la parte entera y el resto. 
;; Formato: (truncate <expr>)
(truncate 5.15)
(print (truncate 5.15)) ; $> 5

;; # Round
;; Redondea hacia el entero positiva mas cercano.
;; Formato: (round <expr>)
(round 5.15) ; $> 5
(print (round 5.95)) ; $> 6

;; # FLOAT
;; convierte un entero en un numero de coma flotante.
;; (float expr)
(float 8) ; $>8.0

;; # RATIONAL
;; convierte un numero real en racional.
;; (rational expr)
(rational 2.5) ; $> 5/2

;; # REM, MOD: 
;; devuelve el remanente de la división de dos números.
;; (rem expr expr)
(rem 7 2) ; $> 1

;; # ABS
;; devuelve el valor absoluto de una expresión.
;; (abs expr)
(abs -8) ; $> 8

;; # SIGNUM
;; permite obtener el signo de una expresión. Devuelve 0 para el 0, 1 para los positivos y -1 para los negativos.
;; (signum expr)
(signum -8) ; $> -1

;; # MAXIMO
;; Devuelve el mayor de una lista de numeros.
;; (MAX NUM NUM ...)
(MAX 2 5 3 1) ; $> 5

;; # MINIMO
;; Devuelve el menor de una lista de números.
;; (MIN NUM NUM ...)
(MIN 2 5 3 1) ; $> 1

;; # GCD: Máximo Común Divisor de una lista de números.
;; Si no recibe argumentos, devuelve 0. Si tiene un solo argumento, devuelve el mismo argumento.
;; (GCD NUM NUM ...)
(GCD 12 34 56) ; $> 2

;; # LCM: Mínimo Común Múltiplo de una lista de números.
;; Si no recibe argumentos, devuelve error. Si tiene un solo argumento, devuelve el mismo argumento. Si el resultado es mayor que el limite de los enteros, devuelve un error.
;;(LCM NUM NUM ...)
(LCM 12 34 56) ; $> 2856

;;; ##############################
;;; Funciones Matemáticas.

;; # RAIZ CUADRADA - SQRT
;; raíz cuadrada de un número.
;; (SQRT NUMBER)
(SQRT 16) ; $> 4.0
(SQRT 2) ; ==> 1.4142135
(SQRT -4) ; ==> #c(0.0 2)

;; # EXPONENCIAL - EXPT
;; Calcula el valor de un número elevado a otro número.
;; (EXPT NUMBER NUMBER)
(EXPT 2 3) ; ==> 8
(EXPT 3 -2) ; ==> 0,11111111
(EXPT 3 0) ; ==> 1

;; ### DESTRUCTIVAS !

;; # INCREMENTO - INCF
;; incrementa en una cantidad DELTA un valor Var, por defecto es 1.
;; (INCF Var [DELTA])
;;  Var debe ser una variable. Suponemos que C vale 5, entonces
(defvar C  '5)
(INCF C 10) ; ==> 15
C ; ==> 15
(INCF C) ; ==> 16
C ; ==> 16

;; Mientras que si asumo que el valor de C es 5, y
(- 1 C) ; ==> 4
C ; ==> 5

;; # DECREMENTO - DECF
;; decrementa en una cantidad DELTA, una variable Var, por defecto es 1.
;; (DECF Var [DELTA])
;;  Var debe ser una variable. Suponemos que C vale 5, entonces
(defvar C  '5)
(DECF C 10) ; ==> -5
C ; ==> -5
(DECF C) ; ==> -6
C ; ==> -6


;; #############################
;; COMPARACIONES DE NUMEROS
;; Predicados de comparación de números.

;; Los predicados, son FUNCIONES booleanas, que devuelven solo los valores T o NIL.
;; # IGUALDAD
;; (= NUM NUM ...)
(= 3 3.0) ; ==> T

;; # NO IGUAL
;; (/= NUM NUM...)
(/= 3 3.0) ; ==> NIL

;; # MENOR QUE, secuencia estrictamente creciente de números.
;; (< NUM NUM ...)
(< 3 5 8) ; ==> T
(< 3 5 4) ; ==> NIL

;; # MAYOR QUE, secuencia estrictamente decreciente de números.
;; (> NUM NUM ...)
(> 8 5 3) ; ==> T 
(< 8 3 5) ; ==> NIL

;; # MENOR O IGUAL QUE, secuencia NO estrictamente creciente de números.
;; (<= NUM NUM ...)
(<= 5 5 8) ; ==> T
(<= 9 9 4) ; ==> NIL

;; # MAYOR O IGUAL QUE, secuencia NO estrictamente decreciente de números.
;; (>= NUM NUM ...)
(>= 9 7 7) ; ==> T
(>= 8 9 8) ; ==> NIL

;;; ###################################
;;; PREDICADOS NUMERICOS
;; Son predicados que se utilian para verificar exclusivamente argumentos numéricos. Aceptan 1 solo argumento.

;; # NUMBERP - ES NUMERO?
;; verifica si el tipo de objeto es numérico. Devuelve true si el objeto es un número. El argumento puede ser de cualquier tipo.
;; (NUMBERP OBJETO)
(NUMBERP 7) ; ==> T
(NUMBERP 'NOMBRE) ; ==> NIL

;; #  EVENP - EL ENTERO ES PAR?
;; verifica un argumento, que debe ser entero, y devuelve cierto si el entero es par.
;;(EVENP ENTERO)
(EVENP 8) ; ==> T
(EVENP 5) ; ==> nil
(EVENP 'NOMBRE) ; ==> ERROR is not an integer

;; # ODDP - EL ENTERO ES IMPAR?
;; verifica un argumento, que debe ser entero, y devuelve cierto si el entero es impar.
;;(ODDP ENTERO)
(ODDP -7) ; ==> T
(ODDP 4) ; ==> NIL
(ODDP 5.8) ; ==> Error: 5.8 is not an integer


;; # INTEGERP - ES ENTERO?
;; verifica si el argumento es un munero entero. Devuelve true si el argumento es un entero. El argumento puede ser de cualquier tipo.
;; (INTEGERP OBJETO)
(INTEGERP 7) ; ==> T
(INTEGERP ‘NOMBRE) ; ==> NIL

;; # ZEROP
;; verifica un argumento, que debe ser numérico, y devuelve cierto si el número es cero.
;; (ZEROP NUMBER)
(ZEROP 0.0) ; ==> T
(ZEROP 'NOMBRE) ; ==> ERROR

;; #########################################
;; # EJEMPLOS
;; # ATOMOS
(+ 1 -2 3.5) ; ==> 2.5
(- 1 '-2 3.5) ; ==> -0.5
(* 1 '-2 3.5) ; ==> -7.0
(/ 1 -2 3.5) ; ==> -0.14285714285714285

;; # Listas de IGUAL LONGITUD en el 1er. nivel
(+ '(1 -2 3.5) '(4 5 6)) ; ==>  (5 3 9.5)
(+ '(1 -2 3.5) '(4 5 6) '((2 4) (3) (((5)2)))) ; ==> ((7 9) (6) (((14.5) 11.5)))
(- '(1 -2 3.5) '(4 5 6)) ; ==> (-3 -7 -2.5)
(- '(1 2 3.5) '(4 5 6) '((2 4) (3) (((5)2)))) ; ==> ((-5 -7) (-10) (((-7.5) -4.5)))
(* '(1 -2 3.5) '(4 5 6)) ; ==> (4 -10 21.0)
(* '(1 2 3.5) '(4 5 6) '((2 4) (3) (((5)2)))) ; ==> ((8 16) (30) (((105.0) 42.0)))

;; # Listas de DISTITNA longitud  en el 1er. nivel
(/ '(1 -2 3.5) '(4 5 6)) ; ==> (0.25 -0.4 0.583)
(+ '(1 2 3) '(4 5) ); ==> Error: arguments not all the same length


;; # Átomos y Listas
(+ 1 '(2 -3 4) 5) ; ==> (8 3 10)
(+ 1 '(2 -3 4) 5 '(4 7) ) ; ==> Error: arguments not all the same length
(+ 1 '(2 -3 4) 5 '(4 (7) ((2)))) ; ==> (12 (10) ((12)))
(- 1 '(2 -3 4) 5) ; ==> (-6 -1 -8)
(- 1 '(2 -3 4) 5 '(4 (7) ((2)))) ; ==> (-10 (-8) ((-10)))
(* 1 '(2 -3 4) 5) ; ==> (10 -15 20)
(* 1 '(2 -3 4) 5 '(4 (7) ((2)))) ; ==> (40 (-105) ((40)))
(/ 1 '(2 -3 4) 5) ; ==> (0.1 -0.066 0.05)
