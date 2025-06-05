;;;; ### TEMA 4: ESTRUCTURAS DE CONTROL DEF LUJO Y RECURSIVIDAD.
; Secuencia de Acciones. Estructuras condicionales. Estructuras iterativas. Com unicación con elusuario: entrada/salida. Recursividad. Definición de procedim ientos que se usan a sí mismos. Recursividad de cola, eficiencia. Procedimientos iterativos. Aplicación explícita y reiterada de una función a todos los datos contenidos en listas. Destructividad/N o destructividad. Lista de funciones.

; ###############################################################
; ### 4.1 SECUENCIA DE ACCIONES

; Se llama BLOQUE a una secuencia de sentencias que deben ser ejecutadas secuencialmente en el orden en el que aparecen.
; Estos bloques se pueden crear de manera implicita como en loop y return o de forma explicita como en "prog".

; ## PROG-N
; ( progn [ < sentencia 1 > ... < sentencia n> ] )
; permite crear un bloque de sentencias que serán evaluadas secuencialmente. Se toma el valor devuelto por la ULTIMA de ellas como valor de retorno de la forma progn completa.
; Proporciona una forma de agrupar una serie de expresiones LISP. Cada una corresponde con una forma determinada y serán evaluadas en orden.
; Sintaxis: PROGN {((VAR {INIT})/ VAR*)} DECLARACIONES CUERPO).
; * El valor devuelto por PROGN corresponde al de la última forma evaluada.
(progn (setq a 23) (setq b 35) (setq c (* a b))) ; ==> 805 ; la N devuelve el ulitmo

(prog1 (setq a 23) (setq b 35) (setq c (* a b))) ; ==> 23 ; el 1 en "prog1" devuelve el primera forma
(prog2 (setq a 23) (setq b 35) (setq c (* a b))) ; ==> 35


; * PROG1 y PROG2, devuelven el valor de la primera y segunda forma respectivamente, mientras se evalúan el resto de las formas.
; Si en la declaración de un PROGN se incluyen variables locales, la asignación de sus valores se hace en paralelo
