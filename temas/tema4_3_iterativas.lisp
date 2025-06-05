;;;; ### TEMA 4: ESTRUCTURAS DE CONTROL DEF LUJO Y RECURSIVIDAD.
; Secuencia de Acciones. Estructuras condicionales. Estructuras iterativas. Com unicación con elusuario: entrada/salida. Recursividad. Definición de procedimientos que se usan a sí mismos. Recursividad de cola, eficiencia. Procedimientos iterativos. Aplicación explícita y reiterada de una función a todos los datos contenidos en listas. Destructividad/N o destructividad. Lista de funciones.

; ###############################################################
; ### 4.3 Estructuras ITERATIVAS
; - LOOP
; - DO
; - DOTIMES
; - DOLIST
; - MAPCAR
; - MAPLIST

; ## LOOP - ITERACIÓN INDEFINIDA
; (LOOP {FORMA}*)
; Expresa una construcción iterativa que cicla continuamente, a menos que se le indique explícitamente que pare, evaluando las formas en secuencia. La forma de indicar la terminación del ciclo es mediante RETURN. En general la evaluación de la sentencia RETURN provocará la salida de cualquier sentencia iterativa. 
(setq x 1)
(loop
    (print x)
    (setq x (+ x 1))
    (if (= x 20) (return))
) ; end loop
; ==> Imprime del 1 al 19 y sale con un NIL.

; ## DO - HACER
; DO ( {PARAM}* / {(PARAM VALOR)}* / {(PARAM VALOR INCRE)}*)
;    (TEST-EVALUACION {FORMAS}*) {FORMAS}*
; La estructura DO tiene tres partes: lista de parámetros, test de final y cuerpo. La lista de parámetros liga las variables con sus valores iniciales y en cada ciclo se asignan nuevos valores dependiendo de la función de incremento indicada. En la estructura DO, primero se activa la ligadura de los parámetros y después se evalúa el test de final, y si no se cumple se ejecuta el cuerpo.
(do ((var1 inic1 [paso1])...
     (var2 init2 step2) 
     (varn inicn [pason])) ; Asignación de variables en paralelo
     (test-final resultado) ; Test-final se evalúa antes que cuerpo
      declaraciones 
        cuerpo)

; Función exponente mn
(setq m 3 n 4)
(do ((resultado 1) (exponente n)) ; asignación de parámetros, asigna 1 a resultado y n a exponente
    ((zerop exponente) resultado) ; test de final
                                  ; si exponente es 0, muestra resultado,
                                  ; sino sigue con la próxima sentencia.
    (setq resultado (* m resultado)) ; asigna el producto a resultado
    (setq exponente (- exponente 1)) ) ; resta 1 a exponente, vuelve al test.
; ==> 81
(do ((resultado 1 (* m resultado))
    (exponente n (1- exponente)))
    ((zerop exponente) resultado) ) ; Otra forma con el test al final.
; ==> 81

; REVERSE lista 
(setq lista ‘(1 2 3 4))
(do ((l lista (cdr l))
      (resultado nil (cons (car l) resultado)))
      ((null l) resultado) )
; ==> (4 3 2 1)


; ## DOTIMES
; permite escribir procedimientos sencillos con iteración controlada por un contador. Ejecuta el cuerpo un número de veces determinado.
; Sintaxis: (DOTIMES (var forma-limite-superior [forma-resultado]) 
;                     cuerpo)
; Cuando comienza el ciclo se evalúa la forma límite superior, produciendo un valor n. Entonces desde el valor 0 incrementándose en uno hasta n-1, se asigna a la variable var.
; Para cada valor de la variable se ejecuta el cuerpo y al final, la ligadura con la variable se elimina y se ejecuta la forma resultado dando valor al DOTIMES. Si no tiene forma-resultado, finaliza con NIL.

(dotimes (cont 4) 
         (print (* cont cont))
)
; ==> 0
; ==> 1
; ==> 4
; ==> 9
; ==> NIL

(dotimes (x 5 '(fin)) 
         (print (list x x))
)
; ==> (0 0) 
; ==> (1 1) 
; ==> (2 2) 
; ==> (3 3) 
; ==> (4 4) 

; ## DO LIST
; asigna a VAR el primer elemento de la lista L; evalúa S1,..., SN para cada valor de VAR; si L no tiene más elementos, devuelve resultado; en otro caso, le asigna a VAR el siguiente elemento de L e itera el proceso. Si no tiene el valor de resultado, finaliza con NIL.
(DOLIST (VAR L [resultado])
         S1, ..., SN)

(dolist (x '(a b c))
        (print x) 
)
; ==> A
; ==> B
; ==> C
; ==> NIL

; ## MAPCAR
; Va aplicando la función fn a los sucesivos car's de las listas. Devuelve una lista con los resultados de las llamadas a la función. Utiliza LIST para construir el resultado.
; (MAPCAR fn lista1 ... listan)

(mapcar ‘+ ‘(7 8 9)  ́(1 2 3)); ==> (8 10 12)
(mapcar ‘oddp ‘(7 8 9)); ==> (T NIL T)
(mapcar ‘atom ‘(A (B) 3)); ==> (T NIL T)

; ## MAP LIST
; Se aplica la función fn a los CDR ́s sucesivos de las listas. Devuelve una lista con los resultados de las llamadas a la función. Utiliza LIST para construir el resultado.
; Sintaxis: (MAPLIST fn lista1 ... listan)

(maplist '+ ' (7 8 9) '(1 2 3)) ; ==> ((8 10 12) (10 12) (12))
(maplist 'cons '(1 2 3) '(a b c)) ; ==> (((1 2 3) A B C) ((2 3) B C) ((3) C))