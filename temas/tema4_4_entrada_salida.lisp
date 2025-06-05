;;;; ### TEMA 4: ESTRUCTURAS DE CONTROL DEF LUJO Y RECURSIVIDAD.
; Secuencia de Acciones. Estructuras condicionales. Estructuras iterativas. Comunicación con elusuario: entrada/salida. Recursividad. Definición de procedimientos que se usan a sí mismos. Recursividad de cola, eficiencia. Procedimientos iterativos. Aplicación explícita y reiterada de una función a todos los datos contenidos en listas. Destructividad/N o destructividad. Lista de funciones.

; ###############################################################
; ### 4.4 Comunicación con elusuario: entrada/salida.

; Lisp tiene primitivas de funciones de entrada/salida. Las más comúnmente utilizadas son: READ, PRINT y FORMAT
; Los dispositivos por defecto son, el de entrada el teclado (*standard-input*) y salida la pantalla (*terminal-io*), pero se puede redireccionar a otros dispositivos. Es decir, todas estas funciones toman un argumento OPCIONAL llamado "input-stream" o "output-stream".

; ### Función de entrada.
; ## READ
; lee un objeto Lisp del teclado y devuelve dicho objeto.
; Esta función detiene la ejecución del programa mientras no se termine con un RETURN. 
; Sintácticamente:  (READ &optional stream)

(read)
35
; ==> 35
> (setq a (read))
a
;==>Hola

; ### Funciones de salida.
; ## PRINT
; toma un objeto Lisp como argumento y lo escribe en una nueva línea con un blanco por detrás (introduce <Return> y blanco después del objeto). Escribe los objetos por su tipo, tal y como serían aceptados por un READ.
; Sintaxis: (PRINT objeto &optional stream)

(print 'a) ; ==> imprime A con #\Newline
(print '(a b)) ; ==> imprime (A B) con #\Newline
(print 99) ;  ==> imprime 99 con #\Newline
(print "hola") ; ==> imprime "hola" con #\Newline

; # PRIN1
; PRIN1, toma un objeto Lisp como argumento y lo escribe con un blanco por detrás. Escribe los objetos por su tipo, tal y como serían aceptados por un READ, sin salto de línea. La próxima impresión será al lado.
; Sintaxis: (PRIN1 objeto &optional stream)
(prin1 'a) ; ==>  imprime A sin #\Newline
(prin1 '(a b)) ; ==>  imprime (A B) sin #\Newline
(prin1 2.5) ; ==>  imprime 2.5 sin #\Newline
(prin1 "hi") ; ==>  imprime "hi" sin #\Newline
; ==> A(A B)2.5"hi"

; # PRINC
; PRINC, toma un objeto Lisp como argumento y lo escribe con un blanco por detrás, y en el caso de los string, imprime sin las comillas. No escribe los objetos por su tipo.
; Sintaxis: (PRINC objeto &optional stream)
(princ 'a) ; ==> imprime A sin #\Newline
(princ '(a b)) ; ==> imprime (A B) sin #\Newline
(princ 99) ; ==> imprime 99 sin #\Newline
(princ "hi") ; ==> imprime hi sin #\Newline
; ==> A(A B)99hi 

; # P PRINT
; PPRINT, toma un objeto Lisp como argumento y lo escribe con un blanco por detrás, introduce solo un Return. No realiza la segunda evaluacion.
; Sintaxis: (PPRINT objeto &optional stream)
(pprint 'a)  ; ==> imprime A returns NIL
(pprint "abcd") ; ==> imprime "abcd" returns NIL


; ## FORMAT - interpolación?
; FORMAT, aparece como un mecanismo más generalizado de dar la salida. Se indican directivas que son como variables situadas entre el string y comienzan con ~. Muchas directivas miran el siguiente argumento de la lista del format y lo procesan de acuerdo a sus propias reglas. 
;Su forma es
; (FORMAT destino control-string &rest argumentos)
; - destino = nil/ t/ stream. Si se indica el nil, se creará un string con las características indicadas y éste será lo que devuelva el format. Si se indica t, será la salida por defecto y sino será otro dispositivo indicado en stream.
; - control-string = contiene el texto a escribir y "directivas"
; - argumentos = lista con los parámetros para las "directivas"

; Se usan diferentes directrices para procesar diferentes tipos de datos e incluirlos en el string:
;     ~A - Imprime un objeto cualquiera.,
;     ~D - Imprime un número en base 10
;     ~S - Imprime una expresión simbólica.
; Además se puede utilizar el símbolo @ junto con la directiva ~A, para justificar a la derecha valores numéricos. 
; No todas la directrices utilizan argumentos: ~
;     % - Inserta una nueva línea y
;     ~| - Nueva página.

(FORMAT T "STRING DE SALIDA")
; ==> STRING DE SALIDA

(SETQ VAR 5)
(FORMAT T "STRING QUE INCLUYE ~A" VAR) ; ==> STRING QUE INCLUYE 5
(FORMAT NIL "STRING QUE INCLUYE ~A" VAR) ; ==> nil
(FORMAT T "ESCRIBE EL ARGUMENTO ~10@A JUSTIFICADO A LA DERECHA CON 10 ESPACIOS." 1000) 
; ==> ESCRIBE EL ARGUMENTO       1000 JUSTIFICADO A LA DERECHA CON 10 ESPACIOS.
(FORMAT T "~%LA PRIMERA RESPUESTA ES ~5@A ~%Y LA SEGUNDA RESPUESTA ES ~3@A " (* 5 3 2) (/ 3 5)) ; ==> 
; ==>  LA PRIMERA RESPUESTA ES    30 
; ==>  Y LA SEGUNDA RESPUESTA ES 3/5 


; TERPRI
; TERPRI realiza un salto de línea
; (TERPRI &optional stream)
(princ "hola") ; ==> hola
(print "hola") ; ==> "hola"
(prin1 "hola") ; ==> "hola"
(format t "hola") ; ==> hola
(format nil "hola") ; 
(format nil "hola soy ~a, ~a" "yo" "Bonn") ; ==> "hola soy yo, Bonn"














