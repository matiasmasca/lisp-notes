; # TEMA 3: MANEJO DE LISTAS Y ARREGLOS
; 3.1 Construcción de listas. 
; 3.2 Acceso a componentes de listas. 
; 3.3 Información de listas. 
; 3.4 Eliminación de elementos de listas. 
; 3.5 Listas/Arboles. 
; 3.6 Manipulación de estructuras. 
; 3.7 Manipulación de arreglos. 
; 3.8 Las A-Listas. 
; 3.9 Las P-Listas.

;; Las listas son conjuntos ordenados de átomos y/o listas conectados.
;; Se utilizan para agrupar elementos, es decir son una suceción de atomos o listas.
;; Para realizar la manipulación de los elementos de una lista es importante tener acceso a sus elementos.

;;; ##############
;;; FUNCIONES PARA OPERAR SOBRE LISTAS

;; # SELECCION DE ELEMENTOS
;; Existen tres tipos importantes de funciones de selección de elementos:
;; - CAR - devuelve el primer elemento de la lista.
;; - CDR - devuelve toda la lista menos el primer elemento.
;; - C*R - permite la concatenación de funciones CAR Y CDR. Es decir, CADR, CDDR, CADADR, etc.

;;; # DATO CURIOSO
;; car ("contents of the address part of register number"), 
;; cdr ("contents of the decrement part of register number"), 
;; cpr ("contents of the prefix part of register number"),
;; car is an acronym from the phrase “Contents of the Address part of the Register”; and cdr (pronounced “could-er”) is an acronym from the phrase “Contents of the Decrement part of the Register”. These phrases refer to the IBM 704 computer on which the original Lisp was developed.  The IBM 704 is a footnote in history, but these names are now beloved traditions of Lisp. 
;; CAR (basically meaning following the pointer in that register) a

;; # CAR - Primer Elemento 
;; La función CAR, devuelve el primer elemento de una lista. Su sintaxis es (CAR LISTA), o de una manera más técnica sería: (CAR CONS).
;; CAR no genera un nuevo valor, sino que da como resultado la dirección de un elemento que existe.

(CAR '(1 2 3)) ; ==> 1

(CAR '(A B C) ) ; ==>  A
(CAR '(A B (C D) E) ) ; ==> A
(CAR '((A B) C) ) ; ==> (A B)
(CAR '((A B C)) ) ; ==> (A B C)
(CAR 'A)  ; ==> Error: A is not a list

;; # CDR - Resto de la lista
;; La función CDR, para listas propias, o no punteadas, devuelve la lista sin el primer elemento.
;La sintaxis de esta función es: (CDR LISTA)

(CDR '(A B C) ) ; ==> (B C)
(CDR '(A B (C D) E) ) ; ==> (B (C D) E)
(CDR '((A B) C) ) ; ==> (C)
(CDR '((A B C)) ) ; ==> NIL
(CDR 'A) ; ==> ERROR A is not a list

;; Para una lista punteada, el CDR de una estructura CONS puede ser un átomo. Por tanto, en estos casos se devolverá un átomo y no una estructura CONS.
(CDR '(A . B) ) ; ==> B

;;; # C*R
;; Las funciones C*R, son una abreviación de las formas CAR y CDR. Podemos invocar a las llamadas 
;; (C*R LISTA), (C**R LISTA), (C***R LISTA), (C****R LISTA)
;;, donde el * podrá sustituirse por A, para invocar a CAR, o por D para invocar a CDR. Solo pueden sustituirse cuatro posiciones en la misma función. Los caracteres incluidos invocan a las funciones respectivas en orden de Derecha a Izquierda.
(CADR '(A B C) ) ; ===> B
(CAR (CDR '(A B C) ) ) ; ===> B

(CDAR '((A B C) D E) ) ; ===> (B C)
(CDR (CAR '((A B C) D E ) )) ; ===> (B C)

(CADDR '(A B C D E) ) ; ===> C
(CAR (CDDDDR '(A B C D E) ) ) ; ===> E

;; DATO CURIOSO: tambien existen las funciones FIRST y REST, equivalentes a CAR y CDR respectivamente. 

;; # LAST LIST - ULTIMO ELMENTO
;; (LAST LIST)
;; Esta función devolverá la última estructura CONS de la lista. Si hay un solo elemento, se toma como ultimo.
(LAST '(A B C)) ; ==> (C)
(LAST '(A (B C))) ; ==> ((B C))
(LAST '(1 2 3.5)) ; ==> (3.5)
(LAST '((A B C)) ; ==> ((A B C))

;; # ELT LISTA INDICE. - SELECCIONAR ELEMENTO
;; Esta función devolverá el elemento de la lista que está en la posición índice. El primer elemento de la secuencia tiene el índice en la posición 0. Ejemplos,
(ELT '(A (B C)) 1) ; ==> (B C)
(ELT '(A B) 3) ;  ==> ERROR index 3 too large
(ELT '(A B C D E) 2) ; ==> C ; base 0, por eso devuelve C

;; ##########################################
;; FUNCION DE ASIGNACION 

;; # SETQ
;; La función SETQ asigna un valor a una variable. 
;; Su sintaxis es: (SETQ {VAR FORM}+).
;; La Q de SETQ es un mnemotécnico de la función Quote. Las llaves y el signo mas, representan repetición del par VAR FORM.
;; (SETQ VAR FORM VAR FORM VAR FORM ...)
;; DEBE haber un número par de argumentos. Los argumentos impares de la función no son evaluados.
(setq x 4 y 6 z 9) ; ==> 9

; Esto asigna 4 a x, 6 a y, 9 a z. Siempre las evaluaciones de expresiones, devuelven el resultado de la ultima evaluación. En este caso, la ultima evaluación fue z 9.

; * Las evaluaciones de las formas y las asignaciones (primero se evalúa y luego se asigna), se realizan secuencialmente.
(SETQ X 5) ; ==> 5
(SETQ NOMBRE "MARIA") ; ==> "MARIA"
(SETQ FRUTA (CAR '(MANZANA PERA ))) ; ==> MANZANA
(SETQ LISTA1 '(A B C)) ; ==> (A B C)
(SETQ X 5 Y 6 Z 7 ; ==> 7
(SETQ X 5 Y 6 Z) ; ==>  Error: too few arguments


;; #########################################
;; FUNCIONES DE CONSTRUCCION DE LISTAS
;; Las funciones con las que Lisp permite construir listas son
;; * CONS
;; * LIST
;; * APPEND.

;; # CONS - construct
;; La función CONS, crea una nueva estructura cons. Sintácticamente podemos expresarlos como:
;; (CONS SEXP SEXP)
;; donde SEXP son expresiones simbólicas, pueden ser átomos o listas.
;; El CAR de la nueva lista será la primera SEXP y el CDR la segunda.
(CONS 'A '(B C D) ) ; ==> (A B C D)
(CONS '(X Y) '(B C D) ) ; ==> ((X Y) B C D)
(CONS '((X Y) (W Z)) '((A B)) ) ; ==> ( ((X Y) (W Z)) (A B))
(CONS 'A NIL ) ; ==> (A)
(CONS NIL '(A) ) ; ==> (NIL A)

; * Obsérvese que si la segunda SEXP corresponde con un átomo entonces se creará una lista punteada ("impropia").
(CONS 'A 'B ) ; ==> (A . B)
(CONS '(A B) 'C ) ; ==> ((A B) . C)
  
;; # LIST
;; LIST &REST ARGS
; La función LIST devuelve una lista formada con todos los elementos pasados como argumentos. Los argumentos deberán ser expresiones simbólicas válidas.
; La notación &REST ARG implica que se permite cualquier número de argumentos. Es decir, se le puede pasar a la llamada de la función cualquier número de parámetros.
; sintaxis:  (LIST SEXP SEXP ...)
; * LIST se utiliza para crear listas propias. Las listas impropias sólo pueden crearse a partir de la función CONS.
(LIST 'A 'B 'C) ; ==> (A B C)
(LIST '(A) '(B) '(C)) ; ==> ((A) (B) (C))
(LIST 'A '(B C)) ; ==> (A (B C))


;; # APPEND.
;;  APPEND &REST ARGS
; Permite crear una nueva lista concatenando dos o más listas dadas. Mientras que LIST y CONS aceptan listas y átomos como argumentos, APPEND sólo acepta listas, excepto en el último argumento. También acepta un número indefinido de argumentos.
; La función APPEND concatena los argumentos en una lista. Todos los argumentos, excepto el último deben ser listas. Los argumentos que no sean listas no serán incluidos.
; sintaxis: (APPEND LISTA LISTA ... LISTA SEXP)
(APPEND '(A) '(B) '(C)) ; ==> (A B C)
(APPEND '(A) '(B C)) ; ==> (A B C)
(APPEND '((A)) '((B)) '((C)) ) ; ==> ((A) (B) (C))
(APPEND '(A) NIL) ; ==> (A)
(APPEND NIL '(B C)) ; ==> (B C) ; NIL es una lista y por tanto cumple los requerimientos de la definición de APPEND.
(APPEND 'A '(B C)) ; ==> ERROR
(APPEND '(A) '(B) 'C) ; ==> (A B . C)
    
; #######################
; # Otras funciones de listas

; # BUTLAST - SIN LOS ÚLTIMOS
; (BUTLAST lista n)
; devuelve una nueva lista con los N-últimos elementos de la lista pasada como parámetro eliminados. No se modifica el contenido de la lista original
(setq b '(3 4 5 6))
(butlast b 2) ; ==> (3 4) 
b ; ==> (3 4 5 6)
; * Si quisiéramos que el resultado se asigne a una nueva lista, haríamos LO SIGUIENTE:
(setq lista (butlast b n))

; # NTH - DEVOLVER ORDINAL
; (NTH <n> <list>)
; Permite extraer el elemento “n” de una lista “list”. El primer elemento es N = 0.
(setq b '(4 5 6))
(nth 1 b) ; ==> 5
(nth 0 b) ; ==> 4

; # NTHCDR - devolver los ultimos N.
; (NTHCDR <n> <list>)
; Permite extraer el cdr de una lista “list” a partir del elemento “n”. Llama repetidamente a CDR.
(setq b '(4 5 6 7 8)) 
(nthcdr 3 b) ; ==> (7 8)
(nthcdr 1 b) ;  ==> (5 6 7 8)
      
; # REVERSE
; (REVERSE <list>)
; Permite obtener el reverso de una lista. No modifica el contenido de la lista original.
(setq b '(4 5 6 7 8))
(reverse b) ; ==> (8 7 6 5 4)
b ; ==> (4 5 6 7 8)
; * Si quisiéramos guardar la lista resultado debemos hacer:
(setq lista1 (reverse b))


;;; #######################################
;; FUNCIONES DESTRUCTIVAS de Manipulacion

; # RPLACA - REPLACE CA
; (RPLACA lista elem)
; sustituye el car de la lista con elem.
(setq a '(1 2 3))
(rplaca a 7) ; ==> (7 2 3)
a ; ==> (7 2 3)

; # RPLACD - REPLACE CDR
; (RPLACD lista elem)
; sustituye el CDR de la lista con elem.
(setq b '(1 2 3))
(rplacd b '(9)) ; ==> (7 2 3)
b ; ==> (7 2 3)

; # NCONC
; (NCONC lista1 ... listaN)
; devuelve su valor modificando el símbolo que expresa lista1, y modifica el valor de las sucesivas listas hasta n-1.
(setq a '(1 2) b '(3 4) c '(5 6) d '(7 8))
(nconc a b c d) ; ==> (1 2 3 4 5 6 7 8)
a ; ==> (1 2 3 4 5 6 7 8)
b ; ==> (3 4 5 6 7 8)
c ; ==> (5 6 7 8)
d ; ==> (7 8)

; # PUSH - PILAS: Añadir cabecera
; (PUSH item lista)
; añade el elemento item a lista al principio.
; equivalente: (setq lista (cons item lista))  
(setq b '(4 5 6))
(push 9 b) ; ==> (9 4 5 6)
b ; ==> (9 4 5 6)

; # POP - PILAS: Eliminar cabecera
; elimina el primer elemento de lista
; sintaxis: (POP lista)
; equivalente: (setq lista (cdr lista))
(setq b '(9 4 5 6))
(pop b) ; ==> 9
b ; ==> (4 5 6)

      
; ##########################################
; OTRAS FUNCIONES

; # LENGTH SECUENCIA
; (LENGTH list)
; Devuelve el número de elementos existentes en el nivel superior de la lista (o secuencia).
(LENGTH '(A B C)) ; ==> 3
(LENGTH '(A B . C)) ; ==> 2 (en este caso devuelve 2, porque el segundo elemento es uno solo llamado “Par punteado”).
(LENGTH '()) ; ==> 0

; # MEMBER
; MEMBER ITEM LISTA {&KEY {:TEST / :TEST-NOT / :KEY}}
; La función MEMBER busca en el nivel superior de la LISTA un elemento que sea igual, EQL, que el ITEM. Si se encuentra un elemento que cumple esta condición, la función devolverá una lista cuyo CAR es el elemento buscado y el CDR es el resto de la lista.
; La opción :TEST permite realizar comparaciones sucesivas del ITEM con cada uno de los elementos de la lista a través del operador señalado después de :TEST, hasta encontrar el primer elemento que cumple dicha condición, es decir, que de NO-NIL.
; La opción :TEST-NOT es semejante a la anterior, salvo que se detiene al encontrar el primer elemento en cuya evaluación se obtenga NIL. 
; La opción :KEY hace lo mismo que las anteriores pero aplica la función indicada en clave.
(MEMBER 'C '(A B C D E F)) ; ==> (C D E F)
(MEMBER 'Z '(A B C D E F)) ; ==> NIL
(MEMBER 'J '(A B (I J) F)) ; ==> NIL
(MEMBER '(X Y) '(A B C (X Y) D)) ; ==> NIL

; Su valor es NIL por que compara con EQL
(MEMBER '(X Y) '(A B C (X Y) D) :TEST #'EQUAL) ; ==> ((X Y) D)
(MEMBER '7 '(3 5 7 9) :TEST-NOT #'>) ; ==> (7 9)
      
; # FMAKUNBOUND - deligar simbolo
; (FMAKUNBOUND <sym>)
; Permite desligar un símbolo de una función.
(fmakunbound mi-suma) ; desliga la función mi-suma
mi-suma ; ==> Error: The variable MI-SUMA is unbound

; # MAKUNBOUND
; (MAKUNBOUND <sym>)
; Permite desligar un símbolo de su valor.
(setq a 4) ; liga el símbolo a con 4
a ; ==> 4
(makunbound 'a) ; desliga el símbolo a
A ; ==> Error: The variable A is unbound.

; # BOUNDP <sym>
; (BOUNDP <sym>)
; El predicado boundp chequea si el símbolo sym, tiene un valor ligado a el. Retorna T si tiene un valor ligado, o NIL en caso contrario.
(setq a 1) ; liga la variable a con el valor 1
(boundp 'a) ; ==> T ; retorna T – el valor es 1

; # FBOUNDP
; (FBOUNDP <sym>)
; El predicado fboundp chequea si el símbolo sym, tiene una definición de función ligada a el. Retorna T si tiene un valor ligado, o NIL en caso contrario.
(defun f1 (x) (print x)) ; set up function F1
(fboundp 'f1) ; ==> T
(fboundp 'car) ; ==> T
(fboundp 'f2) ; ==> Nil


; #########################################
; # PREDICADOS Y OPERADORES LOGICOS P/LISTAS
      
; ### Predicados sobre TIPOS DE DATOS.
; Todos los predicados que se muestran a continuación devuelven T si son ciertos o NIL sino lo son.
; - (ATOM OBJETO) ==> Devuelve true si el objeto NO es una construcción CONS.
; - (CONSP OBJETO)  ==> Devuelve cierto si el objeto es CONS.
; - (LISTP OBJETO) ==> Devuelve cierto si el objeto es CONS o NIL (la lista vacía).
; - (NULL OBJETO) ==> Será cierto si objeto es NIL.
; - (TYPEP OBJETO TIPO-ESPECIFICADO) ==> Es cierto cuando el objeto pertenece al tipo definido en tipo especificado.

; ### Predicados de IGUALDAD.
; Los objetos Lisp pueden verificarse a diferentes niveles de igualdad:
; - Igualdad numérica: =
; - Identidad referencial: EQ
; - Identidad representacional: EQL (acepta solo dos argumentos)
; - Igualdad estructural EQUAL (acepta solo dos argumentos)
; - Igualdad de valores: EQUALP.

; # EQ
; (EQ X Y)
; La función EQ testea si X e Y están referenciados por punteros iguales. Si son objetos identicos.
; Solo se utiliza para caracteres, símbolos y enteros pequeños menores o iguales a 255.
; sintaxis: (EQL SEXPR SEXPR)
(eq 'a 'a) ; ==> T
(eq 1 1) ; ==> T
(eq 256 256) ; ==> Nil
(eq 1 1.0) ; ==> Nil
(eq "a" "a") ; ==> Nil

; # EQL - igual valor
; (EQL X Y)
; La función EQL testea si X e Y representan el mismo valor. Esta función puede devolver false aunque el valor imprimible de los objetos sea igual. Esto sucede cuando los objetos apuntados por variables parecen los mismos pero tienen diferentes posiciones en memoria.
; Este predicado no conviene utilizarlo con strings, ya que su respuesta no será fiable. Por el contrario se deberá utilizar con variables y números cuando sea preciso conocer si tienen la misma posición en memoria.
; sintaxis: (EQL SEXPR SEXPR)
(EQL 5 5) ; ==> T
(EQL 5 5.0) ; ==> NIL 

(SETQ A 'WORD)
(SETQ B 'WORD) 
(EQL A B) ; ==> NIL

(SETQ L '(A B C))
(EQL  '(A B C) L) ; ==> NIL

(SETQ M L)
(EQL L M) ; ==> T

(SETQ N  '(A B C)) 
(EQL L N) ; ==> NIL
(EQUAL L A) ; ==> NIL

; # EQUAL
; (EQUAL X Y)
; Verifica si los objetos X e Y, son estructuralmente iguales. Es decir, los valores imprimibles de los objetos son iguales. 
; Sintaxis: (EQUAL SEXPR SEXPR).

(EQUAL 5.0 5) ; ==> nil
(EQUAL NIL ()) ; ==> T
      
(SETQ A 'WORD B 'WORD)
(EQUAL A B) ; ==> T
      
(SETQ L '(A B C))
(EQUAL '(A B C) L) ; ==> T

(SETQ M L)
(EQUAL L M) ; ==> T
    
(SETQ N '(A B C))
(EQUAL L N) ; ==> T
(EQUAL L A) ; ==> nil

; # EQUALP
; (EQUALP X Y)
; La función EQUALP testea si X e Y representan el mismo valor, pero a diferencia de EQUAL, esta función se denomina Case Insensitive. A diferencia de Eql, puede aplicarse a los string.
(EQUALP 5 5) ; ==> T
(EQUALP 5 5.0) ; ==> T 
(EQUALP "a" "A") ; ==> T

; ### Resumen:
; • Eq – Punteros idénticos. Palabras con caracteres, símbolos y pequeños enteros.
; • Eql – Solo números del mismo tipo.
; • Equal – Listas y Strings.
; • Equalp – Caracteres y strings con identificación de mayúsculas, números de diferentes tipos, arreglos.



; #######################
; ## OPERADORES LOGICOS
; Lisp tiene tres de los operadores lógicos más comunes como primitivas, AND, OR y NOT.      

; AND
; La función AND evalúa sus argumentos en orden. Si cualquiera de los argumentos se evalúa a NIL, se detiene la evaluación y se devuelve el valor NIL. Por el contrario si todos los argumentos son NO-NIL, devolverá el resultado de la ÚLTIMA EVALUCACION.
; Sintácticamente: (AND SEXPR SEXPR ...)
(AND T (< 2 5) (> 7 4) (* 2 5)) ; ==> 10

(SETQ X 3 CONT 0)
(INCF CONT)
(AND (<= CONT 10) (NUMBERP X) (* 2 X)) ; ==> 6
(AND (EVENP X) (/ X 2)) ; ==> NIL

; OR
; La función OR evalúa sus argumentos en orden. Si cualquiera de los argumentos se evalúa a NO-NIL, se detiene la evaluación y se devuelve T. Por el contrario si todos los argumentos son NIL, la función OR devolverá el resultado de la ultima sexp.
; Sintácticamente (OR SEXPR SEXPR ...)
(OR NIL (= 4 5) (NULL '(A B)) (REM 23 13) ) ; ==> 10

(SETQ X 10)
(OR (< 0 X) (DECF X) ) ; ==> T
(OR (> 0 X) (DECF X) ) ; ==> 9
(OR (CONSP X) (EVENP X) (/ X 2)) ; ==> 5
      
; NOT - NEGACION
; (NOT FORM)
; La función NOT evalúa un único argumento. La función NOT devuelve T o NIL. Si los argumentos se evalúan a NIL, entonces devuelve T, en otro caso NIL.
; Sintácticamente (NOT SEXPR), está permitido sólo un argumento.
(NOT NIL) ; ==> T
(NOT T) ; ==> NIL
(NOT (EQUAL  'A  'A) ) ; ==> NIL
(SETQ X  ́(A B C) )
(NOT (ATOM X)) ; ==> T













