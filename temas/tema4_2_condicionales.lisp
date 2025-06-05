;;;; ### TEMA 4: ESTRUCTURAS DE CONTROL DEF LUJO Y RECURSIVIDAD.
; Secuencia de Acciones. Estructuras condicionales. Estructuras iterativas. Com unicación con elusuario: entrada/salida. Recursividad. Definición de procedim ientos que se usan a sí mismos. Recursividad de cola, eficiencia. Procedimientos iterativos. Aplicación explícita y reiterada de una función a todos los datos contenidos en listas. Destructividad/N o destructividad. Lista de funciones.

; ###############################################################
; ### 4.2 Estructuras condicionales
; Verifica y ramifica: verifica devolviendo NIL o NO-NIL (como cualquier función), y ramifica es el comando a ejecutarse posteriormente basándose en el resultado de la verificación.
; Existen diferentes condicionales en Lisp, igual que en otros lenguajes: IF, CASE, COND, WHEN, UNLESS, TYPECASE.

; ## IF ... THEN ELSE -- SI ENTONCES SINO...
;  IF TEST THEN [ELSE]. La forma condicional IF puede expresar opcionalmente su parte THEN.
; Sintaxis:
; (IF FORMA-COND FORMA-THEN)
; (IF FORMA-COND FORMA-THEN FORMA-ELSE)

(setq obj 5)
(if (numberp obj) "Es un número" "Esto no es un número ") ; ==> “Es un número"

(setq obj 'a)
(if (numberp obj) "Es un número" "Esto no es un número ") ; ==> "Esto no es un número"

; ## COND - CONDICIONES
; COND {(TEST {FORMA}+) }+. 
; Permite expresar varias condiciones, ejecutándose la primera que se cumple. La forma COND acepta cualquier número de listas cuyos elementos son formas.
; Estas formas se ejecutaran si y solo si el test devuelve un valor NO-NIL.
; La cabeza de las listas se evalúa secuencialmente hasta encontrar una que devuelva un valor NO-NIL, entonces las formas siguientes a la cabeza de esa lista se evalúan y se sale de la forma COND.
; Si hubiera listas más adelante cuyas condiciones (cabeza de lista) fueran también ciertas éstas no se evalúan y por tanto tampoco se ejecutan las formas incluidas en el resto. COND devolverá el resultado de la última forma evaluada de la lista con un test NO- NIL. Si todos los test devuelven NIL, entonces COND devolverá NIL.
(COND (TEST1 FORMA11 FORMA12 ... FORMA1N)
      (TEST2 ) ; no tiene ningún consecuente y se devolverá el valor NO-NIL del test
      (TEST3 FORMA31 FORMA32 ... FORMA 3M)
      ...
      (TESTR ...) )

(setq a 4) ; ==> 4
(cond
  ((numberp a) "Esto es un número")
  (t "Esto no es un número") )
; ==> "Esto es un número.

(setq x 8)
(cond ((< x 0) (print "NEGATIVO"))
      ((= x 0) (print "CERO"))
      ((<= X 10) (print "VALOR EN RANGO"))
      (T (print "VALOR MUY GRANDE") x)
  ) ;fin-cond
; ==> "VALOR EN RANGO"

; ## WHEN  - CUANDO
; (WHEN TEST FORMA1 ... FORMAN)
; Es equivalente a la forma IF sin opción de incluir la forma else. Si el TEST es verdadero, se evalúan todas las formas restantes.
 (setq a 4)
 (when (equal a 4) (print (* a a)) (print "cierto"))
; ==> 16
; ==> "cierto"
(when (equal a 3) (print (* a a)) (print "cierto"))
; ==> nil ; Al ser NIL el test, no se evalúa nada mas y devuelve NIL.

; ## UNLESS -- SINO 
; (UNLESS TEST FORMA1 ... FORMAN)
; Esta forma condicional difiere de las vistas hasta ahora en que se evaluarán las formas siempre que el resultado de la evaluación del test sea NIL.
; Hasta ahora la evaluación de las formas asociados a un test estaba condicionada a que éstas dieran como resultado un valor NO-NIL.
; Sirve para evitar hacer un if con negacion
(setq a 4)
(unless (equal a 4) (print (* a a)) (print "cierto")) ; ==> nil
(unless (equal a 3) (print (* a a)) (print "cierto"))
; ==> 16
; ===> "cierto"

; ## CASE  
; (CASE KEYFORM {(LIST { FORM}+)}+)
; Permite realizar ciertas acciones (FORM) cuando una determinada forma KEYFORM toma ciertos valores (LIST), expresados a través de una lista.

(setq mes 'jun) ; ==> jun
(case mes
    ((ene mar may jul ag oct dic) 31)
    ((abr jun sep nov) 30)
    (feb (if (zerop (mod año 4)) 29 28) ) ) ; ==> 30
; La clave mes, se compara con el contenido de los elementos de cada lista y si alguno coincide, muestra el resultado.

; ## TYPECASE
; (TYPECASE KEYFORM {(TYPE {FORMA}+)}+)
; Permite como en el caso anterior realizar una serie de acciones siempre que un determinado objeto sea de la clase indicada.
(SETQ X 2) ; ==> 2
(TYPECASE X
  (string "es un string")
  (integer "es un entero")
  (symbol (print '(es un símbolo)))
  (otherwise (print "operador")(print "desconocido") ) )
; ==> "es un entero"




