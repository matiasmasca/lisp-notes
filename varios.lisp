( defun saludar()
  (print "hola mundo!"))

(saludar)

( defun saludar_conocido(nombre)
  (print 
    (concatenate 'string "Saludos " nombre ", bienvenido a Lisp")))

(saludar_conocido "Pepe")

;; mi funcion suma, notacion polaca para la operacion
(defun mi_suma(a b)
  (+ a b))

(mi_suma 3 5)
(print (mi_suma 3 5))


; Trabajando con una Lista.
;; Sumando Elmenentos
(defun sumar_lista(lista)
    (print (reduce '+ lista)))
;; recorre la lista con reduce y le aplica la funcion + a cada elemento

(sumar_lista '(4 5 8 7 6 10))

;; Multiplicar elementos por valor
(defun multiplicar_lista_por_x(lista_param cantidad)
  (mapcar
      (lambda (elemento)(* elemento cantidad)) 
      lista_param
   )
)

( print (multiplicar_lista_por_x '(4 5 8 7 6 10) 2))



;; variables
(defvar mascota "perro") ;; la crea y la asigna
(print mascota)
(setq mascota "gato") ;; la asigna
(print mascota)


;; Recorrer una lista, forma iterativa
(defvar dias '("lunes" "martes" "miercoles" "jueves" "viernes" "sabado" "domingo"))
(print dias)

(defun mostrar_lista(lista_param)
    (loop for n in lista_param
          do(print n)))

(mostrar_lista dias)


;; inmutabilidad
;; -- la variable numbers no es afectada al evaluarse
(defvar numbers '(1 2 3 4 5))
(defvar numbers2  (multiplicar_lista_por_x numbers 3))

(print numbers)
(print numbers2)