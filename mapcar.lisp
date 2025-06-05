;; MAPCAR utilizada para aplicar una función a cada elemento de una o más listas, y recolectar los resultados en una nueva lista

;; La función toma el primer elemento de cada lista, los pasa como argumentos a la función especificada, y coloca el resultado en la nueva lista. 
;; Luego hace lo mismo con el segundo elemento de cada lista, y así sucesivamente.

;; Sintaxis: (mapcar función lista1 lista2 ... listaN)

; Ejemplos

(defun incremento_uno (numero)
  "Suma 1 al valor numérico pasado como parámetro"
  (+ numero 1))

(mapcar #'incremento_uno '(10 20 30))
; Resultado: (11 21 31)

(mapcar #'1+ '(1 2 3 4))
;; Resultado: (2 3 4 5)

;; con multiples listas
(mapcar #'+ '(1 2 3) '(10 20 30))
;; Resultado: (11 22 33)

(mapcar #'* '(2 3 4) '(5 6 7) '(1 2 3))
; Resultado: (10 36 84)

(mapcar #'string-upcase '("hola" "mundo" "lisp"))
;; Resultado: ("HOLA" "MUNDO" "LISP")

;; con funciones anonimas
(mapcar #'(lambda (x) (* x x)) '(1 2 3 4 5))
;; Resultado: (1 4 9 16 25)


;; Combinar Listas a SubListas
(mapcar #'(lambda (x y) (list x y)) '(a b c) '(1 2 3 4))
;; Resultado: ((A 1) (B 2) (C 3))


(defun nuevaLista (x y) (list x y))

(mapcar #'nuevaLista '(a b c d) '(1 2 3 4))