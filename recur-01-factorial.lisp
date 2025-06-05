;; Función Factorial (n!)
(defun factorial (n)
  (if (= n 0) 1
      (* n (factorial (- n 1)))
      )
  ) ; existe una llamada a función factorial en el cuerpo de la función factorial.

( PRINT (factorial '3) )