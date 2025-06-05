;; Método Iterativo
(defun fibonacci (n)
  (do
    ((i 1 (1+ i))
     (fib1 0 fib)
     (fib 1 (+ fib fib1)))
    ((= i n) fib)))

;; Método Recursivo
(defun fibonacci (n)
  (if (<= n 1)
      1
      (+ (fibonacci (- n 2)) (fibonacci (- n 1)))))


( PRINT (fibonacci '3) )