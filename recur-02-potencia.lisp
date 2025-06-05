(defun potencia-aux (b e)
  (if (= e 0) 1
    (* b (potencia-aux b (1- e)))))

( PPRINT (potencia-aux '3 '4))