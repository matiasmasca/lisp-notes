; ### ARRAYS
; Un array puede declararse explícitamente en algunos dialectos del LISP usando la función "array", la cual tiene la siguiente forma:
; (ARRAY NOMBRE T TAMAÑO)
; - "Nombre" identifica al array y "tamaño" es una secuencia de enteros que identifica al número de elementos de cada dimensión. 
; Por ejemplo, supongamos que A es un array de cinco elementos y B es un array de 5 x 4. Estos arrays pueden declararse como sigue:
(array A t 5) 
(array B t 5 4)

; Para leer un valor en un array se referencia mediante una lista que contiene el nombre del array y una serie de subíndices que identifican la posición de la entrada en el array. Las filas y las columnas se enumeran desde 0. Por tanto, la tercera entrada de A se referencia por (A 2) y el elemento de la cuarta fila y tercera columna de B se referencia por (B 3 2).
; Para asignar un valor a una entrada de un array, se utiliza la siguiente función store:
(store (nombre subíndices) valor)
; Por ejemplo, para almacenar el valor 0 en la entrada de la cuarta fila y tercera columna de B escribimos:
(store (B 3 2) 0)
;En general, el valor almacenado puede especificar cualquier expresión en LISP y cada uno de los subíndices pueden también ser cualquier expresión en LISP cuyo valor sea un entero en el rango adecuado. Por tanto, los arrays en LISP generalmente no tienen tipo, puesto que cada entrada puede ser diferente en estructura y tipo del resto.


; ### LISTA DE PROPIEDADES
; Un método mucho más útil de estructurar datos en una lista es mediante la llamada "lista de propiedades". Esta es una lista que tiene la siguiente forma general:
(p1 V1 p2 V2 .. pn Vn) 
; donde los p son átomos que denotan propiedades y las V son valores asociados estas propiedades. Para ilustrar esto, supongamos que tenemos la información para un individuo clasificada de la siguiente forma:
(NOMBRE (JOSE "PEPE" ARGENTO)
 DU# 07540743
 SUELDO_NETO 125400
 DIRECCION ((1800 BULL RUN) ALEXANDRIA VA 22200)
)
; Aquí hay cuatro propiedades, un nombre, un número de documento unico, un sueldo y una dirección. Lo anterior corresponde a la definición de una lista de propiedades (denominada EMPLEADO). 
; Para obtener información de una lista de propiedades usamos la función "GET" como sigue:
(get nombre p)
; - "Nombre" identifica a la lista y p identifica la propiedad cuyo valor se desea. Por ejemplo, para obtener el SS# de una PERSONA escribimos:
(get PERSONA DU #)
; y el valor devuelto, para el anterior ejemplo, será 07540743.

; Para reemplazar información en una lista de propiedades, se utiliza la función "PUT":
(put nombre p v)
; . "Nombre " identifica a la lista, p la propiedad cuyo valor va a ser reemplazado y v es el nuevo valor.  
(put PERSONA SUELDO_NETO 300000)
; Altera la propiedad SUELDO_NETO de EMPLEADO, de forma que su valor se hace 300000 en vez de 125400, como había sido.

; La función "REMPROP" quita una propiedad y su valor asociado de la lista.
(remprop nombre p)
; "Nombre" identifica la lista de propiedades afectada y p identifica la propiedad y el valor que han de quitarse.
; Por tanto, las listas de propiedades son muy útiles para definir lo que se conoce en otros lenguajes como "registros". Típicamente una lista de propiedades representa un nodo en una lista mayor, enlazada de registros, todos con el mismo conjunto de propiedades. Esta lista mayor es conocida comúnmente como un "archivo" en otros lenguajes. Por ejemplo, el registro EMPLEADO puesto anteriormente puede ser un nodo de una lista que contenga todas las personas empleadas en una determinada organización. Igualmente, puede definirse otra lista de propiedades especificas para una entrada en un catálogo de fechas de una biblioteca y la lista de todas las entradas puede representar el propio catálogo.