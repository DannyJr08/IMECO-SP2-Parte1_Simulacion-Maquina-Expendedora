#lang racket

; Situación Problema #2 Parte 1
; Juan Daniel Rodríguez Oropeza A01411625

; Funciones helpers

; Función de miembro? como predicado
(define (miembro? atomo lista)
  (cond ((null? lista) #false)
        ((equal? atomo (car lista)) #true)
        (else (miembro? atomo (cdr lista)))))

; Función de miembro->= que devuelve valor si se cumple con la condición, de lo contrario regresa falso.
(define (miembro->= atomo lista)
  (cond ((null? lista) #false)
        ((>= atomo (car lista)) (car lista))
        (else (miembro->= atomo (cdr lista)))))

; Función de miembro-valor, la cual funciona exactamente que miembro?, solamente que ésta devuelve el valor encontrado.
(define (miembro-valor atomo lista)
  (cond ((null? lista) #false)
        ((equal? atomo (car lista)) (car lista))
        (else (miembro-valor atomo (cdr lista)))))

; Función que cuenta cuántas coincidencias hay de un elemento en una lista.
(define (CUANTOS atomo lista)
  (cond ((null? lista) 0)
        ((equal? atomo (car lista)) (+ (CUANTOS atomo (cdr lista)) 1))
        (else (CUANTOS atomo (cdr lista)))))

; Función que regresa el elemento de una determinada posición de una lista.
(define (ACCESA-N pos lista)
  (cond ((null? lista) '())
        ((= pos 0) '())
        ((= pos 1) (car lista))
        (else (ACCESA-N (- pos 1) (cdr lista)))))

; Función que regresa la posición de un elmento en una lista.
(define (INDICE atomo lista)
  (if (equal? (list? (memv atomo lista)) #true)
      (- (+ (length lista) 1) (length (memv atomo lista)))
      '()))

; Función que reemplaza un elmento en una determinada posicón de una lista.
(define (reemplaza dato pos lst fin-lst)
    (if (= pos 1)
        (append (reverse (cons dato fin-lst)) (cdr lst))
        (reemplaza dato (- pos 1) (cdr lst) (cons (car lst) fin-lst))))

; Funciones del autómata

; Función que verifica si existe un producto basado en el codigo que fue insertado.
(define (verif-exis-prod inventario-productos codigo-transaccion repositorio-monedas nuevo-archBD og-inventario-productos)
  (cond ((null? inventario-productos) ; Si ya se hizo todo el recorrido y no hubo coincidencia...
         (escribe-lo-mismo nuevo-archBD og-inventario-productos repositorio-monedas) ; Escribe el mismo contendio qu había leído originalmente del archivo.
         "Error: Usted ha ingresado un código que no existe.") ; Despliega el mensaje de error.
        ((equal? (caaar codigo-transaccion) (caar inventario-productos)) ; Si hay coincidencia...
         (transicion (cadar codigo-transaccion) (caddar inventario-productos) repositorio-monedas 0 (caaar codigo-transaccion) nuevo-archBD og-inventario-productos)) ; Llama a la función de transición
        (else (verif-exis-prod (cdr inventario-productos) codigo-transaccion repositorio-monedas nuevo-archBD og-inventario-productos)))) ;  Sigue buscando recursivamente

; Función como predicado que decide si aceptar o no una secuencia de transiciones.
(define (acepta? repositorio-monedas transaccion)
  (if (equal? validador-espacio-monedas #true)
      (cond ((null? transaccion) #false)
            ((miembro? (cadar repositorio-monedas) transaccion) #true) ; Regresa verdadero si el valor de la moneda se encuentra en la secuencia de transiciones.
            (else (acepta? (cdr repositorio-monedas) transaccion))) ; Llama a la función recursivamente haciendo cdr al inventario de monedas.
      (validador-espacio-monedas repositorio-monedas transaccion))) ; Llama a la función de validador-espacio-monedas para regresar el mensaje de error correspondiente en pantalla.

; Función helper de validador-espacio-monedas que se dedica principalmente a contar las coincidencias de cada moneda en la secuencia de transiciones.
(define (validador-espacio-monedas-helper repositorio-monedas transaccion)
  (if (null? (cdr repositorio-monedas)) ; Si ya es el último elemento...
      (cons (CUANTOS (cadar repositorio-monedas) transaccion) '()) ; Solamente despliega un valor.
      (cons (CUANTOS (cadar repositorio-monedas) transaccion) (validador-espacio-monedas-helper (cdr repositorio-monedas) transaccion)))) ; Llama recursivamente a la función para agrupar en una lista el número de coincidencias de cada moneda.

; Función que valida si la transacción es posible de realizar, tomando en cuenta la capacidad máxima de las monedas que hay dentro de la máquina.
(define (validador-espacio-monedas repositorio-monedas transaccion)
  (if (null? repositorio-monedas) ; Si está vacío el repositorio de monedas significa que se han hecho todas las comparaciones y NO se ha encontrado ninguna moneda desconocida.
      #true ; Regresa verdadero.
      (if (> (+ (caddar repositorio-monedas) (car (validador-espacio-monedas-helper repositorio-monedas transaccion))) (car (cdddar repositorio-monedas))) ; Hace la comparación para saber si la cantidad de dicha moneda que se encuentra en la secuencia de transicones va a superar el límite del repositorio.
          (~a "Error: Se ha superado la capacidad máxima de la moneda de " (cadar repositorio-monedas) " pesos.") ; Si se supera el límite del respositorio, se despliega este mensaje.
          (validador-espacio-monedas (cdr repositorio-monedas) transaccion)))) ; Llama recursivamente a la función.

; Falta definir que pasa si se pasa
; Función de transción que va haciendo la suma de las monedas introducidas por el usuario.
(define (transicion listaT precio repositorio-monedas origen codigo nuevo-archBD og-inventario-productos)
  (cond ((equal? (acepta? repositorio-monedas listaT) #true) ; Primero verifica si se acepta la secuencia de monedas.
      (cond ((= (+ origen (car listaT)) precio) (actualiza-inventario-productos codigo og-inventario-productos nuevo-archBD) ; En caso de que el pago sea exacto, solamente se actualiza el inventario 
                                                (write repositorio-monedas nuevo-archBD) ; Escribe el repositiorio de monedas.
                                                (close-output-port nuevo-archBD) ; Cierra el archivo
                                                "Transacción realizada con éxito. El pago fue exacto.") ; Despliega el mensaje de que la transacción fue exitosa.
            ((> (+ origen (car listaT)) precio) (actualiza-inventario-productos codigo og-inventario-productos nuevo-archBD) ; En caso de que se requiera dar cambio, primero se actualiza el inventario de productos.
                                                (actualiza-repositorio-monedas repositorio-monedas precio (+ origen (car listaT)) nuevo-archBD) ; Actualiza el repositorio de monedas.
                                                (~a "Transacción realizada con éxito. Su cambio fue de " (apply + (cambio repositorio-monedas precio (+ origen (car listaT)))) " pesos.")) ; Despliega el mensaje de que la transacción fue exitosa junto con su cambio.
            (else (transicion (cdr listaT) precio repositorio-monedas (+ origen (car listaT)) codigo nuevo-archBD og-inventario-productos)))) ; Llama recursivamente a la función para seguir haciendo las transiciones.
      (else (escribe-lo-mismo nuevo-archBD og-inventario-productos repositorio-monedas) ; En caso de que acepta? sea falso,
            (acepta? repositorio-monedas listaT)))) ; Hace la llamada para desplegar el mensaje correspondiente.

; Función que regresa el cambio. En esta función se utiliza mucho el comando de reverse para que primero tome en cuenta las monedas de más valor primero.
(define (cambio repositorio-monedas precio valor-pagado)
  (if (and (number? (miembro->= (- valor-pagado precio) (reverse (map cadr repositorio-monedas)))) ; Verifica cuales son las monedas que sean igual o mayor a la diferencia del valor pagado por el usuario en comparación con el precio del producto.
           (> (ACCESA-N (INDICE (miembro->= (- valor-pagado precio) (reverse (map cadr repositorio-monedas)))
                                (reverse (map cadr repositorio-monedas)))
                        (reverse (map caddr repositorio-monedas))) 0))
      (if (= (- valor-pagado (miembro->= (- valor-pagado precio) (reverse (map cadr repositorio-monedas)))) precio) ; Si con esta acción ya se acompletó el cambio...
          (cons (miembro->= (- valor-pagado precio) (reverse (map cadr repositorio-monedas))) '()) ; Valor final que regresa
          (cons (miembro->= (- valor-pagado precio) (reverse (map cadr repositorio-monedas))) ; Agrupa ese valor en una lista con los demás valores de monedas que se tuvieron que utilizar para el cambio.
                (cambio (reverse (reemplaza ; Llama recursivamente a la función de cambio tomando en cuenta que se debe actualizar el repositorio de monedas.
                                  (reemplaza ; Primero se reemplaza el valor de la cantidad de monedas disponibles de dicho valor correspondiente, antes de reemplazar en el repositorio con dicha sublista generada.
                                   (- (ACCESA-N (INDICE (miembro->= (- valor-pagado precio)
                                                                    (reverse (map cadr repositorio-monedas))) ; lista con valores de monedas de manera descenedente
                                                        (reverse (map cadr repositorio-monedas))) ; lista con valores de monedas de maneras descendente
                                                (reverse (map caddr repositorio-monedas))) ; lista con valores de la capacidad de las monedas en el mismo orden que la lista anterior
                                      1) ; Se resta 1
                                   3 ; Sustituye en la posición 3
                                   (ACCESA-N (INDICE (miembro->= (- valor-pagado precio) ; Toda esto indica que desea sustituir en la sublista donde se encuentra el valor de la moneda.
                                                                 (reverse (map cadr repositorio-monedas)))
                                                     (reverse (map cadr repositorio-monedas)))
                                             (reverse repositorio-monedas))
                                   null)
                                  (INDICE (miembro->= (- valor-pagado precio) ; En esta función se checa que coincida con la misma posición del valor de la moneda cuya cantidad se está actualizando.
                                                      (reverse (map cadr repositorio-monedas)))
                                          (reverse (map cadr repositorio-monedas)))
                                  (reverse repositorio-monedas)
                                  null))
                        precio ; El precio es igual.
                        (- valor-pagado (miembro->= (- valor-pagado precio) (reverse (map cadr repositorio-monedas))))))) ; Se actualiza la diferencia de cambio.
      (cambio (reverse (cdr (reverse repositorio-monedas))) precio valor-pagado))) ; Si no hay coincidencia sigue buscando recursivamente la moneda que esté disponible.

; Funciones de Escritura de Archivos

; Función que actualiza el repositorio de monedas. En esta función se utiliza mucho el comando de reverse para que primero tome en cuenta las monedas de más valor primero.
(define (actualiza-repositorio-monedas repositorio-monedas precio valor-pagado nuevo-archBD)
  (if (and (number? (miembro->= (- valor-pagado precio) (reverse (map cadr repositorio-monedas)))) ; Verifica cuales son las monedas que sean igual o mayor a la diferencia del valor pagado por el usuario en comparación con el precio del producto.
           (> (ACCESA-N (INDICE (miembro->= (- valor-pagado precio) (reverse (map cadr repositorio-monedas))) (reverse (map cadr repositorio-monedas))) (reverse (map caddr repositorio-monedas))) 0))
      (cond ((= (- valor-pagado (miembro->= (- valor-pagado precio) (reverse (map cadr repositorio-monedas)))) precio) ; Si con esta acción ya se acompletó el cambio...
          (write (reverse (reemplaza (reemplaza
                               (- (ACCESA-N (INDICE (miembro->= (- valor-pagado precio)
                                                                (reverse (map cadr repositorio-monedas))) ; lista con valores de monedas de manera descenedente
                                                    (reverse (map cadr repositorio-monedas))) ; lista con valores de monedas de maneras descendente
                                            (reverse (map caddr repositorio-monedas))) ; lista con valores de la capacidad de las monedas en el mismo orden que la lista anterior
                                  1) ; Se resta 1
                               3 ; Se reemplaza en la posición 3
                               (ACCESA-N (INDICE (miembro->= (- valor-pagado precio) ; Toda esto indica que desea sustituir en la sublista donde se encuentra el valor de la moneda.
                                                             (reverse (map cadr repositorio-monedas)))
                                                 (reverse (map cadr repositorio-monedas)))
                                         (reverse repositorio-monedas))
                               null)
                              (INDICE (miembro->= (- valor-pagado precio) ; En esta función se checa que coincida con la misma posición del valor de la moneda cuya cantidad se está actualizando.
                                                  (reverse (map cadr repositorio-monedas)))
                                      (reverse (map cadr repositorio-monedas)))
                              (reverse repositorio-monedas)
                              null)) nuevo-archBD) (close-output-port nuevo-archBD))
          (else (cons (miembro->= (- valor-pagado precio) (reverse (map cadr repositorio-monedas))) ; Si aún no se iguala la diferencia del precio y del valor pagado.
                (actualiza-repositorio-monedas (reverse (reemplaza
                                  (reemplaza
                                   (- (ACCESA-N (INDICE (miembro->= (- valor-pagado precio)
                                                                    (reverse (map cadr repositorio-monedas))) ; lista con valores de monedas de manera descenedente
                                                        (reverse (map cadr repositorio-monedas))) ; lista con valores de monedas de maneras descendente
                                                (reverse (map caddr repositorio-monedas))) ; lista con valores de la capacidad de las monedas en el mismo orden que la lista anterior
                                      1) ; Se resta 1
                                   3 ; Se reemplaza en la posición 3
                                   (ACCESA-N (INDICE (miembro->= (- valor-pagado precio) ; Toda esto indica que desea sustituir en la sublista donde se encuentra el valor de la moneda.
                                                                 (reverse (map cadr repositorio-monedas)))
                                                     (reverse (map cadr repositorio-monedas)))
                                             (reverse repositorio-monedas))
                                   null)
                                  (INDICE (miembro->= (- valor-pagado precio) ; En esta función se checa que coincida con la misma posición del valor de la moneda cuya cantidad se está actualizando.
                                                      (reverse (map cadr repositorio-monedas)))
                                          (reverse (map cadr repositorio-monedas)))
                                  (reverse repositorio-monedas)
                                  null))
                        precio ; El precio es igual.
                        (- valor-pagado (miembro->= (- valor-pagado precio) (reverse (map cadr repositorio-monedas)))) ; Se actualiza la diferencia de cambio.
                        nuevo-archBD)))) ; Se selecciona el archivo del que se está leeyendo.
      (actualiza-repositorio-monedas (reverse (cdr (reverse repositorio-monedas))) precio valor-pagado nuevo-archBD))) ; Si no hay coincidencia sigue buscando recursivamente la moneda que esté disponible.

; Función que actualiza el inventario de los productos.
(define (actualiza-inventario-productos codigo inventario-productos nuevo-archBD)
  (writeln (reemplaza (reemplaza (- (ACCESA-N (INDICE (miembro-valor codigo (map car inventario-productos)) (map car inventario-productos)) (map cadddr inventario-productos)) 1)
                        4 ; Se sustitye en la posición 4.
                        (ACCESA-N (INDICE (miembro-valor codigo (map car inventario-productos)) (map car inventario-productos)) inventario-productos) ; Indica que desea sustituir en la sublista donde se encuentra el valor del producto.
                        null)
             (INDICE (miembro-valor codigo (map car inventario-productos)) (map car inventario-productos)) ; En esta función se checa que coincida con la misma posición del valor del producto cuya cantidad se está actualizando.
             inventario-productos
             null) nuevo-archBD))

; Función que escribe la misma información que había leído originalmente.
(define (escribe-lo-mismo nuevo-archBD inventario-productos repositorio-monedas)
  (writeln inventario-productos nuevo-archBD)
  (write repositorio-monedas nuevo-archBD)
  (close-output-port nuevo-archBD))

; Funciones de Lectura de Archivos

; Input del nombre de los archivos
(printf "Introduce el nombre del archivo de la Bases de Datos: ")
(define nombre-archBD (read-line (current-input-port) 'return ))
(printf "\nIntroduce el nombre del archivo de las Transacciones: ")
(define nombre-archT (read-line (current-input-port) 'return ))

; Lee el archivo de transiciones
(define archT (open-input-file nombre-archT))
(define transacciones (read archT))
(close-input-port archT)

; Lee el archivo de Base de Datos con el inventario de productos y el repositorio de monedas
(define original-archBD (open-input-file nombre-archBD))
(define original-inventario-productos (read original-archBD))
(define original-repositorio-monedas (read original-archBD))
(close-input-port original-archBD)

 ;Función que ejecuta todas las funciones del codigo para que el proceso de las transacciones sea automático.
(define (inicia-operaciones transacciones num)
  (define nuevo-archBD (open-input-file nombre-archBD))
  (define nuevo-inventario-productos (read nuevo-archBD))
  (define nuevo-repositorio-monedas (read nuevo-archBD))
  (close-input-port nuevo-archBD)
  (cond ((null? transacciones) (cons "No hay más transacciones por realizar." '())) ; Si ya se completó el recorrido de todas las transacciones, se despliega este mensaje.
        (else (define actualiza-nuevo-archBD (open-output-file nombre-archBD #:exists `replace))
         (cons (~a "Transacción #" num) ; Etiqueta a la transacción.
                    (cons (verif-exis-prod nuevo-inventario-productos transacciones nuevo-repositorio-monedas actualiza-nuevo-archBD nuevo-inventario-productos) ; Inicia el proceso con la función de verificar producto.
                          (inicia-operaciones (cdr transacciones) (+ num 1)))))))

; Función que obtiene las ganancias totales después de ejecutar todas las transacciones.
(define (ganancia-obtenida original-inventario-productos final-inventario-productos)
  (if (and (null? original-inventario-productos) (null? final-inventario-productos)) ; Si ya se completó el recorrido de ambas listas.
      0 ; Regresa cero
      (if (< (car (cdddar final-inventario-productos)) (car (cdddar original-inventario-productos))) ; Compara si es menor la cantidad de productos depsués de finalizar las transacciones a cómo eran antes de ello.
          (+ (* (caddar original-inventario-productos) ; Si es menor la cantidad se suma la multiplicación de la cantidad de unidades de determinado producto por la diferencia entre la cantidad al inicio y la actual.
                (- (car (cdddar original-inventario-productos)) (car (cdddar final-inventario-productos))))
             (ganancia-obtenida (cdr original-inventario-productos) (cdr final-inventario-productos)))
          (ganancia-obtenida (cdr original-inventario-productos) (cdr final-inventario-productos))))) ; Ejecuta la función recursivamente si no se cumple con la condición

; Función que obtiene las productos que tienen pocas unidades restantes en el inventario.
(define (productos-poco-inventario inventario-productos)
  (if (null? inventario-productos) ; Si ya se completó el recorrido de la lista
      '() ; Regesa nulo
      (if (<= (car (cdddar inventario-productos)) 5) ; Hace la comparación si hay igual o menos de 5 unidades.
          (cons (append (list (caar inventario-productos)) (list (cadar inventario-productos))) (productos-poco-inventario (cdr inventario-productos))) ; Agrupa en una lista los coincidentes.
          (productos-poco-inventario (cdr inventario-productos))))) ; Sigue buscando recursivamente.

; Función que obtiene cuales son las monedas que ya están llenos o casi llenos su repositorio.
(define (monedas-casi-llenas-repositorio repositorio-monedas)
  (if (null? repositorio-monedas) ; Si ya se completó el recorrido de la lista
      '() ; Regesa nulo
      (if (>= (caddar repositorio-monedas) (* 0.8 (car (cdddar repositorio-monedas)))) ; Hace la comparación si hay igual o más del 80% de la capacidad maxima.
          (cons (cadar repositorio-monedas) (monedas-casi-llenas-repositorio (cdr repositorio-monedas))) ; Agrupa en una lista los coincidentes.
          (monedas-casi-llenas-repositorio (cdr repositorio-monedas))))) ; Sigue buscando recursivamente.

; Función que obtiene cuales son las monedas que ya están vacíos o casi vacíos su repositorio.
(define (pocas-monedas-repositorio repositorio-monedas)
  (if (null? repositorio-monedas) ; Si ya se completó el recorrido de la lista
      '() ; Regesa nulo
      (if (<= (caddar repositorio-monedas) (* 0.2 (car (cdddar repositorio-monedas)))) ; Hace la comparación si hay igual o menos del 20% de la capacidad maxima.
          (cons (cadar repositorio-monedas) (pocas-monedas-repositorio (cdr repositorio-monedas))) ; Agrupa en una lista los coincidentes.
          (pocas-monedas-repositorio (cdr repositorio-monedas))))) ; Sigue buscando recursivamente.

(inicia-operaciones transacciones 1) ; Procesa las transacciones

; Función que ejecuta las funciones necesarias para hacer el reporte final después de las transacciones.
(define (Reporte-Final)
  (define nuevo-archBD (open-input-file nombre-archBD))
  (define nuevo-inventario-productos (read nuevo-archBD))
  (define nuevo-repositorio-monedas (read nuevo-archBD))
  (close-input-port nuevo-archBD)
  (printf "\nREPORTE FINAL")
  (printf "\nGanancia total obtenida después de las transacciones: $~a" (ganancia-obtenida original-inventario-productos nuevo-inventario-productos))
  (printf "\nLista de productos cuyo inventario es poco o nulo (menor que 5 unidades) : ~a" (productos-poco-inventario nuevo-inventario-productos))
  (printf "\nLista de monedas cuyo respositorio está lleno o casi lleno (80% de capacidad o más): ~a" (monedas-casi-llenas-repositorio nuevo-repositorio-monedas))
  (printf "\nLista de monedas cuyo respositorio está vacío o casi vacío (20% de capacidad o menos): ~a" (pocas-monedas-repositorio nuevo-repositorio-monedas)))

(Reporte-Final) ; Ejecuta la función del reporte final.