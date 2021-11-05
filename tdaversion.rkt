#lang racket


;Nivel 1: Constructor
;Descripción: Función constructora del TDA versión
;Dominio: String X String X int
;Recorrido: version
;Recursión: No

(define (version nombredoc contenido numeroversion)
  (list nombredoc contenido numeroversion)
  )

;Nivel 3: Selectores

;Descripción: Función que obtiene el nombre de una versión
;Dominio: version
;Recorrido: String
;Recursión: No
(define (getnombredocver vrs)
  (list-ref vrs 0))

;Descripción: Función que obtiene el contenido de una versión
;Dominio: version
;Recorrido: String
;Recursión: No
(define (getcontenidover vrs)
  (list-ref vrs 1))

;Descripción: Función que obtiene el número de versión de una versión
;Dominio: version
;Recorrido: int
;Recursión: No
(define (getnumeroversion vrs)
  (list-ref vrs 2))

;Nivel 4: Modificadores

;Descripción: Función que establece un nuevo contenido en una versión
;Dominio: version X String
;Recorrido: version
;Recursión: No
(define (setcontenido vrs nuevocontent)
  (list (getnombredocver vrs) nuevocontent (getnumeroversion vrs))
  )

;Descripción: Función que establece un nuevo número de versión a una versión
;Dominio: version X int
;Recorrido: version
;Recursión: No
(define (setnumeroversion vrs nuevaversion)
  (list (getnombredocver vrs) (getcontenidover vrs) nuevaversion))

;Descripción: Función que incrementa el número de versión a una versión dependiendo del largo de la lista de versiones
;Dominio: version
;Recorrido: version
;Recursión: No
(define (incrementarnumeroversion vrs)
  (setnumeroversion vrs (- (+ (getnumeroversion vrs) (length vrs)) 1)))

(provide (all-defined-out))