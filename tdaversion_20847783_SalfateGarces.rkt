#lang racket

(require "encryptfunction_20847783_SalfateGarces.rkt")

;TDA VERSIÓN
;Nivel 0: Representación
;Este TDA versión es representado a través de una lista que contiene el nombre del documento que ha sido modificado (string)
;el contenido resultante de la modificación (string), y un número que represente a la versión (int)

;Nivel 1: Constructor
;Descripción: Función constructora del TDA versión
;Dominio: String X date X String X int
;Recorrido: version
;Recursión: No

(define (version nombredoc fecha contenido numeroversion)
  (list nombredoc fecha contenido numeroversion)
  )

;Nivel 3: Selectores

;Descripción: Función que obtiene el nombre de una versión
;Dominio: version
;Recorrido: String
;Recursión: No
(define (getnombredocver vrs)
  (list-ref vrs 0))

;Descripción: Función que obtiene la fecha de una versión
;Dominio: version
;Recorrido: date
;Recursión: No
(define (getfechaver vrs)
  (list-ref vrs 1))

;Descripción: Función que obtiene el contenido de una versión
;Dominio: version
;Recorrido: String
;Recursión: No
(define (getcontenidover vrs)
  (list-ref vrs 2))

;Descripción: Función que obtiene el número de versión de una versión
;Dominio: version
;Recorrido: int
;Recursión: No
(define (getnumeroversion vrs)
  (list-ref vrs 3))

;Nivel 4: Modificadores

;Descripción: Función que establece un nuevo contenido en una versión
;Dominio: version X String
;Recorrido: version
;Recursión: No
(define (setcontenido vrs nuevocontent)
  (list (getnombredocver vrs) (getfechaver vrs) nuevocontent (getnumeroversion vrs))
  )

;Descripción: Función que establece un nuevo número de versión a una versión
;Dominio: version X int
;Recorrido: version
;Recursión: No
(define (setnumeroversion vrs nuevaversion)
  (list (getnombredocver vrs) (getfechaver vrs) (getcontenidover vrs) nuevaversion))

;Descripción: Función que incrementa el número de versión a una versión dependiendo del largo de la lista de versiones
;Dominio: version
;Recorrido: version
;Recursión: No
(define (incrementarnumeroversion vrs listaversiones)
  (setnumeroversion vrs (length listaversiones)))

;Descripción: Función que transforma la información de una versión a un string que se puede comprender a través de display
;Dominio: version
;Recorrido: String
;Recursión No
(define (versionstringlist version)
  (string-join (list "Nombre de documento:" (getnombredocver version) "Fecha de creación:" (string-join (map number->string (getfechaver version))) "Contenido de la versión:" (encryptFn (getcontenidover version)) "Número de versión:" (number->string (getnumeroversion version)) "\n//////////////\n")))

;Descripción: Función que actualiza la lista de versiones al hacer uso de restoreVersion
;Dominio: listaversions X int X listaversions
;Recorrido: listaversions
(define (actualizarlistaversiones listaversiones idVer nuevalista)
  (list-set listaversiones idVer nuevalista))

;Descripción: Función que obtiene una versión a partir de un id
;Dominio: listaversions X int
;Recorrido: version
;Recursión: No
(define (getversionid listaversiones id)
  (list-ref listaversiones (- (-(length listaversiones) id) 1))
  )

(provide (all-defined-out))
