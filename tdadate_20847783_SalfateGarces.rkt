#lang racket

;TDA DATE
;Nivel 0: Representación
;Este TDA representa una lista con tres números enteros un día, mes y año, representando así una fecha

;Nivel 1: Constructor
;Función que construye el tda date

;Dominio: int X int X int
;Recorrido: date
;Recursión: No
(define (date dia mes anho)
  (list dia mes anho)
  )

;Nivel 2: Pertenencia

;Descripción: Función que valida el tda date
;Dominio: Lista
;Recorrido: Valor Booleano
;Recursión: No

(define (isfecha? fecha)
  (if (and (and (>= (car fecha) 1) (<= (car fecha) 31))
           (and (>= (car (cdr fecha)) 1) (<= (car (cdr fecha)) 12))
           (= (car (cdr (cdr fecha))) 2021)
           )
           #t
           #f
           )
  )


;Nivel 3: Selectores


;Descripción: Función que obtiene el día de una fecha
;Dominio: date
;Recorrido: int
;Recursión: No
(define (getdia fecha)
  (if (isfecha? fecha)
  (car fecha)
  null
  )
)

;Descripción: Función que obtiene el mes de una fecha
;Dominio: date
;Recorrido: int
;Recursión: No
(define (getmes fecha)
  (if (isfecha? fecha)
  (car (cdr fecha))
  null
  )
)

;Descripción: Función que obtiene el año de una fecha
;Dominio: date
;Recorrido: int
;Recursión: No
(define (getanho fecha)
  (if (isfecha? fecha)
  (car (cdr (cdr fecha)))
  null
  )
)

;Nivel 4: Modificadores

;Descripción: Función que establece un nuevo día en una fecha
;Dominio: date X int
;Recorrido: date
;Recursión: No
(define (setdia fecha newdia)
  (if (isfecha? fecha)
      (if (and (>= newdia 1)
               (<= newdia 31) 
               )
          (date newdia (getmes fecha) (getanho fecha))
          fecha)
      null
      )
  )

;Descripción: Función que establece un nuevo mes en una fecha
;Dominio: date X int
;Recorrido: date
;Recursión: No
(define (setmes fecha newmes)
  (if (isfecha? fecha)
      (if (and (>= newmes 1)
               (<= newmes 12) 
               )
          (date (getdia fecha) newmes (getanho fecha))
          fecha)
      null
      )
  )

;Descripción: Función que establece un nuevo año en una fecha
;Dominio: fecha X int
;Recorrido: fecha
;Recursión: No
(define (setanho fecha newanho)
  (if (isfecha? fecha)
      (if (= newanho 2021) 
          (date (getdia fecha) (getmes fecha) newanho)
          fecha)
      null
      )
  )

(provide (all-defined-out))