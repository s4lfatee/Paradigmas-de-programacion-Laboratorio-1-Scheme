#lang racket

;TDA FECHA
;Nivel 0: Representación
;Este TDA representa una fecha, con un día, mes y año, y estos elementos se colocan dentro de una lista en el orden respectivo

;Nivel 1: Constructor
;Función que construye el TDA USUARIO
;Dominio: String
;Recorrido: Lista
;Recursión: No

(define (crearfecha dia mes anho)
  (list dia mes anho)
  )

;Nivel 2: Pertenencia
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
;Dominio: Lista
;Recorrido: Entero
;Recursión: No

(define (getdia fecha)
  (if (isfecha? fecha)
  (car fecha)
  null
  )
)

(define (getmes fecha)
  (if (isfecha? fecha)
  (car (cdr fecha))
  null
  )
)

(define (getanho fecha)
  (if (isfecha? fecha)
  (car (cdr (cdr fecha)))
  null
  )
)

;Nivel 4: Modificadores
;Dominio: Lista
;Recorrido: Lista
;Recursión: No

(define (setdia fecha newdia)
  (if (isfecha? fecha)
      (if (and (>= newdia 1)
               (<= newdia 31) 
               )
          (crearfecha newdia (getmes fecha) (getanho fecha))
          fecha)
      null
      )
  )

(define (setmes fecha newmes)
  (if (isfecha? fecha)
      (if (and (>= newmes 1)
               (<= newmes 12) 
               )
          (crearfecha (getdia fecha) newmes (getanho fecha))
          fecha)
      null
      )
  )

(define (setanho fecha newanho)
  (if (isfecha? fecha)
      (if (= newanho 2021) 
          (crearfecha (getdia fecha) (getmes fecha) newanho)
          fecha)
      null
      )
  )

