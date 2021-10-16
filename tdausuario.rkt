#lang racket

;TDA Usuario
;Nivel 0: Representación
;El TDA usuario se compone de una lista que incluye el nombre de usuario, su contraseña, documentos creados, documentos compartidos y documentos accesibles

;Nivel 1: Constructor
;Todos los elementos anteriormente mencionados en el nivel 0, se almacenan en una lista
;Dominio: String
;Recorrido: Lista
;Recursión: No

(define (user nombreusuario contrasenha date)
  (list nombreusuario contrasenha date)
  )

;Nivel 2: Pertenencia
;La única forma disponible por ahora para verificar el tda, es a través de su largo de lista, posiblemente después tenga otra solución al respecto
;Dominio: Lista
;Recorrido: Valor Booleano

(define (isusuario? usuario)
  (if (and (= (length usuario) 3)
           (list? usuario))
      #t
      #f
      )
  )

;Nivel 3: Selectores
;Funciones que permiten obtener elementos de la lista por si solos
;Dominio: Lista
;Recorrido: String

(define (getnombre usuario)
  (if (isusuario? usuario)
      (car usuario)
      null
      )
  )

(define (getcontrasenha usuario)
  (if (isusuario? usuario)
      (car (cdr usuario))
      null
      )
  )

(define (getdate usuario)
  (if (isusuario? usuario)
      (car (cdr (cdr usuario)))
      null
      )
  )

;Nivel 4: Modificadores
;Funciones que modificarán los elementos de la lista
;Dominio: Lista
;Recorrido: Lista

(define (setnombreusuario usuario nuevonombre)
  (if (isusuario? usuario)
      (user nuevonombre (getcontrasenha usuario) (getdate usuario))
      usuario)
  )

(define (setcontrasenha usuario nuevacontrasenha)
  (if (isusuario? usuario)
      (user (getnombre usuario) nuevacontrasenha (getdate usuario))
      usuario)
  )


(provide (all-defined-out))