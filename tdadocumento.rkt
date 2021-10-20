#lang racket

;TDA Documento
;Nivel 0: Representación
;EL TDA documento se compone de una lista que incluye titulo, creador, contenido, usuarios, usuarios online

;Nivel 1: Constructor
;Todos los elementos anteriormente mencionados en el nivel 0, se almacenan en una lista
;Dominio: String
;Recorrido: Lista
;Recursión: No

(define (documento titulo date contenido id)
  (list titulo date contenido id '()))

;Nivel 2: Pertenencia
;Función que verifica que el documento tenga la cantidad de elementos especificados y que sea una lista
;Dominio: Lista
;Recorrido: Valor Booleano

(define (isdocumento? documento)
  (if (and (<= (length documento) 5)
           (list? documento))
      #t
      #f
      )
  )

;Nivel 3: Selectores
;Funciones que permiten obtener elementos de la lista
;Dominio: Lista
;Recorrido: String

(define (gettitulo documento)
  (if (isdocumento? documento)
      (list-ref documento 0)
      null
      )
  )

(define (getdatedocument documento)
  (if (isdocumento? documento)
      (list-ref documento 1)
      null
      )
  )

(define (getcontenido documento)
  (if (isdocumento? documento)
      (list-ref documento 2)
      null
      )
  )

(define (getid documento)
  (if (isdocumento? documento)
      (list-ref documento 3)
      null
      )
  )

(define (getaccesodocs documento)
  (if (isdocumento? documento)
      (list-ref documento 4)
      null
      )
  )


;Nivel 4

(define (settitulo documento nuevotitulo)
  (if (isdocumento? documento)
      (documento nuevotitulo (getdatedocument documento) (getcontenido documento))
      documento)
  )

(define (setcontenido documento nuevocontenido)
  (if (isdocumento? documento)
      (documento (gettitulo documento) (getdatedocument documento) nuevocontenido)
      documento)
  )

(define (setaccesodocs documento nuevoaccesodocs)
  (if (isdocumento? documento)
      (documento (gettitulo documento) (getdatedocument documento) (getcontenido documento) nuevoaccesodocs)
      documento)
  )

(provide (all-defined-out))