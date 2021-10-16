#lang racket

;TDA Documento
;Nivel 0; Representación
;EL TDA documento se compone de una lista que incluye titulo, creador, contenido, usuarios, usuarios online

;Nivel 1: Constructor
;Todos los elementos anteriormente mencionados en el nivel 0, se almacenan en una lista
;Dominio: String
;Recorrido: Lista
;Recursión: No

(define (creardocumento titulo creador contenido usuarios usuariosonline)
  (list titulo creador contenido usuarios usuariosonline))

;Nivel 2: Pertenencia
;Función que verifica que el documento tenga la cantidad de elementos especificados y que sea una lista
;Dominio: Lista
;Recorrido: Valor Booleano

(define (isdocumento? documento)
  (if (and (= (length documento) 5)
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
      (car documento)
      null
      )
  )

(define (getcreador documento)
  (if (isdocumento? documento)
      (car (cdr documento))
      null
      )
  )

(define (getcontenido documento)
  (if (isdocumento? documento)
      (car (cdr (cdr documento)))
      null
      )
  )

(define (getusuarios documento)
  (if (isdocumento? documento)
      (car (cdr (cdr (cdr documento))))
      null
      )
  )

(define (getusuariosonline documento)
  (if (isdocumento? documento)
      (car (cdr (cdr (cdr (cdr documento)))))
      null
      )
  )

;Nivel 4

(define (settitulo documento nuevotitulo)
  (if (isdocumento? documento)
      (creardocumento nuevotitulo (getcreador documento) (getcontenido documento) (getusuarios documento) (getusuariosonline documento))
      documento)
  )

(define (setcontenido documento nuevocontenido)
  (if (isdocumento? documento)
      (creardocumento (gettitulo documento) (getcreador documento) nuevocontenido (getusuarios documento) (getusuariosonline documento))
      documento)
  )

(provide (all-defined-out))