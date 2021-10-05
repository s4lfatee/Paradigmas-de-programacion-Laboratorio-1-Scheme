#lang racket

;TDA ParadigmaDocs

;Nivel 0: Representación
;Este TDA representa el proyecto de laboratorio 1 de paradigma funcional, que contiene el nombre de la plataforma, una fecha, y dos strings que dependerán de la función que se debe implementar

;Nivel 1: Constructor
;Función que construye el TDA ParadigmaDocs
;Dominio:
;Recorrido:
;Recursión: No

(define (paradigmadocs name date encryptFunction decryptFunction)
  (list name date encryptFunction decryptFunction)
  )

;Nivel 2: Pertenencia
;Función que verifica el TDA
;Dominio: paradigmadocs
;Recorrido: Boolean

(define (isparadigmadocs? paradigmadocs)
  (if (string=? (car paradigmadocs) "paradigmadocs")
  #t
  #f
  )
  )

(define strings1 (paradigmadocs "paradigmadocs" "string1" "string2" "string3"))
