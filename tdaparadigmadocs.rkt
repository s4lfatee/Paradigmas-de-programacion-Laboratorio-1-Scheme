#lang racket
(require "tdafecha.rkt")
(require "tdausuario.rkt")
(require "tdadocumento.rkt")

;TDA ParadigmaDocs

;Nivel 0: Representación
;Este TDA representa el proyecto de laboratorio 1 de paradigma funcional, que contiene el nombre de la plataforma, una fecha, y dos funciones para encriptar y desencriptar el contenido

;Nivel 1: Constructor
;Función que construye el TDA ParadigmaDocs
;Dominio:
;Recorrido:
;Recursión: No

(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))

(define (paradigmadocs name date encryptFn decryptFn)
  (list name date encryptFn decryptFn '() '())
  )

;Nivel 2: Pertenencia
;Función que verifica el TDA ParadigmaDocs
;Dominio:
;Recorrido:
;Recursión: No

(define (isparadigmadocs? paradigmadocs)
  (if (and (and (= (length paradigmadocs) 6)
                (list? paradigmadocs)))
      #t
      #f
      )
  )

;Nivel 3: Selectores
;Funciones que permiten obtener elementos de la lista por si solos
;Dominio:
;Recorrido:
;Recursión:

(define (getplatformname paradigmadocs)
  (if (isparadigmadocs? paradigmadocs)
      (list-ref paradigmadocs 0)
      null
      )
  )

(define (getparadigmadate paradigmadocs)
  (if (isparadigmadocs? paradigmadocs)
      (list-ref paradigmadocs 1)
      null
      )
  )

(define (getlistausers paradigmadocs)
  (if (isparadigmadocs? paradigmadocs)
      (list-ref paradigmadocs 4)
      null
      )
  )

(define (getlistadocs paradigmadocs)
  (if (isparadigmadocs? paradigmadocs)
      (list-ref paradigmadocs 5)
      null
      )
  )

;Nivel 4: Modificadores
;Funciones que modificarán los elementos de la lista
;Dominio:
;Recorrido:
;Recursión:

(define (setplatformname paradigmadocs platformname)
  (list platformname
        (list-ref paradigmadocs 1)
        (list-ref paradigmadocs 2)
        (list-ref paradigmadocs 3)
        (list-ref paradigmadocs 4)
        (list-ref paradigmadocs 5))
  )

(define (setdate paradigmadocs newdate)
  (list (list-ref paradigmadocs 0)
        newdate
        (list-ref paradigmadocs 2)
        (list-ref paradigmadocs 3)
        (list-ref paradigmadocs 4)
        (list-ref paradigmadocs 5))
  )

(define (setlistausers paradigmadocs listausuarios)
  (list (list-ref paradigmadocs 0)
        (list-ref paradigmadocs 1)
        (list-ref paradigmadocs 2)
        (list-ref paradigmadocs 3)
        listausuarios
        (list-ref paradigmadocs 5))
  )

;Nivel 5:
;Dominio:
;Recorrido:
;Recursión

(define (alreadyregistered? listausuarios user)
  (if (eq? null listausuarios)
      #f
      (if (string=? (getnombre (car listausuarios)) (getnombre user))
          #t
          (alreadyregistered? (cdr listausuarios) user)
      )
  )
  )

(define (register paradigmadocs date username password)
  (if (alreadyregistered? (getlistausers paradigmadocs) (user username password date))
      paradigmadocs
      (setlistausers paradigmadocs (cons (user username password date) (getlistausers paradigmadocs)))
      )
  )

;(define (login paradigmadocs username password operation)
 ; (
  

(provide (all-defined-out))