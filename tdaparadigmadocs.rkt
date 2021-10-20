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
  (list name date encryptFn decryptFn '() '() '())
  )

;Nivel 2: Pertenencia
;Función que verifica el TDA ParadigmaDocs
;Dominio: Paradigmadocs
;Recorrido:
;Recursión: No

(define (isparadigmadocs? paradigmadocs)
  (if (list? paradigmadocs)
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

(define (getencryptfn paradigmadocs)
  (if (isparadigmadocs? paradigmadocs)
      (list-ref paradigmadocs 2)
      null
  )
  )

(define (getdecryptfn paradigmadocs)
  (if (isparadigmadocs? paradigmadocs)
      (list-ref paradigmadocs 3)
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

(define (getestado paradigmadocs)
  (if (isparadigmadocs? paradigmadocs)
      (list-ref paradigmadocs 6)
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
        (getparadigmadate paradigmadocs)
        (getencryptfn paradigmadocs)
        (getdecryptfn paradigmadocs)
        (getlistausers paradigmadocs)
        (getlistadocs paradigmadocs)
        (getestado paradigmadocs))
  )

(define (setdate paradigmadocs newdate)
  (list (getplatformname paradigmadocs)
        newdate
        (getencryptfn paradigmadocs)
        (getdecryptfn paradigmadocs)
        (getlistausers paradigmadocs)
        (getlistadocs paradigmadocs)
        (getestado paradigmadocs))
  )

(define (setlistausers paradigmadocs listausuarios)
  (list (getplatformname paradigmadocs)
        (getparadigmadate paradigmadocs)
        (getencryptfn paradigmadocs)
        (getdecryptfn paradigmadocs)
        listausuarios
        (getlistadocs paradigmadocs)
        (getestado paradigmadocs))
  )

(define (setlistadocumentos paradigmadocs listadocumentos)
  (list (getplatformname paradigmadocs)
        (getparadigmadate paradigmadocs)
        (getencryptfn paradigmadocs)
        (getdecryptfn paradigmadocs)
        (getlistausers paradigmadocs)
        listadocumentos
        (getestado paradigmadocs))
  )

(define (setestado paradigmadocs nuevoestado)
  (list (getplatformname paradigmadocs)
        (getparadigmadate paradigmadocs)
        (getencryptfn paradigmadocs)
        (getdecryptfn paradigmadocs)
        (getlistausers paradigmadocs)
        (getlistadocs paradigmadocs)
        nuevoestado)
  )

(provide (all-defined-out))