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
  (if (and (and (= (length paradigmadocs) 7)
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
  (list-ref paradigmadocs 0)
  )

(define (getparadigmadate paradigmadocs)
  (list-ref paradigmadocs 1)
  )

(define (getencryptfn paradigmadocs)
  (list-ref paradigmadocs 2)
  )

(define (getdecryptfn paradigmadocs)
  (list-ref paradigmadocs 3)
  )

(define (getlistausers paradigmadocs)
  (list-ref paradigmadocs 4)
  )

(define (getlistadocs paradigmadocs)
  (list-ref paradigmadocs 5)
  )

(define (getestado paradigmadocs)
  (list-ref paradigmadocs 6)
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
        (list-ref paradigmadocs 5)
        (list-ref paradigmadocs 6))
  )

(define (setlistausers paradigmadocs listausuarios)
  (list (list-ref paradigmadocs 0)
        (list-ref paradigmadocs 1)
        (list-ref paradigmadocs 2)
        (list-ref paradigmadocs 3)
        listausuarios
        (list-ref paradigmadocs 5)
        (list-ref paradigmadocs 6))
  )

(define (setlistadocumentos paradigmadocs listadocumentos)
  (list (list-ref paradigmadocs 0)
        (list-ref paradigmadocs 1)
        (list-ref paradigmadocs 2)
        (list-ref paradigmadocs 3)
        (list-ref paradigmadocs 4)
        listadocumentos
        (list-ref paradigmadocs 6))
  )

(define (setestado paradigmadocs nuevoestado)
  (list (list-ref paradigmadocs 0)
        (list-ref paradigmadocs 1)
        (list-ref paradigmadocs 2)
        (list-ref paradigmadocs 3)
        (list-ref paradigmadocs 4)
        (list-ref paradigmadocs 5)
        nuevoestado
        )
  )

;Nivel 5:
;Dominio:
;Recorrido:
;Recursión

(define (alreadyregistered? listausuarios user)
  (if (eq? null listausuarios)
      #f
      (if (string=? (getnombreuser (car listausuarios)) (getnombreuser user))
          #t
          (alreadyregistered? (cdr listausuarios) user)
      )
  )
  )

(define (is-in-list lista value)
 (cond
  [(empty? lista) false]
  [(eq? (first lista) value) true]
  [else (is-in-list (rest lista) value)]))

(define (register paradigmadocs date username password)
  (if (alreadyregistered? (getlistausers paradigmadocs) (user username password date))
      paradigmadocs
      (setlistausers paradigmadocs (cons (user username password date) (getlistausers paradigmadocs)))
      )
  )

(define (buscarusuario listausuarios user password)
  (if (eq? null listausuarios)
      #f
      (if (and (is-in-list (car listausuarios) user)
               (is-in-list (car listausuarios) password))

          #t
          (buscarusuario (cdr listausuarios) user password)
          )
      )
  )

                        
(define (login paradigmadocs username password operation)
  (define logged (setestado paradigmadocs (cons (cons username (cons "connected" null)) (getestado paradigmadocs))))
  (if (buscarusuario (getlistausers paradigmadocs) username password)
      (cond
        [(eq? operation create) (lambda (date nombre contenido)(operation logged date nombre contenido))])
      operation
      )
  )

(define (create paradigmadocs date nombre contenido)
  (setestado (setlistadocumentos paradigmadocs (cons (documento nombre date contenido) (getlistadocs paradigmadocs))) null)
  )



(provide (all-defined-out))