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

(define (getlistausers paradigmadocs)
  (list-ref paradigmadocs 4)
  )

(define (setlistausers paradigmadocs listausuarios)
  (list (list-ref paradigmadocs 0)
        (list-ref paradigmadocs 1)
        (list-ref paradigmadocs 2)
        (list-ref paradigmadocs 3)
        listausuarios
        (list-ref paradigmadocs 5))
  )

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

(define (login paradigmadocs username password operation)
  (


  
  

(define emptyGDocs (paradigmadocs "gDocs" (date 25 10 2021) encryptFn encryptFn))

(define gDocs1
(register (register (register emptyGDocs (date 25 10 2021) "user1" "pass1") (date 25 10 2021) "user1" "pass2") (date 25 10 2021) "user3" "pass3"))



