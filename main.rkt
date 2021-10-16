#lang racket
(require "tdaparadigmadocs.rkt")
(require "tdafecha.rkt")
(require "tdausuario.rkt")
(require "tdadocumento.rkt")



(define emptyGDocs (paradigmadocs "gDocs" (date 25 10 2021) encryptFn encryptFn))

(define gDocs1
(register (register (register emptyGDocs (date 25 10 2021) "user1" "pass1") (date 25 10 2021) "user2" "pass2") (date 25 10 2021) "user3" "pass3"))

;(define gDocs2 ((login gdocs1 “user1” “pass1” create) (date 30 08 2021) “doc0” “contenido doc0”))
