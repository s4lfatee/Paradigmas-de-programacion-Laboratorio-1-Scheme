#lang racket
(require "tdaparadigmadocs.rkt")
(require "tdafecha.rkt")
(require "tdausuario.rkt")
(require "tdadocumento.rkt")
(require "tdaaccess.rkt")


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

(define (buscardocumento listadocumentos idDoc)
  (if (eq? null listadocumentos)
      #f
      (if (is-in-list (car listadocumentos) idDoc)
          #t
          (buscardocumento (cdr listadocumentos) idDoc)
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
  (setestado (setlistadocumentos paradigmadocs (cons (documento nombre date contenido (length (getlistadocs paradigmadocs))) (getlistadocs paradigmadocs))) null)
  )

(define (share paradigmadocs idDoc access . accesses)
  (if (buscardocumento (getlistadocs paradigmadocs) idDoc))
      (
  )
  



(define emptyGDocs (paradigmadocs "gDocs" (date 25 10 2021) encryptFn encryptFn))

(define gDocs1
(register (register (register emptyGDocs (date 25 10 2021) "user1" "pass1") (date 25 10 2021) "user2" "pass2") (date 25 10 2021) "user3" "pass3"))


(define gDocs2 ((login gDocs1 "user1" "pass1" create) (date 30 08 2021) "doc1" "contenido doc1"))

(define gDocs3 ((login gDocs2 "user2" "pass2" create) (date 30 08 2021) "doc2”" "contenido doc2"))

(define gDocs4 ((login gDocs2 "user3" "pass3" create) (date 30 08 2021) "doc3" "contenido doc3"))

;(define gDocs5 ((login gDocs4 "user1" "pass1" share) 1 (access "user2" #\r)))
