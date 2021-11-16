#lang racket

;Descripción: Función que encripta un string de forma de que el texto quede invertido, esta función la provee el documento de laboratorio del proyecto
;Dominio: String
;Recorrido: String
;Recursión: No
(define encryptFunction (lambda (s) (list->string (reverse (string->list s)))))

;Descripción: Función que encripta un string de forma de que el texto quede encerrado entre dos cadenas de λλλλλλ
;Dominio: String
;Recorrido: String
;Recursión: No
(define (encryptFn texto)
  (list->string (append (append (list #\λ #\λ #\λ #\λ #\λ #\λ) (string->list texto)) (list #\λ #\λ #\λ #\λ #\λ #\λ))))

;Descripción: Función que desencripta el texto encriptado por encryptFn
;Dominio: String
;Recorrido: String
;Recursión: No
(define (decryptFn texto)
  (list->string (filter (lambda (character)
            (not (eq? character #\λ))) (string->list texto))))

(provide (all-defined-out))