#lang racket

;Descripción: Función que encripta un string de forma de que el texto quede invertido
;Dominio: String
;Recorrido: String
;Recursión: No
(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))

(provide (all-defined-out))