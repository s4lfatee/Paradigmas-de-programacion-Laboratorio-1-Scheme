#lang racket

;TDA USUARIO
;Nivel 0: Representación
;El TDA usuario se compone de una lista que incluye el nombre de usuario (string)
;su contraseña (string) y una fecha de creación del usuario (date)

;Nivel 1: Constructor

;Descripción: Función que construye el tda usuario
;Dominio: String X String X date
;Recorrido: user
;Recursión: No
(define (user nombreusuario contrasenha date)
  (list nombreusuario contrasenha date)
  )


;Nivel 3: Selectores

;Descripción: Función que obtiene el nombre de un usuario
;Dominio: user
;Recorrido: String
;Recursión: No
(define (getnombreuser usuario)
      (car usuario))

;Descripción: Función que obtiene la contraseña de un usuario
;Dominio: user
;Recorrido: String
;Recursión: No
(define (getcontrasenha usuario)
      (car (cdr usuario)))

;Descripción: Función que obtiene la fecha de creación de un usuario
;Dominio: user
;Recorrido: date
;Recursión: No
(define (getdate usuario)
      (car (cdr (cdr usuario))))


;Descripción: Función que obtiene el primer usuario de la lista de accesos de paradigmadocs
;Dominio: listausers
;Recorrido: user
;Recursión: No
(define (primerusuario listausuario)
  (car listausuario))

;Descripción: Función que obtiene un user tras ser utilizado en una función de tipo filter lambda (Donde es necesario aplicar un car debido a que se crean listas involuntariamente)
;Dominio: lista
;Recorrido: user
;Recursión: No
(define (getinfouser user)
  (car user))



;Nivel 4: Modificadores

;Descripción: Función que establece un nuevo username a un usuario
;Dominio: user X String
;Recorrido: user
;Recursión: No
(define (setnombreusuario usuario nuevonombre)
      (user nuevonombre (getcontrasenha usuario) (getdate usuario)))

;Descripción: Función que establece una nueva contraseña a un usuario
;Dominio: user X String
;Recorrido: user
;Recursión: No
(define (setcontrasenha usuario nuevacontrasenha)
      (user (getnombreuser usuario) nuevacontrasenha (getdate usuario)))

;Nivel 5

;Descripción: Función que transforma la información de un usuario a un string que se puede comprender a través de display
;Dominio: user
;Recorrido: String
;Recursión: No
(define (usertostring user)
  (string-join (list "Nombre de usuario:" (getnombreuser user) "\n" "Contraseña:" (getcontrasenha user) "\n" "Fecha de creación de la cuenta:" (string-join (map number->string (getdate user))) "\n------------------------\n")))


(provide (all-defined-out))