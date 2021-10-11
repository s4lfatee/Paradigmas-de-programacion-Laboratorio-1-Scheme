#lang racket

;TDA Usuario
;Nivel 0: Representación
;El TDA usuario se compone de una lista que incluye el nombre de usuario, su contraseña, documentos creados, documentos compartidos y documentos accesibles

;Nivel 1: Constructor
;Todos los elementos anteriormente mencionados en el nivel 0, se almacenan en una lista
;Dominio: String
;Recorrido: Lista
;Recursión: No

(define (crearusuario nombreusuario contrasenha docscreados docscompartidos docsaccesibles estado)
  (list nombreusuario contrasenha docscreados docscompartidos docsaccesibles estado))

;Nivel 2: Pertenencia
;La única forma disponible por ahora para verificar el tda, es a través de su largo de lista, posiblemente después tenga otra solución al respecto
;Dominio: Lista
;Recorrido: Valor Booleano

(define (isusuario? usuario)
  (if (and (= (length usuario) 6)
           (list? usuario))
      #t
      #f
      )
  )

;Nivel 3: Selectores
;Funciones que permiten obtener elementos de la lista por si solos
;Dominio: Lista
;Recorrido: String

(define (getnombre usuario)
  (if (isusuario? usuario)
      (car usuario)
      null
      )
  )

(define (getcontrasenha usuario)
  (if (isusuario? usuario)
      (car (cdr usuario))
      null
      )
  )

(define (getdocscreados usuario)
  (if (isusuario? usuario)
      (car (cdr (cdr usuario)))
      null
      )
  )

(define (getdocscompartidos usuario)
  (if (isusuario? usuario)
      (car (cdr (cdr (cdr usuario))))
      null
      )
  )

(define (getdocsaccesibles usuario)
  (if (isusuario? usuario)
      (car (cdr (cdr (cdr (cdr usuario)))))
      null
      )
  )

(define (getestado usuario)
  (if (isusuario? usuario)
      (car (cdr (cdr (cdr (cdr (cdr usuario))))))
      null
      )
  )

;Nivel 4: Modificadores
;Funciones que modificarán los elementos de la lista dependiendo de la función
;Dominio: Lista
;Recorrido: Lista

(define (setnombreusuario usuario nuevonombre)
  (if (isusuario? usuario)
      (crearusuario nuevonombre (getcontrasenha usuario) (getdocscreados usuario) (getdocscompartidos usuario) (getdocsaccesibles usuario))
      usuario)
  )

(define (setcontrasenha usuario nuevacontrasenha)
  (if (isusuario? usuario)
      (crearusuario (getnombre usuario) nuevacontrasenha (getdocscreados usuario) (getdocscompartidos usuario) (getdocsaccesibles usuario))
      usuario)
  )

;Nivel 5: Operaciones

(define (isconectado? usuario)
  (if (string=? (getestado usuario) "conectado")
      #t
      #f
      )
  )
