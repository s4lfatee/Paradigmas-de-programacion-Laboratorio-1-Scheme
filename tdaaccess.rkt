#lang racket

;TDA Access
;Nivel 0: Representación
;El TDA Access se compone de una lista que contiene el nombre del usuario y el tipo de acceso otorgado

;Nivel 1: Constructor

(define (access user permiso)
  (list user permiso)
  )

;Nivel 2: Pertenencia

(define (isacceso? acceso)
  (if (list? acceso)
      #t
      #f
      )
  )

;Nivel 3: Selectores

(define (getuseraccess acceso)
  (if (isacceso? acceso)
      (list-ref acceso 0)
      null
      )
  )

(define (getpermiso acceso)
  (if (isacceso? acceso)
      (list-ref acceso 1)
      null
      )
  )

;Nivel 4: Modificadores

(define (setuseraccess acceso newuseraccess)
  (if (isacceso? acceso)
      (acceso newuseraccess (getpermiso acceso))
      acceso)
  )

(define (setpermiso acceso newperm)
  (if (isacceso? acceso)
      (acceso (getuseraccess acceso) newperm)
      acceso)
  )
      

(provide (all-defined-out))

