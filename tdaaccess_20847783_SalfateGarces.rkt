#lang racket
(require "tdausuario_20847783_SalfateGarces.rkt")
(require "tdaparadigmadocs_20847783_SalfateGarces.rkt")

;TDA ACCESS
;Nivel 0: Representación
;Este TDA que corresponde al TDA acceso tendrá una representación de una lista que contiene dos datos, el nombre del usuario en forma de string
;que corresponde al acceso y un character que represente el tipo de acceso otorgado

;Nivel 1: Constructor

;Descripción: Función constructora del tda access
;Dominio: String X Character
;Recorrido: access
;Recursión: No
(define (access user permiso)
  (list user permiso)
  )


;Nivel 3: Selectores

;Descripción: Función que obtiene el usuario de un acceso
;Dominio: acceso
;Recorrido: String
;Recursión: No
(define (getuseraccess acceso)
  (list-ref acceso 0))

;Descripción: Función que obtiene el permiso de un acceso
;Dominio: acceso
;Recorrido: Character
;Recursión: No
(define (getpermiso acceso)
  (list-ref acceso 1))


;Nivel 4: Modificadores

;Descripción: Función que establece un nuevo usuario en un acceso
;Dominio: acceso X String
;Recorrido: acceso
;Recursión: No
(define (setuseraccess acceso newuseraccess)
      (acceso newuseraccess (getpermiso acceso)))

;Descripción: Función que establece un nuevo permiso en un acceso
;Dominio: acceso X Character
;Recorrido: acceso
;Recursión: No
(define (setpermiso acceso newperm)
  (acceso (getuseraccess acceso) newperm))

      
  
;Nivel 5:

;Descripción: Función auxiliar para obtener el username de un acceso
;Dominio: listaaccess
;Recorrido: String
;Recursión: No
(define (getallusers listaaccess)
  (getuseraccess listaaccess))

;Descripción: Función que aplica la función getallusers a todos los elementos de una lista de accesos
;Dominio: listaaccess
;Recorrido: listadeusers
;Recursión: No
(define (getalluserswithperms lista)
  (map getallusers lista))

;Descripción: Función que busca la existencia de usuarios con múltiples accesos
;Dominio: listaaccess X user (String)
;Recorrido: Boolean
;Recursión: Recursión de cola
(define (verificarexistenciauser lista valor)
 (cond
  [(empty? lista) false]
  [(eq? (first lista) valor) true]
  [else (verificarexistenciauser (rest lista) valor)]))

;Descripción: Función que aplica la función getuseraccess a una lista de accesos
;Dominio: procedure X listaccesos
;Recorrido: listusers
;Recursión: No
(define (generateuserslist getuseraccess accesos)
  (map getuseraccess accesos))

;Descripción: Función que actualiza la lista de accesos de un documento en paradigmadocs
;Dominio: listadocs X int X newlistaaccess
;Recorrido: listadocs
;Recursión: No
(define (writevalidacceses lista id listanueva)
  (list-set lista id listanueva))

;Descripción: Función que une accesos con accesos validados
;Dominio: listaccess X listvalidaccess
;Recorrido: listaccess
;Recursión: No
(define (uniraccesos accesos accesosfiltrados)
  (cons accesos accesosfiltrados))

;Descripción: Función que valida los accesos verificando si existen
;Dominio: listaccess X listausers
;Recorrido: listaacess
;Recursión: Recursión de cola al hacer uso de verificarexistenciauser
(define (overwriteaccess listaccess listausers)
  (if (eq? listaccess null)
      null
      (if (verificarexistenciauser listausers (getuseraccess (car listaccess)))
          (cdr listaccess)
          (overwriteaccess (cdr listaccess) listausers)
          )
      )
  )

;Descripción: Función que genera una lista de accesos con los accesos válidos
;Dominio: paradigmadocs X listaccess X String
;Recorrido: listaccess
;Recursión: Recursión de cola al hacer uso de la función verificarexistenciauser
(define (removeaccess paradigmadocs listaccess owner)
  (filter (lambda (perm)
            (and (not (eqv? (getuseraccess perm) owner)) (verificarexistenciauser (map getnombreuser (getlistausers paradigmadocs)) (getuseraccess perm)))
          )listaccess))



(provide (all-defined-out))