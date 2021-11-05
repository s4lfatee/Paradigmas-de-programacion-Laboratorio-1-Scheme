#lang racket
(require "tdadate_20847783_SalfateGarces.rkt")
(require "tdausuario_20847783_SalfateGarces.rkt")
(require "encryptfunction_20847783_SalfateGarces.rkt")


;TDA ParadigmaDocs

;Nivel 0: Representación
;Este TDA representa el proyecto de laboratorio 1 de paradigma funcional, que contiene el nombre de la plataforma, una fecha, y dos funciones para encriptar y desencriptar el contenido

;Nivel 1: Constructor
;Función que construye el TDA ParadigmaDocs
;Dominio: String X Date X EncryptFunction X DecryptFunction
;Recorrido: paradigmadocs
;Recursión: No

(define (paradigmadocs name date encryptFn decryptFn)
  (list name date encryptFn decryptFn '() '() '())
  )

;Nivel 2: Pertenencia
;Función que verifica el TDA ParadigmaDocs
;Dominio: Paradigmadocs
;Recorrido: Boolean
;Recursión: No

(define (isparadigmadocs? paradigmadocs)
  (if (list? paradigmadocs)
      #t
      #f
      )
  )

;Nivel 3: Selectores
;Funciones que permiten obtener elementos de la lista por si solos


;Descripción: Función que obtiene el nombre de la plataforma de un paradigmadocs
;Dominio: paradigmadocs
;Recorrido: String
;Recursión: No
(define (getplatformname paradigmadocs)
  (if (isparadigmadocs? paradigmadocs)
      (list-ref paradigmadocs 0)
      null
  )
  )

;Descripción: Función que obtiene la fecha de creación de paradigmadocs
;Dominio: paradigmadocs
;Recorrido: date
;Recursión: No
(define (getparadigmadate paradigmadocs)
  (if (isparadigmadocs? paradigmadocs)
      (list-ref paradigmadocs 1)
      null
  )
  )


;Descripción: Función que obtiene la función de encriptación asociada a paradigmadocs
;Dominio: paradigmadocs
;Recorrido: procedure
;Recursión: No
(define (getencryptfn paradigmadocs)
  (if (isparadigmadocs? paradigmadocs)
      (list-ref paradigmadocs 2)
      null
  )
  )

;Descripción: Función que obtiene la función de decriptación asociada a paradigmadocs
;Dominio: paradigmadocs
;Recorrido: procedure
;Recursión: No
(define (getdecryptfn paradigmadocs)
  (if (isparadigmadocs? paradigmadocs)
      (list-ref paradigmadocs 3)
      null
  )
  )

;Descripción: Función que obtiene la lista de users registrados en paradigmadocs
;Dominio: paradigmadocs
;Recorrido: listausers
;Recursión: No
(define (getlistausers paradigmadocs)
      (list-ref paradigmadocs 4)
  )


;Descripción: Función que obtiene la lista de documentos creados en paradigmadocs
;Dominio: paradigmadocs
;Recorrido: listadocs
;Recursión: No
(define (getlistadocs paradigmadocs)
  (if (isparadigmadocs? paradigmadocs)
      (list-ref paradigmadocs 5)
      null
  )
  )

;Descripción: Función que obtiene la lista de estado de sesión de paradigmadocs
;Dominio: paradigmadocs
;Recorrido: listaestado
;Recursión: No
(define (getestado paradigmadocs)
  (if (isparadigmadocs? paradigmadocs)
      (list-ref paradigmadocs 6)
      null
  )
  )

;Descripción: Función que obtiene el user logueado dentro de la lista de estado
;Dominio: listaestado
;Recorrido: string
;Recursión: No
(define (getuserlogueado estado)
  (car estado))

;Nivel 4: Modificadores
;Funciones que modificarán los elementos de la lista

;Descripción: Función que establece una nueva lista de usuarios
;Dominio: paradigmadocs x lista
;Recorrido: paradigmadocs
;Recursión: No
(define (setlistausers paradigmadocs listausuarios)
  (list (getplatformname paradigmadocs)
        (getparadigmadate paradigmadocs)
        (getencryptfn paradigmadocs)
        (getdecryptfn paradigmadocs)
        listausuarios
        (getlistadocs paradigmadocs)
        (getestado paradigmadocs))
  )

;Descripción: Función que establece una nueva lista de documentos
;Dominio: paradigmadocs x lista
;Recorrido: paradigmadocs
;Recursión: No
(define (setlistadocumentos paradigmadocs listadocumentos)
  (list (getplatformname paradigmadocs)
        (getparadigmadate paradigmadocs)
        (getencryptfn paradigmadocs)
        (getdecryptfn paradigmadocs)
        (getlistausers paradigmadocs)
        listadocumentos
        (getestado paradigmadocs))
  )

;Descripción: Función que establece una nueva lista de estado
;Dominio: paradigmadocs X lista
;Recorrido: paradigmadocs
;Recursión: No
(define (setestado paradigmadocs nuevoestado)
  (list (getplatformname paradigmadocs)
        (getparadigmadate paradigmadocs)
        (getencryptfn paradigmadocs)
        (getdecryptfn paradigmadocs)
        (getlistausers paradigmadocs)
        (getlistadocs paradigmadocs)
        nuevoestado)
  )

;Nivel 5


;Descripción: Función que agrega un usuario a la lista de estado trás loguearse
;Dominio: string X listaestado
;Recorrido: listaestado
;Recursión: No
(define (agregaruserlogueado usuario estado)
  (cons usuario estado))

;Descripción: Función que agrega un usuario a la lista de usuarios de un paradigmadocs
;Dominio: user X listausers
;Recorrido: listausers
;Recursión: No
(define (agregarusers usuario lista)
  (cons usuario lista)
  )

;Descripción: Función que verifica la existencia de un elemento en una lista
;Dominio: lista X any
;Recorrido: Boolean
;Recursión: Recursión de cola
(define (verificarenlista lista valor)
 (cond
  [(empty? lista) false]
  [(eq? (first lista) valor) true]
  [else (verificarenlista (rest lista) valor)]))


;Descripción: Función que valida la existencia de un usuario a través de su username y password
;Dominio: listausers X string X string
;Recorrido: Boolean
;Recursión: Recursión de cola
(define (buscarusuario listausuarios user password)
  (if (eq? null listausuarios)
      #f
      (if (and (verificarenlista (car listausuarios) user)
               (verificarenlista (car listausuarios) password))
          #t 
          (buscarusuario (cdr listausuarios) user password)
          )
      )
  )

;Descripción: Función que válida la existencia de un documento a través de su ID
;Dominio: listadocs X int
;Recorrido: Boolean
;Recursión: Recursión de cola
(define (buscardocumento listadocumentos idDoc)
  (if (eq? null listadocumentos)
      #f
      (if (verificarenlista (car listadocumentos) idDoc)
          #t
          (buscardocumento (cdr listadocumentos) idDoc)
          )
      )
  )

;Descripción: Función que verifica si un usuario ya ha sido registrado
;Dominio: listausers X user
;Recorrido: Boolean
;Recursión: Recursión de cola
(define (alreadyregistered? listausuarios user)
  (if (eq? null listausuarios)
      #f
      (if (string=? (getnombreuser (car listausuarios)) (getnombreuser user))
          #t
          (alreadyregistered? (cdr listausuarios) user)
      )
  )
  )

;Descripción: Función que agrega un nuevo documento a la lista de documentos de paradigmadocs
;Dominio: documento X listadocs
;Recorrido: listadocs
;Recursión: No
(define (agregardocumento doc listadocs)
  (cons doc listadocs))

;Descripción: Función que obtiene el largo de la lista de documentos de paradigmadocs
;Dominio: listadocs
;Recorrido: int
;Recursión: No
(define (largolistadocs lista)
  (length lista))

;Descripción: Función que desloguea al usuario conectado
;Dominio: listaestado
;Recorrido: listaestado
;Recursión: No
(define (desloguear estado)
  (setestado estado null))


(provide (all-defined-out))