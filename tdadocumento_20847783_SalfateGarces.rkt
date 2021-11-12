#lang racket

(require "tdadate_20847783_SalfateGarces.rkt")
(require "tdaversion_20847783_SalfateGarces.rkt")
(require "tdaaccess_20847783_SalfateGarces.rkt")
(require "encryptfunction_20847783_SalfateGarces.rkt")


;TDA DOCUMENTO
;Nivel 0: Representación
;Este TDA corresponde a un documento, cuya representación consiste en una lista que contiene
;un título (string), una fecha (date), contenido (string) un id (int), un dueño de documento y dos listas vacías representando
;una lista donde se almacenan los accesos de un documento y otra lista donde se almacenan las versiones
;ambos accesos y versiones corresponden a otro TDA aparte a este.

;Nivel 1: Constructor

;Descripción: Función constructora del tda documento
;Dominio: String X date X String X int X String
;Recorrido: documento
;Recursión: No
(define (documento titulo date contenido id owner)
  (list titulo date contenido id owner '() '()))


;Nivel 2: Pertenencia
;Descripción: Función que valida el tda documento
;Dominio: documento
;Recorrido: Boolean
;Recursión: No
(define (isdocumento? documento)
  (if (and (<= (length documento) 8)
           (list? documento))
      #t
      #f
      )
  )

;Nivel 3: Selectores

;Descripción: Función que obtiene el título de un documento
;Dominio: documento
;Recorrido: String
;Recursión: No
(define (gettitulo documento)
      (list-ref documento 0))

;Descripción: Función que obtiene la fecha de creación de un documento
;Dominio: documento
;Recorrido: date
;Recursión: No
(define (getdatedocument documento)
      (list-ref documento 1))
  
;Descripción: Función que obtiene el contenido de un documento
;Dominio: documento
;Recorrido: String
;Recursión: No
(define (getcontenido documento)
      (list-ref documento 2))

;Descripción: Función que obtiene el ID de un documento
;Dominio: documento
;Recorrido: int
;Recursión: No
(define (getid documento)
  (if (isdocumento? documento)
      (list-ref documento 3)
      null
      )
  )

;Descripción: Función que obtiene el dueño de un documento
;Dominio: documento
;Recorrido: String
;Recursión: No
(define (getowner documento)
  (list-ref documento 4))

;Descripción: Función que obtiene la lista de accesos de un documento
;Dominio: documento
;Recorrido: listaaccess
;Recursión: No
(define (getaccesodocs documento)
      (list-ref documento 5)
  )

;Descripción: Función que obtiene la lista de versiones de un documento
;Dominio: documento
;Recorrido: listaversiones
;Recursión: No
(define (getversionesdocs documento)
      (list-ref documento 6))


;Descripción: Función que obtiene la última versión de un documento
;Dominio: documento
;Recorrido: version
;Recursión: No
(define (lastversion documento)
      (car (getversionesdocs documento))
      )

;Descripción: Función que obtiene el contenido de un documento dependiendo de su última versión
;Dominio: documento
;Recorrido: String
;Recursión: No
(define (getcontentlastversion documento)
  (if (eq? null (getversionesdocs documento))
      (getcontenido documento)
      (getcontenidover (lastversion documento))))

;Descripción: Función que obtiene todos los documentos de un usuario a partir de un nombre de usuario
;Dominio: String X listadocs
;Recorrido: listadocs
;Recursión: No
(define (finduserdocs username listadocs)
  (filter (lambda (docs)
            (eqv? (getowner docs) username)) listadocs))

;Descripción: Función que obtiene documentos donde un usuario en especifico tiene accesos a partir de un username
;Dominio: String X listadocs
;Recorrido: listadocs
;Recursión: No
(define (finduserdocsaccess username listadocs)
  (filter (lambda (docs)
            (verificaraccessuser (map getuseraccess (getaccesodocs docs)) username)) listadocs))

;Nivel 4: Modificadores

;Descripción: Función que establece un nuevo contenido a un documento
;Dominio: documento X String
;Recorrido: documento
;Recursión: No
(define (setcontenidodocs documento nuevocontenido)
      (list (gettitulo documento) (getdatedocument documento) nuevocontenido (getid documento) (getowner documento) (getaccesodocs documento) (getversionesdocs documento)))

;Descripción: Función que establece una nueva lista de accesos en un documento
;Dominio: documento X listadeaccesos
;Recorrido: documento
;Recursión: No
(define (setaccesodocs documento nuevoaccesodocs)
      (list (gettitulo documento) (getdatedocument documento) (getcontenido documento) (getid documento) (getowner documento) nuevoaccesodocs (getversionesdocs documento)))

;Descripción: Función que establece una nueva lista de versiones en un documento
;Dominio: documento X listadeversiones
;Recorrido: documento
;Recursión: No
(define (setversionesdocs documento nuevasver)
  (if (isdocumento? documento)
      (list (gettitulo documento) (getdatedocument documento) (getcontenido documento) (getid documento) (getowner documento) (getaccesodocs documento) nuevasver)
      documento)
  )


;Nivel 5

;Descripción: Función que verifica la existencia de un acceso en la lista de accesos de un documento
;Dominio: listaaccesos X username (String)
;Recorrido: Boolean
;Recursión: Recursión de cola
(define (verificaraccessuser lista valor)
 (cond
  [(empty? lista) false]
  [(eq? (first lista) valor) true]
  [else (verificaraccessuser (rest lista) valor)]))

;Descripción: Función que verifica la existencia de un documento a través de su ID
;Dominio: listadocs X int
;Recorrido: Boolean
;Recursión: Recursión de cola
(define (verificardocumentoid lista id)
 (cond
  [(empty? lista) false]
  [(eq? (first lista) id) true]
  [else (verificardocumentoid (rest lista) id)]))

;Descripción: Función que genera una lista con todos los accesos
;Dominio: access X accesses (lista)
;Recorrido: listadeaccesses
;Recursión: No
(define (createlistadeaccesos acceso accesos)
  (cons acceso accesos))

;Descripción: Función que consigue el primer acceso de una lista de accesos
;Dominio: listaccesos
;Recorrido: access
;Recursión: No
(define (getprimeracceso listaccesos)
  (car listaccesos))



;Descripción: Función que verifica la existencia de un usuario de una lista de accesos
;Dominio: listaccess X username (String)
;Recorrido: Boolean
;Recursión: Recursión de cola
(define (verificaruserwithaccess lista valor)
 (cond
  [(empty? lista) false]
  [(eq? (first lista) valor) true]
  [else (verificaruserwithaccess (rest lista) valor)]))

;Descripción: Función que elimina todos los accesos que pertenezcan a un usuario
;Dominio: listadocs X username (String)
;Recorrido: listadocs
;Recursión: No
(define (deleteacceses docs user)
  (map (lambda (documento)
         (if (string=? (getowner documento) user)
             (setaccesodocs documento null)
             documento)) docs))

;Descripción: Función que obtiene un documento de la lista de documentos a través de un ID
;Dominio: listadocs X int
;Recorrido: documento
;Recursión: No
(define (getdocumentoparadigma listadocs id)
  (list-ref listadocs (- (-(length listadocs) id) 1))
  )

;Descripción: Función que actualiza la lista de documento tras hacer uso de la función add
;Dominio: paradigmadocs X String X date X int
;Recorrido: paradigmadocs
;Recursión: Recursión de cola al utilizar la función verificardocumentoid, la cual verifica si un documento coincide con el id entregado
(define (agregarcontenidoporid listdocs encryptfn decryptfn newcontent id fecha)
  (map (lambda (docs)
         (if (verificardocumentoid (getdocumentoparadigma listdocs (getid docs)) id)
             (agregarcontenido listdocs encryptfn decryptfn newcontent (getid docs) fecha)
             docs
             )) listdocs))

;Descripción: Función que genera una nueva lista con usuarios que tengan permisos de escritura y de comentarios
;Dominio: int X paradigmadocs
;Recorrido: listaaccesos
;Recursión: No
(define (getuserswithperms id listadocs)
  (filter (lambda (acceso) (or (eq? (getpermiso acceso) #\w)
                               (eq? (getpermiso acceso) #\c)))
          (getaccesodocs (getdocumentoparadigma listadocs id)))
  )

;Descripción: Función que actualiza el documento afecta restoreVersion
;Dominio: listadocs X int x list
;Recorrido: listadocs
;Recursión: No
(define (actualizardocumento listadocs idDoc listanueva)
  (list-set listadocs idDoc listanueva))

;Descripción: Función que agrega la versión que se necesita ser restaurada a la lista de versiones
;Dominio: version X listaversiones
;Recorrido: listaversiones
;Recursión: No
(define (agregarversionrestaurada nuevaversion listaversiones)
  (append (list nuevaversion) listaversiones))

;Descripción: Función que obtiene el largo de la lista de versiones de un documento
;Dominio: listaversiones
;Recorrido: int
;Recursión: No
(define (getlargoversiones listaversiones)
  (length listaversiones))

;Descripción: Función que transforma la información de un documento a un string que un usuario pueda comprender a través de display
;Dominio: documento
;Recorrido: String
;Recursión: No
(define (documentstringlist documento)
  (string-join (list "Nombre de documento:" (gettitulo documento) "\n" "Fecha de creación de documento:" (string-join (map number->string (getdatedocument documento)))"\n""Contenido del documento:" (encryptFn (getcontentlastversion documento))"\n""ID del documento:" (number->string (getid documento))"\n""Dueño del documento:" (getowner documento)"\n" "Accesos del documento:" (string-join (map accessestostringlist (getaccesodocs documento))) "\n""Versiones del documento:\n" (string-join (map versionstringlist (getversionesdocs documento))) "\n------------------------\n\n")))

;Descripción: Función que agrega un nuevo contenido y una nueva versión a un documento
;Dominio: paradigmadocs X String X date X int
;Recorrido: documento
;Recursión: Recursión de cola al hacer uso de la función verificardocumentoid, la cual verifica si un documento coincide con el id entregado
(define (agregarcontenido listdocs encryptfn decryptfn newcontent id fecha)
  (if (verificardocumentoid (getdocumentoparadigma listdocs id) id)
      (if (null? (getversionesdocs (getdocumentoparadigma listdocs id)))
          (setcontenidodocs (setversionesdocs (getdocumentoparadigma listdocs id)
                            (list (version (gettitulo (getdocumentoparadigma listdocs id)) fecha (encryptfn
                                                                                                                     (string-append (decryptfn
                                                                                                                                     (getcontenido (getdocumentoparadigma listdocs id))) newcontent)) 1)
                                  (version (gettitulo (getdocumentoparadigma listdocs id)) (getdatedocument (getdocumentoparadigma listdocs id)) (getcontenido (getdocumentoparadigma listdocs id)) 0))) (encryptfn (string-append (decryptfn (getcontenido (getdocumentoparadigma listdocs id))) newcontent)))
          (setcontenidodocs (setversionesdocs (getdocumentoparadigma listdocs id)
                            (cons (version (gettitulo (getdocumentoparadigma listdocs id)) fecha
                                           (encryptfn (string-append (decryptfn (getcontenidover (lastversion (getdocumentoparadigma listdocs id)))) newcontent))
                                           (+ (getnumeroversion (lastversion (getdocumentoparadigma listdocs id))) 1))
                                  (getversionesdocs (getdocumentoparadigma listdocs id)))
                            ) (encryptfn (string-append (decryptfn (getcontenido (getdocumentoparadigma listdocs id))) newcontent)))
          )
      listdocs
      )
  )

(provide (all-defined-out))