#lang racket

(require "tdadate_20847783_SalfateGarces.rkt")
(require "tdaparadigmadocs_20847783_SalfateGarces.rkt")
(require "tdaversion_20847783_SalfateGarces.rkt")
(require "tdaaccess_20847783_SalfateGarces.rkt")



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
  (if (isdocumento? documento)
      (list-ref documento 5)
      null
      )
  )

;Descripción: Función que obtiene la lista de versiones de un documento
;Dominio: documento
;Recorrido: listaversiones
;Recursión: No
(define (getversionesdocs documento)
      (list-ref documento 6))

;Nivel 4

;Descripción: Función que obtiene un documento de la lista de documentos a través de un ID
;Dominio: listadocs X int
;Recorrido: documento
;Recursión: No
(define (getdocumentoparadigma listadocs id)
  (list-ref listadocs (- (-(length listadocs) id) 1))
  )

;Descripción: Función que obtiene la última versión de un documento
;Dominio: documento
;Recorrido: version
;Recursión: No

(define (lastversion documento)
      (car (getversionesdocs documento))
      )

(define (getversionid listaversiones id)
  (list-ref listaversiones (- (-(length listaversiones) id) 1))
  )

(define (actualizarlistaversiones listaversiones idVer nuevalista)
  (list-set listaversiones idVer nuevalista))

;Nivel 4: Modificadores

;Descripción: Función que establece un nuevo contenido a un documento
;Dominio: documento X String
;Recorrido: documento
;Recursión: No
(define (setcontenidodocs documento nuevocontenido)
      (documento (gettitulo documento) (getdatedocument documento) nuevocontenido (getid documento (getowner documento) (getaccesodocs documento) (getversionesdocs documento))))

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

;Descripción: Función que verifica la existencia de un acceso en un documento
;Dominio: listaaccesos X username (String)
;Recorrido: Boolean
;Recursión: Recursión de cola
(define (verificaraccessuser lista valor)
 (cond
  [(empty? lista) false]
  [(eq? (first lista) valor) true]
  [else (verificaraccessuser (rest lista) valor)]))

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

;Descripción: Función que verifica la existencia de un documento a través de su ID
;Dominio: listadocs X int
;Recorrido: Boolean
;Recursión: Recursión de cola
(define (verificardocumentoid lista id)
 (cond
  [(empty? lista) false]
  [(eq? (first lista) id) true]
  [else (verificardocumentoid (rest lista) id)]))

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

;Descripción: Función que agrega un nuevo contenido y una nueva versión a un documento
;Dominio: paradigmadocs X String X date X int
;Recorrido: documento
;Recursión: Recursión de cola al hacer uso de la función verificardocumentoid
(define (agregarcontenido paradigmadocs newcontent id)
  (if (verificardocumentoid (getdocumentoparadigma (getlistadocs paradigmadocs) id) id)
      (if (null? (getversionesdocs (getdocumentoparadigma (getlistadocs paradigmadocs) id)))
          (setversionesdocs (getdocumentoparadigma (getlistadocs paradigmadocs) id)
                            (list (version (gettitulo (getdocumentoparadigma (getlistadocs paradigmadocs) id)) ((getencryptfn paradigmadocs)
                                                                                                                     (string-append ((getdecryptfn paradigmadocs)
                                                                                                                                     (getcontenido (getdocumentoparadigma (getlistadocs paradigmadocs) id))) newcontent)) 0)))
          (setversionesdocs (getdocumentoparadigma (getlistadocs paradigmadocs) id)
                            (cons (version (gettitulo (getdocumentoparadigma (getlistadocs paradigmadocs) id))
                                           ((getencryptfn paradigmadocs) (string-append ((getdecryptfn paradigmadocs) (getcontenidover (lastversion (getdocumentoparadigma (getlistadocs paradigmadocs) id)))) newcontent))
                                           (+ (getnumeroversion (lastversion (getdocumentoparadigma (getlistadocs paradigmadocs) id))) 1))
                                  (getversionesdocs (getdocumentoparadigma (getlistadocs paradigmadocs) id)))
                            )
          )
      paradigmadocs
      )
  )

;Descripción: Función que actualiza la lista de documento tras hacer uso de la función add
;Dominio: paradigmadocs X String X date X int
;Recorrido: paradigmadocs
;Recursión: Recursión de cola al utilizar la función verificardocumentoid
(define (agregarcontenidoporid paradigmadocs newcontent id)
  (list-set paradigmadocs 5 (map (lambda (docs)
         (if (verificardocumentoid (getdocumentoparadigma (getlistadocs paradigmadocs) (getid docs)) id)
             (agregarcontenido paradigmadocs newcontent (getid docs))
             docs
             )) (getlistadocs paradigmadocs))))

;Descripción: Función que genera una nueva lista con usuarios que tengan permisos de escritura y de comentarios
;Dominio: int X paradigmadocs
;Recorrido: listaaccesos
;Recursión: No
(define (getuserswithperms id paradigmadocs)
  (filter (lambda (acceso) (or (eq? (getpermiso acceso) #\w)
                               (eq? (getpermiso acceso) #\c)))
          (getaccesodocs (getdocumentoparadigma (getlistadocs paradigmadocs) id)))
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

(provide (all-defined-out))