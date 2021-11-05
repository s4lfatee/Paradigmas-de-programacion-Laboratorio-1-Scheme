#lang racket

(require "tdaparadigmadocs.rkt")
(require "tdadate.rkt")
(require "tdausuario.rkt")
(require "tdadocumento.rkt")
(require "tdaaccess.rkt")
(require "tdaversion.rkt")
(require "encryptfunction.rkt")

;Descripción: Función que registra usuarios en paradigmadocs
;Dominio: paradigmadocs X date X String X String
;Recorrido: paradigmadocs
;Recursión: Recursión de cola
(define (register paradigmadocs date username password)
  (if (empty? (getlistausers paradigmadocs))
      (setlistausers paradigmadocs (cons (user username password date) (getlistausers paradigmadocs)))
      (if (alreadyregistered? (getlistausers paradigmadocs) (user username password date))
          (setlistausers paradigmadocs (getlistausers paradigmadocs))
          (setlistausers paradigmadocs (agregarusers (user username password date) (getlistausers paradigmadocs)))
          )
      )
  )

                  
;Descripción: Función que permite autenticar a un usuario registrado e inicia sesión, permitiendo la ejecución de comandos dentro de la plataforma
;Dominio: paradigmadocs X String X String X Function
;Recorrido: Function
;Recorrido final: paradigmadocs
;Recursión: Recursión de cola al hacer uso de la función buscarusuario                        
(define (login paradigmadocs username password operation)
  (if (buscarusuario (getlistausers paradigmadocs) username password)
      (operation (setestado paradigmadocs (agregaruserlogueado username (getestado paradigmadocs))))
      (operation paradigmadocs)
      )
  )

;Descripción: Función que permite a un usuario logueado crear un documento nuevo
;Dominio: paradigmadocs X date X String X String
;Recorrido: paradigmadocs
;Recursión: No
(define (create paradigmadocs)
  (lambda (date nombre contenido)
    (if (not (empty? (getestado paradigmadocs)))
        (desloguear (setlistadocumentos paradigmadocs
                                       (agregardocumento (documento nombre date ((getencryptfn paradigmadocs) contenido)
                                                                    (largolistadocs (getlistadocs paradigmadocs)) (getuserlogueado (getestado paradigmadocs)))
                                                         (getlistadocs paradigmadocs))))
        create)))

;Descripción: Función que permite compartir un documento con otros usuarios
;Dominio: paradigmadocs X int X access List
;Recorrido: paradigmadocs
;Recursión: Recursión de cola al hacer uso de removeaccess y overwriteaccess
(define (share paradigmadocs)
  (lambda (idDoc access . accesses)
    (if (not (empty? (getestado paradigmadocs)))
        (if (and (buscardocumento (getlistadocs paradigmadocs) idDoc) (eqv? (getowner (getdocumentoparadigma (getlistadocs paradigmadocs) idDoc)) (getuserlogueado (getestado paradigmadocs))))
            (cond
              [(eq? null (getaccesodocs (getdocumentoparadigma (getlistadocs paradigmadocs) idDoc)))
               (desloguear (setlistadocumentos paradigmadocs (writevalidacceses (getlistadocs paradigmadocs) (- (- (largolistadocs (getlistadocs paradigmadocs)) idDoc) 1)
                                                                               (setaccesodocs (getdocumentoparadigma (getlistadocs paradigmadocs) idDoc)
                                                                                              (removeaccess paradigmadocs (createlistadeaccesos access accesses) (getowner (getdocumentoparadigma (getlistadocs paradigmadocs) idDoc)))))))]
        
              [else (desloguear (setlistadocumentos paradigmadocs (writevalidacceses (getlistadocs paradigmadocs) (- (- (largolistadocs (getlistadocs paradigmadocs)) idDoc) 1)
                                                                                    (setaccesodocs (getdocumentoparadigma (getlistadocs paradigmadocs) idDoc)
                                                                                                   (uniraccesos (getprimeracceso (removeaccess paradigmadocs (createlistadeaccesos access accesses) (getowner (getdocumentoparadigma (getlistadocs paradigmadocs) idDoc))))
                                                                                                                (overwriteaccess (getaccesodocs (getdocumentoparadigma (getlistadocs paradigmadocs) idDoc))
                                                                                                                                 (generateuserslist getuseraccess (removeaccess paradigmadocs (createlistadeaccesos access accesses)
                                                                                                                                                                                (getowner (getdocumentoparadigma (getlistadocs paradigmadocs) idDoc))))))))))])
            paradigmadocs
            )
        share)))


;Descripción: Función que permite añadir texto al final de la versión actual del documento
;Dominio: paradigmadocs X int X date X String
;Recorrido: paradigmadocs
;Recursión: Recursión natural al usar verificaruserwithaccess
(define (add paradigmadocs)
  (lambda (idDoc fecha contenidoTexto)
    (if (not (empty? (getestado paradigmadocs)))
        (cond
          [(or (verificaruserwithaccess (getalluserswithperms (getuserswithperms idDoc paradigmadocs)) (getuserlogueado (getestado paradigmadocs)))
               (eq? (getowner (getdocumentoparadigma (getlistadocs paradigmadocs) idDoc))
                    (getuserlogueado (getestado paradigmadocs)))) (desloguear (agregarcontenidoporid paradigmadocs contenidoTexto idDoc))]
          
          [else (desloguear paradigmadocs)])
        add)))


;Descripción: Función que permite restaurar una version anterior de un documento
;Dominio: paradigmadocs X int X int
;Recorrido: paradigmadocs
;Recursión: Recursión natural al usar buscardocumento
(define (restoreVersion paradigmadocs)
  (lambda (idDoc idVersion)
    (if (not (empty? (getestado paradigmadocs)))
        (if (and (and (buscardocumento (getlistadocs paradigmadocs) idDoc) (eq? (getowner (getdocumentoparadigma (getlistadocs paradigmadocs) idDoc)) (getuserlogueado (getestado paradigmadocs)))) (<= idVersion (- (getlargoversiones (getversionesdocs (getdocumentoparadigma (getlistadocs paradigmadocs) idDoc))) 1)))
            (desloguear
             (setlistadocumentos paradigmadocs
                                 (actualizardocumento (getlistadocs paradigmadocs) (- (- (largolistadocs (getlistadocs paradigmadocs)) idDoc) 1)
                                                      (setversionesdocs (getdocumentoparadigma (getlistadocs paradigmadocs) idDoc)
                                                                        (agregarversionrestaurada (incrementarnumeroversion
                                                                                                   (getversionid (getversionesdocs (getdocumentoparadigma (getlistadocs paradigmadocs) idDoc)) idVersion))
                                                                                                  (getversionesdocs (getdocumentoparadigma (getlistadocs paradigmadocs) idDoc)))))))
            (desloguear paradigmadocs)
            )
        restoreVersion)))



;Descripción: Función que permite al usuario revocar todos los accesos a sus documentos
;Dominio: paradigmadocs
;Recorrido: paradigmadocs
;Recursión: No
(define (revokeAllAccesses paradigmadocs)   
  (if (not (empty? (getestado paradigmadocs)))
      (setestado (setlistadocumentos paradigmadocs (deleteacceses (getlistadocs paradigmadocs) (getuserlogueado (getestado paradigmadocs)))) null)
      revokeAllAccesses))

;Ejemplo de constructor de paradigmadocs: Se crea un paradigmadocs con el nombre "gDocs"
(define emptyGDocs (paradigmadocs "gDocs" (date 25 10 2021) encryptFn encryptFn))

;Ejemplos de register: Se registran cuatro usuarios, user1 se repite en el último registro, por ende, se omite
(define gDocs1
(register (register (register (register emptyGDocs (date 25 10 2021) "user1" "pass1") (date 25 10 2021) "user2" "pass2") (date 25 10 2021) "user3" "pass3") (date 25 10 2021) "user1" "pass4"))

;Ejemplos de create: Se crean 4 documentos y dos ejemplos de error
(define gDocs2 ((login gDocs1 "user1" "pass1" create) (date 30 08 2021) "doc1" "contenido doc1"))
(define gDocs3 ((login gDocs2 "user2" "pass2" create) (date 30 08 2021) "doc2" "contenido doc2"))
(define gDocs4 ((login gDocs3 "user3" "pass3" create) (date 30 08 2021) "doc3" "contenido doc3"))
(define gDocs5 ((login gDocs4 "user1" "pass1" create) (date 30 08 2021) "doc4" "contenido doc4"))
(define gDocs1000 ((login gDocs2 "user1" "pass4352" create) (date 30 08 2021) "doc23" "contenido doc1000")) ; User1 no coincide con su contraseña
(define gDocs1001 ((login gDocs3 "user3243" "pass43928" create) (date 30 08 2021) "doc234233" "contenido doc1001")) ; User no existe

;Ejemplos de share: Se utiliza esta función cinco veces, con casos distintos, y dos errores
(define gDocs6 ((login gDocs5 "user1" "pass1" share) 0 (access "user2" #\r)))
(define gDocs7 ((login gDocs6 "user2" "pass2" share) 1 (access "user3" #\r) (access "user2" #\c))) ;Un usuario que es dueño de un documento agrega permisos a otro usuario y así mismo
(define gDocs8 ((login gDocs7 "user3" "pass3" share) 3 (access "user2" #\c))) ; Un usuario que NO es dueño de su documento agrega permisos a un usuario
(define gDocs9 ((login gDocs8 "user3" "pass3" share) 2 (access "user1"  #\w))) ; Usuario dueño de su documento concede accesos a otro usuario
(define gDocs10 ((login gDocs9 "user1" "pass1" share) 3 (access "user3" #\w))) ; Un usuario dueño de dos documentos asigna accesos a un usuario en uno de sus documentos
(define gDocs11 ((login gDocs10 "user1" "pass1" share) 45 (access "user2" #\c))) ; Usuario existente intenta modificar un documento que no existe, generando ningún cambio
(define gDocs1999 ((login gDocs9 "user23123" "pass1" share) 0 (access "user3" #\c))) ; Un usuario inexistente, no sucede nada y se devuelve operation

;Ejemplos de add: Cinco ejemplos, dos de ellos corresponden a errores
(define gDocs12 ((login gDocs10 "user1" "pass1" add) 0 (date 30 11 2021) "mas contenido en doc0"))
(define gDocs13 ((login gDocs12 "user3" "pass3" add) 0 (date 30 11 2021) "mas contenido en doc0"))
(define gDocs14 ((login gDocs13 "user3" "pass3" add) 3 (date 30 11 2021) "mas contenido en doc3"))
(define gDocs15 ((login gDocs14 "user1" "pass1" add) 0 (date 30 11 2021) "mas contenido todavia"))
(define gDocs16 ((login gDocs15 "user3" "pass3" add) 1 (date 30 11 2021) "mas contenido en doc1")) ; Un user sin permisos de escritura o comentar inteta añadir texto a un documento
(define gDocs1998 ((login gDocs13 "user3243" "pass1343" add) 0 (date 30 11 2021) "mas contenido en doc0")) ; Un user inexistente, no hay efectos

;Ejemplos de revokeAllAcceses: Tres ejemplos, uno de ellos corresponde a un error
(define gDocs17 (login gDocs16 "user1" "pass1" revokeAllAccesses)) ; User 1 elimina todos los permisos de sus dos documentos
(define gDocs18 (login gDocs17 "user3" "pass3" revokeAllAccesses)) ; User 3 elimina todos los permisos de sus documentos
(define gDocs1997 (login gDocs17 "user232" "pass664" revokeAllAccesses)) ; Un user inexistente, no hay efectos

;Ejemplos de restoreVersion: Cuatro ejemplos, tres de ellos corresponden a errores
(define gDocs19 ((login gDocs18 "user1" "pass1" restoreVersion) 0 0)) ; User1 logra restaurar la versión 0 con éxito
(define gDocs20 ((login gDocs19 "user2" "pass2" restoreVersion) 0 0)) ; un user que no es dueño de el documento, por ende no hay efecto
(define gDocs21 ((login gDocs20 "user1" "pass1" restoreVersion) 0 5)) ; user dueño de su documento intenta restaurar una version inexistente
(define gDocs1996 ((login gDocs21 "user3q2432" "pass1" restoreVersion) 0 0)) ; Un user inexistente, no hay efectos y se retorna procedure