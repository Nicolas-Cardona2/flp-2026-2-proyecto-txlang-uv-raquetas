#lang eopl
;Autores: Laura

(require "valores.rkt")
(require "sintaxis.rkt")

#|
  Ambientes de TxLang-UV

  Define los ambientes y las operaciones para
  extenderlos y consultar los valores asociados
  a los identificadores.
|#

;;; 1. Definicion de la estructura de ambientes


(define-datatype ambiente ambiente?
  (ambiente-vacio)

  (ambiente-extendido
   (variables (list-of symbol?))
   (valores (list-of valor-expresado?))
   (ambiente-anterior ambiente?))

  (ambiente-recursivo
   (nombres (list-of symbol?))
   (parametros (list-of (list-of symbol?)))
   (cuerpos (list-of expresion?))
   (ambiente-anterior ambiente?)))

;;; 2. Creacion de ambientes

(define crear-ambiente-vacio
  (lambda ()
    (ambiente-vacio)))

;;; 3. Extension de ambientes

(define extender-ambiente
  (lambda (variables valores ambiente-anterior)
    (if (= (length variables) (length valores))
        (ambiente-extendido variables valores ambiente-anterior)
        (eopl:error 'extender-ambiente "La cantidad de variables y valores debe coincidir"))))

;;; 4. Busqueda de identificadores


(define buscar-ambiente
  (lambda (amb identificador)
    (cases ambiente amb
      (ambiente-vacio ()
        (eopl:error 'buscar-ambiente "Identificador no definido: ~s" identificador))

      (ambiente-extendido (variables valores ambiente-anterior)
        (buscar-en-listas
         variables valores identificador ambiente-anterior))

      (ambiente-recursivo (nombres parametros cuerpos ambiente-anterior)
        (buscar-recursivo
         nombres parametros cuerpos identificador amb)))))

(define buscar-en-listas
  (lambda (variables valores identificador ambiente-anterior)
    (cond
      [(null? variables)
       (buscar-ambiente ambiente-anterior identificador)]
      
      [(eq? identificador (car variables))
       (car valores)]
      
      [else (buscar-en-listas (cdr variables) (cdr valores) identificador ambiente-anterior)])))

(define buscar-recursivo
  (lambda (nombres parametros cuerpos identificador amb)
    (cond
      [(null? nombres)
       (cases ambiente amb
         (ambiente-recursivo (nombres parametros cuerpos ambiente-anterior)
           (buscar-ambiente ambiente-anterior identificador))
         (else (eopl:error 'buscar-recursivo "Identificador no definido: ~s" identificador)))]

      [(eq? identificador (car nombres)) (procedimiento-val (car parametros) (car cuerpos) amb)]

      [else (buscar-recursivo (cdr nombres) (cdr parametros) (cdr cuerpos) identificador amb)])))

;;; 5. Exportacion del modulo

(provide (all-defined-out))
