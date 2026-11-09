#lang eopl
;Autores: Nicolas Cardona 2477349, Laura Sofía Echeverry González 2477067, Santiago Serrano Morales 2477006, Samuel Peña Jaramillo 2477399

#|
  Fachada del interpretador de TxLang-UV: reune los modulos de src/
  y los vuelve a exportar.
|#

(require "src/sintaxis.rkt"
         "src/unparse.rkt"
         "src/ambiente.rkt"
         "src/almacen.rkt"
         "src/valores.rkt"
         "src/tipos.rkt"
         "src/interprete.rkt"
         "src/interfaz.rkt")

(provide (all-from-out "src/sintaxis.rkt")
         (all-from-out "src/unparse.rkt")
         (all-from-out "src/ambiente.rkt")
         (all-from-out "src/almacen.rkt")
         (all-from-out "src/valores.rkt")
         (all-from-out "src/tipos.rkt")
         (all-from-out "src/interprete.rkt")
         (all-from-out "src/interfaz.rkt"))

#|;;; ==========================================================
;;; 1. Especificación léxica
;;; ==========================================================
;; Los dos tipos de comentario, los literales de cada clase y los
;; identificadores. El comentario de bloque no admite | adentro.

(define especificacion-lexica
  '((espacio (whitespace) skip)
    (numero (digit (arbno digit)) number)
    (identificador (letter (arbno (or letter digit "-" "_" "?"))) symbol)))

;;; ==========================================================
;;; 2. Especificación gramatical
;;; ==========================================================
;; Un datatype por no terminal: programa, declaración de registro, tipo,
;; expresión y primitiva. Los nombres de las variantes los escoge el grupo.

(define especificacion-gramatical
  '((programa (expresion) un-programa)
    (expresion (numero) lit-exp)))

(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

(define scan&parse
  (sllgen:make-string-parser especificacion-lexica especificacion-gramatical))

;; Muestra los define-datatype que SLLGEN generó a partir de la gramática.
(define mostrar-datatypes
  (lambda ()
    (sllgen:list-define-datatypes especificacion-lexica especificacion-gramatical)))

;;; ==========================================================
;;; 3. Unparse: del árbol de sintaxis abstracta al texto
;;; ==========================================================

(define unparse
  (lambda (pgm)
    (cases programa pgm
      (un-programa (cuerpo) (unparse-expresion cuerpo)))))

(define unparse-expresion
  (lambda (exp)
    (cases expresion exp
      (lit-exp (n) (number->string n)))))

;;; ==========================================================
;;; 4. TAD ambiente
;;; ==========================================================
;; Liga identificadores a valores. Su interfaz va aparte de la del almacén.

;;; ==========================================================
;;; 5. TAD almacén
;;; ==========================================================
;; Celdas que se reservan con ref, se leen con deref y se escriben con set.
;; En la primera entrega las escrituras son definitivas; en la segunda el
;; almacén lleva el registro que permite revertir un bloque atomic. Conviene
;; diseñar la interfaz desde ahora pensando en eso.

;;; ==========================================================
;;; 6. Valores expresados
;;; ==========================================================
;; Un define-datatype con los valores del lenguaje y valor->texto, que es
;; lo que imprime correr.

(define valor->texto
  (lambda (v)
    (cond ((number? v) (number->string v))
          (else (eopl:error 'valor->texto "no sé escribir el valor ~s" v)))))

;;; ==========================================================
;;; 7. Chequeador de tipos (segunda entrega)
;;; ==========================================================
;; Recibe el programa y lo devuelve si está bien tipado; si no, aborta con
;; eopl:error antes de que se evalúe. Mientras no exista, deja pasar todo.

(define chequear-programa
  (lambda (pgm) pgm))

;; Tipo del programa, escrito como texto: "int", "ref(bool)", "(int -> int)".
(define tipo-del-programa
  (lambda (texto)
    (eopl:error 'tipo-del-programa "el chequeador de tipos llega en la segunda entrega")))

;;; ==========================================================
;;; 8. Interpretador
;;; ==========================================================

(define value-of
  (lambda (exp amb)
    (cases expresion exp
      (lit-exp (n) n))))

;;; ==========================================================
;;; 9. Interfaz
;;; ==========================================================

(define interpretar
  (lambda (texto)
    (let ((pgm (scan&parse texto)))
      (begin
        (chequear-programa pgm)
        (cases programa pgm
          (un-programa (cuerpo) (value-of cuerpo 'ambiente-vacio)))))))

;; Lo que usan las baterías: un texto de TxLang-UV y el valor como texto.
(define correr
  (lambda (texto) (valor->texto (interpretar texto))))

(provide (all-defined-out))|#
