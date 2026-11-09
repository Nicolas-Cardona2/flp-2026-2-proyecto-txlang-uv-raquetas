#lang eopl
;Autores: Nicolas Cardona 2477349, Laura Sofía Echeverry González 2477067, Santiago Serrano Morales 2477006, Samuel Peña Jaramillo 2477399

(require "sintaxis.rkt")

#|
  Chequeador de tipos (realmente es de la segunda entrega pero se adelanta nada mas como para evitar enbutirlo en el interfaz.rkt)
  en la primera entrega no se implementa, chequear-programa deja pasar
  el programa y tipo-del-programa avisa que aun no existe
|#

(define chequear-programa
  (lambda (pgm) pgm))

(define tipo-del-programa
  (lambda (texto)
    (eopl:error 'tipo-del-programa "el chequeador de tipos llega en la segunda entrega")))

(provide (all-defined-out))