#lang eopl
;Autores: Nicolas Cardona 2477349, Laura Sofía Echeverry González 2477067, Santiago Serrano Morales 2477006, Samuel Peña Jaramillo 2477399

(require "sintaxis.rkt" "valores.rkt" "ambiente.rkt" "almacen.rkt"
         "tipos.rkt" "interprete.rkt")

#|
  interfaz del interpretador interpretar, correr y valor->texto
|#

(define valor->texto
  (lambda (v)
    (cases valor-expresado v
      (entero-val (n) (number->string n))
      (flotante-val (n) (number->string n))
      (cadena-val (s) s)
      (caracter-val (c) c)
      (booleano-val (b) (if b "true" "false"))
      (unit-val () "unit")
      (referencia-val (r) (string-append "<ref " (number->string r) ">"))
      (procedimiento-val (ps cuerpo amb) "<procedimiento>"))))

(define interpretar
  (lambda (texto)
    (let ((pgm (scan&parse texto)))
      (begin
        (chequear-programa pgm)
        (inicializar-almacen!)
        (cases programa pgm
          (un-programa (registros cuerpo)
            (value-of cuerpo (crear-ambiente-vacio))))))))

(define correr
  (lambda (texto) (valor->texto (interpretar texto))))

(provide (all-defined-out))