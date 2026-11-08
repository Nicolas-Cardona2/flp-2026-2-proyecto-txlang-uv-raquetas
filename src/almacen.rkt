#lang eopl
;Autores: Laura Sofía Echeverry González 2477067, Santiago Serrano Morales 2477006

(require "valores.rkt")

#|
  Almacen de TxLang-UV

  Define el almacenamiento de valores y las operaciones
  para reservar, consultar y modificar posiciones.
|#

;;; 1. Definicion del almacen

(define almacen-global '())

;;; 2. Inicializacion del almacen

(define inicializar-almacen!
  (lambda ()
    (set! almacen-global '())))

;;; 3. Reserva de posiciones

(define nuevo-ref
  (lambda (valor)
    (let ([posicion (length almacen-global)])
      (set! almacen-global
            (append almacen-global (list valor))) posicion)))

;;; 4. Consulta de posiciones

(define obtener-ref
  (lambda (posicion)
    (if (and (integer? posicion)
             (>= posicion 0)
             (< posicion (length almacen-global)))
        (list-ref almacen-global posicion)
        (eopl:error 'obtener-ref "Referencia invalida: ~s" posicion))))

;;; 5. Modificacion de posiciones

(define reemplazar-posicion
  (lambda (lista posicion nuevo-valor)
    (cond
      [(null? lista)
       (eopl:error 'reemplazar-posicion "Posicion inexistente: ~s" posicion)]

      [(zero? posicion)
       (cons nuevo-valor (cdr lista))]

      [else
       (cons (car lista) (reemplazar-posicion (cdr lista) (- posicion 1) nuevo-valor))])))

(define asignar-ref!
  (lambda (posicion nuevo-valor)
    (if (and (integer? posicion)
             (>= posicion 0)
             (< posicion (length almacen-global)))
        (set! almacen-global
              (reemplazar-posicion
               almacen-global
               posicion
               nuevo-valor))
        (eopl:error 'asignar-ref! "Referencia invalida: ~s" posicion))))

;;; 6. Exportacion del modulo

(provide (all-defined-out))
