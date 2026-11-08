#lang eopl
;Autores: Laura

(require "sintaxis.rkt")

#|
  Valores de TxLang-UV

  Define los valores expresados del lenguaje y los extractores
  necesarios para trabajar con ellos.
|#

;;;1. Definicion de los valores expresados

(define-datatype valor-expresado valor-expresado?
  (entero-val
   (numero number?))

  (flotante-val
   (numero number?))

  (cadena-val
   (cadena string?))

  (caracter-val
   (caracter string?))

  (booleano-val
   (booleano boolean?))

  (unit-val)

  (referencia-val
   (referencia number?))

  (procedimiento-val
   (parametros (list-of symbol?))
   (cuerpo expresion?)
   (ambiente-definicion (lambda (ambiente) #t))))

;;;2. Extractores de valores

(define valor-a-numero
  (lambda (valor)
    (cases valor-expresado valor
      (entero-val (numero) numero)
      (flotante-val (numero) numero)
      (else (eopl:error 'valor-a-numero "Se esperaba un valor numerico")))))

(define valor-a-cadena
  (lambda (valor)
    (cases valor-expresado valor
      (cadena-val (cadena) cadena)
      (else (eopl:error 'valor-a-cadena "Se esperaba un valor de tipo cadena")))))

(define valor-a-caracter
  (lambda (valor)
    (cases valor-expresado valor
      (caracter-val (caracter) caracter)
      (else (eopl:error 'valor-a-caracter "Se esperaba un valor tipo caracter")))))

(define valor-a-booleano
  (lambda (valor)
    (cases valor-expresado valor
      (booleano-val (booleano) booleano)
      (else (eopl:error 'valor-a-booleano "Se esperaba un valor de tipo booleano")))))

(define valor-a-referencia
  (lambda (valor)
    (cases valor-expresado valor
      (referencia-val (referencia) referencia)
      (else (eopl:error 'valor-a-referencia "Se esperaba un valor de tipo referencia")))))

(define valor-a-procedimiento
  (lambda (valor)
    (cases valor-expresado valor
      (procedimiento-val (parametros cuerpo ambiente)
        (list parametros cuerpo ambiente))
      (else (eopl:error 'valor-a-procedimiento "Se esperaba un valor de tipo procedimiento")))))

;;;3. Exportacion del modulo

(provide (all-defined-out))
