#lang eopl
;Autores: Santiago Serrano Morales 2477006

(require "sintaxis.rkt")
(require "valores.rkt")
(require "ambiente.rkt")
(require "almacen.rkt")

#|
  Interprete de TxLang-UV

  Implementa la evaluacion de expresiones mediante
  el procedimiento value-of.
|#

;;; 1. Evaluacion de expresiones

(define value-of
  (lambda (exp ambiente)
    (cases expresion exp

      ;; Literales numericos
      (lit-exp (numero)
        (entero-val numero))

      (flotante-exp (numero)
        (flotante-val numero))

      ;; Literales de texto
      (cadena-exp (cadena)
        (cadena-val
         (substring cadena 1 (- (string-length cadena) 1))))
   
      (caracter-exp (caracter)
        (caracter-val
         (substring caracter 1 (- (string-length caracter) 1))))

      ;; Literales booleanos
      (true-exp ()
        (booleano-val #t))

      (false-exp ()
        (booleano-val #f))

      ;; Literal unit
      (unit-exp ()
        (unit-val))

      ;; Variables
      (var-exp (identificador)
        (buscar-ambiente ambiente identificador))

      ;; Declaracion de variables locales
      (let-exp (variables expresiones cuerpo)
        (let ([valores (map
                (lambda (expresion)
                  (value-of expresion ambiente))
                expresiones)])
          (value-of cuerpo
                    (extender-ambiente variables valores ambiente))))

      ;; Creacion de referencias
      (ref-exp (expresion)
        (referencia-val
         (nuevo-ref (value-of expresion ambiente))))

      ;; Consulta del contenido de una referencia
      (deref-exp (expresion)
        (let ([referencia (value-of expresion ambiente)])
          (obtener-ref (valor-a-referencia referencia))))

      ;; Modificacion del contenido de una referencia
      (set-exp (expresion-referencia expresion-valor)
        (let ([referencia
               (value-of expresion-referencia ambiente)]
              [nuevo-valor
               (value-of expresion-valor ambiente)])
          (asignar-ref!
           (valor-a-referencia referencia) nuevo-valor)
          (unit-val)))

      ;; Evaluacion Secuencial de expresiones
      (begin-exp (primera-exp otras-exps)
        (letrec
            ([evaluar-secuencia
              (lambda (expresiones)
                (if (null? (cdr expresiones))
                    (value-of (car expresiones) ambiente)
                    (begin
                      (value-of (car expresiones) ambiente)
                      (evaluar-secuencia (cdr expresiones)))))])
          (evaluar-secuencia (cons primera-exp otras-exps))))

      ;; Evaluacion de condicionales if, elif y else
      (if-exp ( condicion expresion-then condiciones-elif expresiones-elif expresion-else)
        (let ([resultado (value-of condicion ambiente)])
          (cases valor-expresado resultado
            (booleano-val (booleano)
              (if booleano
                  (value-of expresion-then ambiente)
                  (evaluar-elif condiciones-elif expresiones-elif expresion-else ambiente)))
            (else (eopl:error 'value-of "La condicion de if debe ser booleana")))))

      ;; Evaluacion de ciclos while
      (while-exp (condicion cuerpo)
        (letrec
            ([ejecutar-ciclo
              (lambda ()
                (let ([resultado
                       (value-of condicion ambiente)])
                  (cases valor-expresado resultado
                    (booleano-val (booleano)
                      (if booleano
                          (begin
                            (value-of cuerpo ambiente)
                            (ejecutar-ciclo))
                          (unit-val)))
                    (else (eopl:error 'value-of "La condicion de while debe ser booleana")))))])
          (ejecutar-ciclo)))

      ;; Evaluacion de primitivas
      (prim-exp (operador argumentos)
        (evaluar-primitiva operador argumentos ambiente))
      
      ;; Creacion de procedimientos
      (proc-exp (parametros tipos cuerpo)
        (procedimiento-val parametros cuerpo ambiente))

      ;; Aplicacion de procedimientos
      (apply-exp (expresion-procedimiento argumentos)
        (let ([procedimiento
               (value-of expresion-procedimiento ambiente)]
              [valores-argumentos
               (map
                (lambda (argumento)
                  (value-of argumento ambiente))
                argumentos)])
          (cases valor-expresado procedimiento
            (procedimiento-val (parametros cuerpo ambiente-definicion)
              (value-of cuerpo (extender-ambiente parametros valores-argumentos ambiente-definicion)))
            (else (eopl:error 'value-of "Se esperaba un procedimiento")))))

      ;; Declaraciones recursivas
      (letrec-exp (tipos nombres parametros tipos-parametros cuerpos cuerpo)
        (value-of cuerpo
         (ambiente-recursivo nombres parametros cuerpos ambiente)))

      ;; Expresiones pendientes de implementar
      (else (eopl:error 'value-of "Expresion aun no implementada: ~s" exp)))))

;;; Evaluacion de condiciones elif

(define evaluar-elif
  (lambda (condiciones expresiones-elif expresion-else ambiente)
    (if (null? condiciones)
        (value-of expresion-else ambiente)
        (let ([resultado
               (value-of (car condiciones) ambiente)])
          (cases valor-expresado resultado
            (booleano-val (booleano)
              (if booleano
                  (value-of (car expresiones-elif) ambiente)
                  (evaluar-elif (cdr condiciones) (cdr expresiones-elif) expresion-else ambiente)))
            (else (eopl:error 'value-of "La condicion de elif debe ser booleana")))))))

;;; 2. Evaluacion de primitivas

(define evaluar-primitiva
  (lambda (operador argumentos ambiente)
    (let ([valores
           (map
            (lambda (expresion)
              (value-of expresion ambiente))
            argumentos)])
      (cases primitiva operador

        (suma-prim ()
          (let ([n1 (valor-a-numero (car valores))]
                [n2 (valor-a-numero (cadr valores))])
            (if (and (exact? n1) (exact? n2))
                (entero-val (+ n1 n2))
                (flotante-val (+ n1 n2)))))

        (resta-prim ()
          (let ([n1 (valor-a-numero (car valores))]
                [n2 (valor-a-numero (cadr valores))])
            (if (and (exact? n1) (exact? n2))
                (entero-val (- n1 n2))
                (flotante-val (- n1 n2)))))

        (mult-prim ()
          (let ([n1 (valor-a-numero (car valores))]
                [n2 (valor-a-numero (cadr valores))])
            (if (and (exact? n1) (exact? n2))
                (entero-val (* n1 n2))
                (flotante-val (* n1 n2)))))

        (div-prim ()
          (let ([n1 (valor-a-numero (car valores))]
                [n2 (valor-a-numero (cadr valores))])
            (if (= n2 0)
                (eopl:error 'evaluar-primitiva "Division por cero")
                (if (and (exact? n1) (exact? n2))
                    (entero-val (quotient n1 n2))
                    (flotante-val (/ n1 n2))))))

        (mod-prim ()
          (let ([n1 (valor-a-numero (car valores))]
                [n2 (valor-a-numero (cadr valores))])
            (if (and (exact? n1) (integer? n1)
                     (exact? n2) (integer? n2))
                (if (= n2 0)
                    (eopl:error 'evaluar-primitiva "Division por cero")
                    (entero-val (remainder n1 n2)))
                (eopl:error 'evaluar-primitiva "El modulo requiere operandos enteros"))))
        
        (mayor-prim ()
          (let ([n1 (valor-a-numero (car valores))]
                [n2 (valor-a-numero (cadr valores))])
            (booleano-val (> n1 n2))))

        (menor-prim ()
          (let ([n1 (valor-a-numero (car valores))]
                [n2 (valor-a-numero (cadr valores))])
            (booleano-val (< n1 n2))))

        (mayor-igual-prim ()
          (let ([n1 (valor-a-numero (car valores))]
                [n2 (valor-a-numero (cadr valores))])
            (booleano-val (>= n1 n2))))

        (menor-igual-prim ()
          (let ([n1 (valor-a-numero (car valores))]
                [n2 (valor-a-numero (cadr valores))])
            (booleano-val (<= n1 n2))))

        (igual-prim ()
          (let ([v1 (car valores)]
                [v2 (cadr valores)])
            (cases valor-expresado v1
              (entero-val (n1)
                (cases valor-expresado v2
                  (entero-val (n2) (booleano-val (= n1 n2)))
                  (flotante-val (n2) (booleano-val (= n1 n2)))
                  (else (booleano-val #f))))
              (flotante-val (n1)
                (cases valor-expresado v2
                  (entero-val (n2) (booleano-val (= n1 n2)))
                  (flotante-val (n2) (booleano-val (= n1 n2)))
                  (else (booleano-val #f))))
              (cadena-val (c1)
                (cases valor-expresado v2
                  (cadena-val (c2) (booleano-val (string=? c1 c2)))
                  (else (booleano-val #f))))
              (booleano-val (b1)
                (cases valor-expresado v2
                  (booleano-val (b2) (booleano-val (eq? b1 b2)))
                  (else (booleano-val #f))))
              (caracter-val (c1)
                (cases valor-expresado v2
                  (caracter-val (c2) (booleano-val (string=? c1 c2)))
                  (else (booleano-val #f))))
              (unit-val ()
                (cases valor-expresado v2
                  (unit-val () (booleano-val #t))
                  (else (booleano-val #f))))
              (else (booleano-val #f)))))
        
        (diferente-prim ()
          (let ([v1 (car valores)]
                [v2 (cadr valores)])
            (cases valor-expresado v1
              (entero-val (n1)
                (cases valor-expresado v2
                  (entero-val (n2) (booleano-val (not (= n1 n2))))
                  (flotante-val (n2) (booleano-val (not (= n1 n2))))
                  (else (booleano-val #t))))
              (flotante-val (n1)
                (cases valor-expresado v2
                  (entero-val (n2) (booleano-val (not (= n1 n2))))
                  (flotante-val (n2) (booleano-val (not (= n1 n2))))
                  (else (booleano-val #t))))
              (cadena-val (c1)
                (cases valor-expresado v2
                  (cadena-val (c2) (booleano-val (not (string=? c1 c2))))
                  (else (booleano-val #t))))
              (booleano-val (b1)
                (cases valor-expresado v2
                  (booleano-val (b2) (booleano-val (not (eq? b1 b2))))
                  (else (booleano-val #t))))
              (caracter-val (c1)
                (cases valor-expresado v2
                  (caracter-val (c2) (booleano-val (not (string=? c1 c2))))
                  (else (booleano-val #t))))
              (unit-val ()
                (cases valor-expresado v2
                  (unit-val () (booleano-val #f))
                  (else (booleano-val #t))))
              (else (booleano-val #t)))))

        (and-prim ()
          (let ([b1 (valor-a-booleano (car valores))]
                [b2 (valor-a-booleano (cadr valores))])
            (booleano-val (and b1 b2))))

        (or-prim ()
          (let ([b1 (valor-a-booleano (car valores))]
                [b2 (valor-a-booleano (cadr valores))])
            (booleano-val (or b1 b2))))

        (not-prim ()
          (let ([b1 (valor-a-booleano (car valores))])
            (booleano-val (not b1))))

        (concat-prim ()
          (let ([c1 (valor-a-cadena (car valores))]
                [c2 (valor-a-cadena (cadr valores))])
            (cadena-val (string-append c1 c2))))

        (length-prim ()
          (let ([cadena (valor-a-cadena (car valores))])
            (entero-val (string-length cadena))))

        (to-string-prim ()
          (let ([valor (car valores)])
            (cases valor-expresado valor
              (entero-val (numero)
                (cadena-val (number->string numero)))
              (flotante-val (numero)
                (cadena-val (number->string numero)))
              (cadena-val (cadena)
                (cadena-val cadena))
              (booleano-val (booleano)
                (cadena-val (if booleano "true" "false")))
              (caracter-val (caracter)
                (cadena-val caracter))
              (unit-val ()
                (cadena-val "unit"))
              (else (eopl:error 'evaluar-primitiva "No se puede convertir este valor a cadena")))))

        (to-number-prim ()
          (let ([cadena (valor-a-cadena (car valores))])
            (let ([numero (string->number cadena)])
              (if numero
                  (if (integer? numero)
                      (entero-val numero)
                      (flotante-val numero))
                  (eopl:error 'evaluar-primitiva "La cadena no representa un numero")))))

        (print-prim ()
          (let ([valor (car valores)])
            (cases valor-expresado valor
              (entero-val (numero)
                (display numero))
              (flotante-val (numero)
                (display numero))
              (cadena-val (cadena)
                (display cadena))
              (caracter-val (caracter)
                (display caracter))
              (booleano-val (booleano)
                (display booleano))
              (unit-val ()
                (display "unit"))
              (referencia-val (referencia)
                (display referencia))
              (procedimiento-val (parametros cuerpo ambiente)
                (display "<procedimiento>")))
            (unit-val)))

        (else (eopl:error 'evaluar-primitiva "Primitiva aun no implementada"))))))


;;; 3. Evaluacion de procedimientos

;;; 4. Exportacion del modulo

(provide (all-defined-out))

