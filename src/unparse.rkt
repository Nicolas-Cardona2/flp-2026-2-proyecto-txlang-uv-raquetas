#lang eopl
;Autores: Samuel Peña Jaramillo 2477399

(require "sintaxis.rkt")

#|
  Unparser de TxLang-UV, reconstruye el texto fuente de un programa a partir de su arbol
  de sintaxis abstracta (AST) para que vuelva a parsear igual.
|#

;; Funciones auxiliares para juntar textos con separadores

;; Junta una lista de strings poniendo un separador en el medio como , o ;
(define unir-con
  (lambda (lista sep)
    (cond
      [(null? lista) ""]
      [(null? (cdr lista)) (car lista)]
      [else (string-append (car lista) sep (unir-con (cdr lista) sep))])))

;; Arma pares "id: tipo" para los campos de record y parametros de proc/letrec
(define unir-campos-tipo
  (lambda (ids tipos)
    (if (null? ids)
        ""
        (unir-con
         (map (lambda (id t)
                (string-append (symbol->string id) ": " (unparse-tipo t)))
              ids tipos)
         ", "))))

;; Arma pares "id = exp" para las ligaduras de let y la creacion con new
(define unir-ligaduras
  (lambda (ids exps)
    (if (null? ids)
        ""
        (unir-con
         (map (lambda (id e)
                (string-append (symbol->string id) " = " (unparse-exp e)))
              ids exps)
         ", "))))

;; Arma las ramas "elif cond then exp" del condicional
(define unir-elifs
  (lambda (conds exps)
    (if (null? conds)
        ""
        (string-append
         " elif " (unparse-exp (car conds))
         " then " (unparse-exp (car exps))
         (unir-elifs (cdr conds) (cdr exps))))))

;; Arma las declaraciones de funciones dentro de letrec
(define unir-letrecs
  (lambda (tipos-ret nombres listas-params listas-tipos cuerpos)
    (if (null? nombres)
        ""
        (unir-con
         (map (lambda (t-ret nom params t-params cuerpo)
                (string-append
                 (unparse-tipo t-ret) " "
                 (symbol->string nom) " ("
                 (unir-campos-tipo params t-params) ") = "
                 (unparse-exp cuerpo)))
              tipos-ret nombres listas-params listas-tipos cuerpos)
         " "))))

;; Unparse de cada categoria gramatical

;; Convierte el nodo raiz del programa a texto
(define unparse
  (lambda (prog)
    (cases programa prog
      (un-programa (registros exp)
        (let ([texto-regs (unir-con (map unparse-registro registros) " ")])
          (if (string=? texto-regs "")
              (unparse-exp exp)
              (string-append texto-regs " " (unparse-exp exp))))))))

;; Reconstruye declaraciones record
(define unparse-registro
  (lambda (reg)
    (cases decl-registro reg
      (un-registro (nombre campos tipos)
        (string-append
         "record " (symbol->string nombre)
         " { " (unir-campos-tipo campos tipos) " }")))))

;; Reconstruye las anotaciones de tipo
(define unparse-tipo
  (lambda (t)
    (cases tipo t
      (int-tipo () "int")
      (float-tipo () "float")
      (bool-tipo () "bool")
      (string-tipo () "string")
      (char-tipo () "char")
      (unit-tipo () "unit")
      (ref-tipo (subtipo)
        (string-append "ref(" (unparse-tipo subtipo) ")"))
      (proc-tipo (tipos-arg tipo-ret)
        (string-append
         "(" (unir-con (map unparse-tipo tipos-arg) ", ")
         " -> " (unparse-tipo tipo-ret) ")"))
      (registro-tipo (id)
        (symbol->string id)))))

;; Reconstruye cualquier expresion del lenguaje
(define unparse-exp
  (lambda (exp)
    (cases expresion exp
      ;; En sintaxis.rkt las cadenas y caracteres ya guardan sus comillas originales
      (lit-exp (n) (number->string n))
      (flotante-exp (f) (number->string f))
      (cadena-exp (s) s)
      (caracter-exp (c) c)
      (true-exp () "true")
      (false-exp () "false")
      (unit-exp () "unit")
      (var-exp (id) (symbol->string id))

      (let-exp (ids exps cuerpo)
        (string-append
         "let " (unir-ligaduras ids exps)
         " in " (unparse-exp cuerpo) " end"))

      (ref-exp (e)
        (string-append "ref(" (unparse-exp e) ")"))

      (deref-exp (e)
        (string-append "deref(" (unparse-exp e) ")"))

      (set-exp (ref val)
        (string-append "set " (unparse-exp ref) " := " (unparse-exp val)))

      (begin-exp (primera otras)
        (string-append
         "begin "
         (unir-con (map unparse-exp (cons primera otras)) "; ")
         " end"))

      (if-exp (cond-if then-if conds-elif thens-elif else-if)
        (string-append
         "if " (unparse-exp cond-if)
         " then " (unparse-exp then-if)
         (unir-elifs conds-elif thens-elif)
         " else " (unparse-exp else-if)
         " end"))

      (while-exp (cond-w cuerpo-w)
        (string-append
         "while " (unparse-exp cond-w)
         " do " (unparse-exp cuerpo-w) " end"))

      (prim-exp (prim args)
        (string-append
         (unparse-primitiva prim)
         "(" (unir-con (map unparse-exp args) ", ") ")"))

      (proc-exp (ids tipos cuerpo)
        (string-append
         "proc (" (unir-campos-tipo ids tipos) ") "
         (unparse-exp cuerpo) " end"))

      (apply-exp (rator rands)
        (string-append
         "apply " (unparse-exp rator)
         "(" (unir-con (map unparse-exp rands) ", ") ")"))

      (letrec-exp (tipos-ret nombres params tipos-params cuerpos cuerpo-in)
        (string-append
         "letrec " (unir-letrecs tipos-ret nombres params tipos-params cuerpos)
         " in " (unparse-exp cuerpo-in) " end"))

      (atomic-exp (cuerpo id-err manejador)
        (string-append
         "atomic " (unparse-exp cuerpo)
         " onfail (" (symbol->string id-err) ") "
         (unparse-exp manejador) " end"))

      (abort-exp (e)
        (string-append "abort(" (unparse-exp e) ")"))

      (new-exp (id-reg campos exps)
        (string-append
         "new " (symbol->string id-reg)
         " { " (unir-ligaduras campos exps) " }"))

      (field-exp (e campo)
        (string-append
         "field(" (unparse-exp e) ", " (symbol->string campo) ")")))))

;; Traduce cada primitiva a su simbolo en texto
(define unparse-primitiva
  (lambda (p)
    (cases primitiva p
      (suma-prim () "+")
      (resta-prim () "-")
      (mult-prim () "*")
      (div-prim () "/")
      (mod-prim () "%")
      (mayor-prim () ">")
      (menor-prim () "<")
      (mayor-igual-prim () ">=")
      (menor-igual-prim () "<=")
      (igual-prim () "==")
      (diferente-prim () "!=")
      (and-prim () "and")
      (or-prim () "or")
      (not-prim () "not")
      (concat-prim () "concat")
      (length-prim () "length")
      (to-string-prim () "to-string")
      (to-number-prim () "to-number")
      (print-prim () "print"))))

;; Exportacion 

(provide (all-defined-out))