#lang eopl
;Autores: Nicolas 2477349

#|
  Sintaxis de TxLang-UV especificación lexica, gramatica, arbol de sintaxis
  abstracta y parser
|#

;;;1 especificacion lexica 
;; tokens espacios, dos tipos de comentario, enteros, flotantes, cadenas,
;; caracteres e identificadores
(define especificacion-lexica
  '((espacio (whitespace) skip)
    (comentario-linea ("//" (arbno (not #\newline))) skip)
    (comentario-bloque ("#|" (arbno (not #\|)) "|#") skip)
    (identificador (letter (arbno (or letter digit "-" "_" "?"))) symbol)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (flotante (digit (arbno digit) "." digit (arbno digit)) number)
    (flotante ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (cadena ("\"" (arbno (not #\")) "\"") string)
    (caracter ("'" (not #\') "'") string)))

;;;2 especificacion gramatical

(define especificacion-gramatical
  '((programa ((arbno decl-registro) expresion) un-programa)

    (decl-registro
     ("record" identificador "{"
      (separated-list identificador ":" tipo ",") "}")
     un-registro)

    (tipo ("int") int-tipo)
    (tipo ("float") float-tipo)
    (tipo ("bool") bool-tipo)
    (tipo ("string") string-tipo)
    (tipo ("char") char-tipo)
    (tipo ("unit") unit-tipo)
    (tipo ("ref" "(" tipo ")") ref-tipo)
    (tipo ("(" (separated-list tipo ",") "->" tipo ")") proc-tipo)
    (tipo (identificador) registro-tipo)

    (expresion (numero) lit-exp)
    (expresion (flotante) flotante-exp)
    (expresion (cadena) cadena-exp)
    (expresion (caracter) caracter-exp)
    (expresion ("true") true-exp)
    (expresion ("false") false-exp)
    (expresion ("unit") unit-exp)
    (expresion (identificador) var-exp)

    (expresion
     ("let" (separated-list identificador "=" expresion ",")
      "in" expresion "end")
     let-exp)

    (expresion ("ref" "(" expresion ")") ref-exp)
    (expresion ("deref" "(" expresion ")") deref-exp)
    (expresion ("set" expresion ":=" expresion) set-exp)

    (expresion
     ("begin" expresion (arbno ";" expresion) "end")
     begin-exp)

    (expresion
     ("if" expresion "then" expresion
      (arbno "elif" expresion "then" expresion)
      "else" expresion "end")
     if-exp)

    (expresion ("while" expresion "do" expresion "end") while-exp)

    (expresion
     (primitiva "(" (separated-list expresion ",") ")")
     prim-exp)

    (expresion
     ("proc" "(" (separated-list identificador ":" tipo ",") ")"
      expresion "end")
     proc-exp)

    (expresion
     ("apply" expresion "(" (separated-list expresion ",") ")")
     apply-exp)

    (expresion
     ("letrec"
      (arbno tipo identificador
             "(" (separated-list identificador ":" tipo ",") ")"
             "=" expresion)
      "in" expresion "end")
     letrec-exp)

    (expresion
     ("atomic" expresion "onfail" "(" identificador ")" expresion "end")
     atomic-exp)

    (expresion ("abort" "(" expresion ")") abort-exp)

    (expresion
     ("new" identificador "{"
      (separated-list identificador "=" expresion ",") "}")
     new-exp)

    (expresion ("field" "(" expresion "," identificador ")") field-exp)

    ;;primitivas
    (primitiva ("+") suma-prim)
    (primitiva ("-") resta-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("/") div-prim)
    (primitiva ("%") mod-prim)
    (primitiva (">") mayor-prim)
    (primitiva ("<") menor-prim)
    (primitiva (">=") mayor-igual-prim)
    (primitiva ("<=") menor-igual-prim)
    (primitiva ("==") igual-prim)
    (primitiva ("!=") diferente-prim)
    (primitiva ("and") and-prim)
    (primitiva ("or") or-prim)
    (primitiva ("not") not-prim)
    (primitiva ("concat") concat-prim)
    (primitiva ("length") length-prim)
    (primitiva ("to-string") to-string-prim)
    (primitiva ("to-number") to-number-prim)
    (primitiva ("print") print-prim)))

;; genera los define-datatype del arbol a partir de la gramática
(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

;;;3 parser y utilidades

(define scan&parse
  (sllgen:make-string-parser especificacion-lexica especificacion-gramatical))

(define scan-solo
  (sllgen:make-string-scanner especificacion-lexica especificacion-gramatical))

;; muestra los define-datatype que SLLGEN genero
(define mostrar-datatypes
  (lambda ()
    (sllgen:list-define-datatypes especificacion-lexica especificacion-gramatical)))

(provide (all-defined-out))