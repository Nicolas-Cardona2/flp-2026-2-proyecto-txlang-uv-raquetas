#lang eopl
;Autores: Samuel Peña Jaramillo 2477399

#|
  Bateria de pruebas del interpretador de TxLang-UV: primera entrega.

  Cada prueba compara el texto que devuelve `correr` con el que se espera, o
  comprueba que un programa se rechace. Cubre el scanner, la gramatica,
  unparse (ida y vuelta) y el nucleo de value-of: literales, let, ref/deref/set,
  begin, if/elif/else, while, primitivas, procedimientos, clausuras y letrec.

  Se ejecuta con:  racket pruebas-txlang.rkt
|#

(require (only-in racket/base with-handlers exn:fail? exn-message))
(require "interprete-txlang.rkt")

(define pasadas 0)
(define fallidas 0)

;; Corre el programa y, si aborta, devuelve el mensaje precedido de ERROR.
(define intentar
  (lambda (texto)
    (with-handlers ((exn:fail? (lambda (e) (string-append "ERROR: " (exn-message e)))))
      (correr texto))))

(define empieza-con-error?
  (lambda (texto)
    (and (>= (string-length texto) 5) (string=? (substring texto 0 5) "ERROR"))))

(define verificar
  (lambda (etiqueta obtenido esperado)
    (if (string=? obtenido esperado)
        (begin (set! pasadas (+ pasadas 1))
               (eopl:printf "  ok    ~a~%" etiqueta))
        (begin (set! fallidas (+ fallidas 1))
               (eopl:printf "  FALLA ~a: se esperaba ~s y llegó ~s~%"
                            etiqueta esperado obtenido)))))

;; El programa devuelve el texto esperado.
(define prueba
  (lambda (etiqueta texto esperado)
    (verificar etiqueta (intentar texto) esperado)))

;; el chequeador asigna al programa el tipo esperado
(define prueba-tipo
  (lambda (etiqueta texto esperado)
    (verificar etiqueta
               (with-handlers ((exn:fail? (lambda (e) (string-append "ERROR: " (exn-message e)))))
                 (tipo-del-programa texto))
               esperado)))

;; dl programa se rechaza, por el chequeador o por el interprete
(define prueba-rechazo
  (lambda (etiqueta texto)
    (let ((r (intentar texto)))
      (if (empieza-con-error? r)
          (begin (set! pasadas (+ pasadas 1))
                 (eopl:printf "  ok    ~a  [~a]~%" etiqueta r))
          (begin (set! fallidas (+ fallidas 1))
                 (eopl:printf "  FALLA ~a: se esperaba un rechazo y llegó ~s~%" etiqueta r))))))

;; unparse del arbol de un texto vuelve a parsear al mismo arbol
(define ida-y-vuelta
  (lambda (etiqueta texto)
    (let ((una (unparse (scan&parse texto))))
      (verificar etiqueta (unparse (scan&parse una)) una))))

;;; lexico, gramatica, unparse y nucleo
(eopl:printf "~%== Primera entrega ==~%")

(prueba "literal entero" "42" "42")
(ida-y-vuelta "unparse de un literal" "42")

;; pruebas de ejecucion del nucleo (value-of)

;; 1 literales basicos (flotante, cadena, caracter, booleanos y unit)
(prueba "literales flotante y neg" "-3.14" "-3.14")
(prueba "literal cadena" "\"hola mundo\"" "hola mundo")
(prueba "literal caracter y bool" "if true then 'a' else 'b' end" "a")
(prueba "literal unit" "unit" "unit")

;; 2 ligaduras locales let (verificando que no se vean entre si)
(prueba "let multiple y alcance"
        "let x = 10, y = 20 in let x = +(x, 5), y = x in +(x, y) end end"
        "25")

;; 3 referencias (ref, deref, set) y begin
(prueba "celda mutable y begin"
        "let c = ref(0) in begin set c := +(deref(c), 1); set c := +(deref(c), 1); deref(c) end end"
        "2")

;; 4 if con multiples ramas elif y else
(prueba "condicional if-elif-else"
        "let n = 0 in if >(n, 0) then \"positivo\" elif <(n, 0) then \"negativo\" else \"cero\" end end"
        "cero")

;; 5 verificacion de error cuando la condicion del if no es booleana
(prueba-rechazo "if con prueba no booleana"
                "if 1 then 2 else 3 end")

;; 6 ciclo while con mutacion en el almacen
(prueba "ciclo while acumulador"
        "let i = ref(5), acc = ref(0) in begin while >(deref(i), 0) do begin set acc := +(deref(acc), deref(i)); set i := -(deref(i), 1) end end; deref(acc) end end"
        "15")

;; 7 primitivas aritmeticas
(prueba "primitivas aritmeticas"
        "+(*(3, 4), -(/(20, 3), %(10, 3)))"
        "17")

;; 8 verificacion de error en division por cero
(prueba-rechazo "error division por cero"
                "/(10, 0)")

;; 9 primitivas relacionales y logicas (>, <, >=, <=, ==, !=, and, or, not)
(prueba "primitivas relacionales y booleanas"
        "and(and(>=(5, 5), <=(3, 4)), or(==(1, 2), and(!=(1, 2), not(false))))"
        "true")

;; 10 primtivas de cadenas, conversion y print (concat, length, to-string, to-number)
(prueba "primitivas de cadenas y conversion"
        "let s = concat(\"Edad: \", to-string(+(to-number(\"20\"), 5))) in length(s) end"
        "8")

;; 11 procedimientos (proc), aplicacion (apply) y clausuras con alcance lexico
(prueba "clausura y alcance lexico"
        "let factor = 10 in let escalar = proc (v: int) *(v, factor) end in let factor = 1000 in apply escalar (5) end end end"
        "50")

;; 12 paso de referencias como argumentos a procedimientos
(prueba "paso de referencia a proc"
        "let celda = ref(10), duplicar = proc (r: ref(int)) set r := *(deref(r), 2) end in begin apply duplicar (celda); apply duplicar (celda); deref(celda) end end"
        "40")

;; 13 recursion simple con letrec (factorial)
(prueba "letrec factorial"
        "letrec int fact (n: int) = if ==(n, 0) then 1 else *(n, apply fact (-(n, 1))) end in apply fact (5) end"
        "120")

;; 14 recursion mutua con letrec (par e impar)
(prueba "letrec recursion mutua"
        "letrec bool par (n: int) = if ==(n, 0) then true else apply impar (-(n, 1)) end bool impar (n: int) = if ==(n, 0) then false else apply par (-(n, 1)) end in apply par (6) end"
        "true")

;; 15 comentarios de linea y de bloque ignorados por el scanner
(prueba "comentarios linea y bloque"
        "#| comentario de bloque |# let x = 10 // comentario de linea\n in +(x, 5) end"
        "15")

;; Pruebas de ida y vuelta para el unparser (unparse + scan&parse) 

(ida-y-vuelta "unparse 1: literales texto y char" "begin \"hola\"; 'z'; 3.14; true; false; unit end")
(ida-y-vuelta "unparse 2: let multiple" "let a = 1, b = 2 in +(a, b) end")
(ida-y-vuelta "unparse 3: ref deref y set" "let r = ref(10) in begin set r := 20; deref(r) end end")
(ida-y-vuelta "unparse 4: condicional con elif" "if >(x, 0) then 1 elif <(x, 0) then -1 else 0 end")
(ida-y-vuelta "unparse 5: ciclo while" "while <(deref(i), 10) do set i := +(deref(i), 1) end")
(ida-y-vuelta "unparse 6: primitivas aritmeticas" "+(-(10, 5), *(2, /(8, %(5, 3))))")
(ida-y-vuelta "unparse 7: primitivas logicas y relacionales" "or(and(==(a, b), !=(c, d)), not(>=(x, <=(y, z))))")
(ida-y-vuelta "unparse 8: primitivas cadenas y print" "print(concat(to-string(length(\"abc\")), to-string(to-number(\"99\"))))")
(ida-y-vuelta "unparse 9: proc con tipos complejos y apply" "let f = proc (x: int, g: (int -> bool), r: ref(float)) apply g(x) end in unit end")
(ida-y-vuelta "unparse 10: letrec simple" "letrec int fact (n: int) = if ==(n, 0) then 1 else *(n, apply fact(-(n, 1))) end in apply fact(5) end")
(ida-y-vuelta "unparse 11: letrec mutuo" "letrec bool es-par (n: int) = apply es-impar(-(n, 1)) bool es-impar (n: int) = apply es-par(-(n, 1)) in apply es-par(4) end")
(ida-y-vuelta "unparse 12: bloque atomic y abort" "atomic begin set x := 1; abort(\"fallo\") end onfail (msg) print(msg) end")
(ida-y-vuelta "unparse 13: declaracion record y new" "record Cuenta { titular: string, saldo: ref(int) } new Cuenta { titular = \"Ana\", saldo = ref(100) }")
(ida-y-vuelta "unparse 14: acceso a campo field" "record Par { a: int, b: int } let p = new Par { a = 1, b = 2 } in field(p, a) end")
(ida-y-vuelta "unparse 15: programa completo combinado" "record Item { cant: ref(int) } let it = new Item { cant = ref(5) } in atomic set field(it, cant) := 0 onfail (e) -1 end end")

;;; Resumen

(eopl:printf "~%==========================================~%")
(eopl:printf "  pasadas ~s, fallidas ~s~%" pasadas fallidas)
(eopl:printf "==========================================~%")