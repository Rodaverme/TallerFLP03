#lang eopl

; -----------------------------------------  Integrantes --------------------------------------------------

;; Jean Carlo Castañeda Trujillo - 202610106
;; Andres Mauricio Muñoz Garcia - 202610104
;; Jean Carlo Sanchez Parra - 202610123
;; Ronald David Vera Meneses - 202610127

; -----------------------------------------  Declaración IA ----------------------------------------------

;; Se hizo uso de la IA para reforzar conceptos teoricos de los ejercicios
;; también para realizar aclaraciones sobre el enunciado.

; -----------------------------------------  Ejercicios --------------------------------------------------

;;;;******************************************************************************************************************************************************************************************************************************************

;;;; INTERPRETADOR TALLER 3 V4



;;; DEFINICION BNF DEL LENGUAJE DADA EN EL TALLER


;<programa> :=  <expresion>

;               un-programa (exp)

;<expresion> := <numero>

;              numero-lit (num)

;            := "\""<texto> "\""

;               texto-lit (txt)

;            := <identificador>

 ;              var-exp (id)

  ;           := (<expresion> <primitiva-binaria> <expresion>)

   ;            primapp-bin-exp (exp1 prim-binaria exp2)

    ;          := <primitiva-unaria> (<expresion>)

     ;          primapp-un-exp (prim-unaria exp)

;<primitiva-binaria> :=  + (primitiva-suma)

; :=  ~ (primitiva-resta)      Observe que al tratarse de notación infija, resta la trabajaremos con la virgulilla para evitar confusión entre números negativos y la operación resta. Ejemplo: (4 ~ 5) -> -1  y (4 ~ -5) ->9 

;:=  / (primitiva-div)

;  :=  * (primitiva-multi)

;  :=  concat (primitiva-concat)

;  := > (primitiva-mayor)

;  := < (primitiva-menor)

;  := >= (primitiva-mayor-igual)

; := <= (primitiva-menor-igual)

; := != (primitiva-diferente)

; := == (primitiva-comparador-igual)

;<primitiva-unaria>:=  longitud (primitiva-longitud)

;           :=  add1 (primitiva-add1)

;            :=  sub1 (primitiva-sub1)

;            := neg (primitiva-negacion-booleana)



;Especificacion Lexica

(define scaner-interpreter
  '((white-sp
    (whitespace)skip)
  (comment
   ("%"(arbno(not #\newline)))skip)
   (texto
   (letter (arbno (or letter digit "_"))) string)
   (identificador
    ("@" letter (arbno (or letter digit "_")))symbol)
   (numero
    (digit (arbno digit))number)
   (numero
    ("-" digit (arbno digit))number)
   (numero
    (digit (arbno digit) "." digit (arbno digit)) number)
   (numero
    ("-" digit (arbno digit) "." digit (arbno digit))number
  )))


;Especificacion sintetica (gramatica)

(define grammar-simple-interpreter
  '((programa (expresion) un-programa)
    (expresion(numero)numero-lit)
    (expresion("\"" texto "\"")texto-lit)
    (expresion(identificador) var-exp)
    
    (expresion
     ("(" expresion primitiva-binaria expresion ")")
     primapp-bin-exp)
    (expresion
     (primitiva-unaria "(" expresion ")")
     primapp-un-exp)

    (expresion
     ("Si" expresion "{" expresion "}" "sino" "{" expresion "}" )
     condicional-exp)

    ;;Declaracion de variables locales
    (expresion
     ("declarar" "(" (arbno identificador "=" expresion ";") ")" "{" expresion "}")
     variableLocal-exp)
    
    ;;Creacion de procedimientos
    (expresion
     ("procedimiento" "(" (separated-list identificador ",") ")" "{" expresion "}")
     procedimiento-exp)

    ;; Evaluación de procedimientos
    (expresion
     ("evaluar" expresion "(" (separated-list expresion ",") ")" "finEval") app-exp)

    ;;Llamados recursivos
    (expresion
     ("declarar-rec" identificador "(" (separated-list identificador ",") ")" "=" "{"
                     expresion "}" "en" "{" expresion "}") rec-exp)

    
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)
    (primitiva-binaria (">") primitiva-mayor)
    (primitiva-binaria ("<") primitiva-menor)
    (primitiva-binaria (">=") primitiva-mayor-igual)
    (primitiva-binaria ("<=") primitiva-menor-igual)
    (primitiva-binaria ("!=") primitiva-diferente)
    (primitiva-binaria ("==") primitiva-comparador-igual)
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    (primitiva-unaria ("neg") primitiva-neg-bool)
       )
      )
     
    


;Tipos de datos para la sintaxis abstracta de la gramática

 ;Construidos automáticamente:

(sllgen:make-define-datatypes scaner-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scaner-interpreter grammar-simple-interpreter)))
;Parser, Scanner, Interfaz

(define scan&parse
  (sllgen:make-string-parser scaner-interpreter grammar-simple-interpreter))

(define just-scan
  (sllgen:make-string-scanner  scaner-interpreter grammar-simple-interpreter))

(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm) (eval-program  pgm))
    (sllgen:make-stream-parser 
      scaner-interpreter
      grammar-simple-interpreter)))





;el interprete


(define eval-program
  (lambda(pgm)
    (cases programa pgm
      (un-programa(body)
                (eval-expression body(init-env))))))



;Ambiente inicial

(define init-env
  (lambda ()
    (extend-env
     '(@a @b @c @d @e)
     (list 1 2 3 "hola" "FLP")
     (empty-env))))

;Determina si un numero representa verdadero
(define valor-verdad?
  (lambda (num)
    (not(zero? num))))

; #t -> 1, #f -> 0
(define transformar-bool
  (lambda (bool)
    (if bool 1 0)))

;enval-expression: <expresion><enviroment> -> valor
(define eval-expression
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (num) num)
      (texto-lit (txt) txt)
      (var-exp (id) (find-var id env))
      (primapp-bin-exp (exp1 prim-binaria exp2)
                       (let ((arg1 (eval-expression exp1 env))
                             (arg2 (eval-expression exp2 env)))
                         (apply-primitive-bin prim-binaria arg1 arg2)))
      (primapp-un-exp (prim-unaria exp)
                      (let ((arg (eval-expression exp env)))
                        (apply-primitive-un prim-unaria arg)))
      (condicional-exp (test-exp true-exp false-exp)
                       (if (valor-verdad? (eval-expression test-exp env))
                               (eval-expression true-exp env)
                               (eval-expression false-exp env)))

      ;; Evaluacion de variables locales
      (variableLocal-exp (ids exps cuerpo)
                         (let loop ((ids-rest ids)
                                    (exps-rest exps)
                                    (current-env env))
                           (if (null? ids-rest)
                               (eval-expression cuerpo current-env)
                               (let ((val (eval-expression (car exps-rest) current-env)))
                                 (loop (cdr ids-rest)
                                       (cdr exps-rest)
                                       (extend-env (list (car ids-rest)) (list val) current-env))))))

      ;; Evaluacion de procedimientos
      (procedimiento-exp (ids cuerpo)
                         (closure ids cuerpo env))
      (app-exp (exp exps)
               (let ((proc (eval-expression exp env))
                     (args (map (lambda (e) (eval-expression e env)) exps)))
                 (apply-procedure proc args)))
      
      (rec-exp (proc-name ids body-proc cuerpo-principal)
               (eval-expression cuerpo-principal
                                (recursively-extended-env-record proc-name ids body-proc env))))))


;apply-primitive-bin: <primitiva-binaria>valor valor -> valor

(define apply-primitive-bin
  (lambda (prim arg1 arg2)
    (cases primitiva-binaria prim
      (primitiva-suma ()(+ arg1 arg2))
      (primitiva-resta ()(- arg1 arg2))
      (primitiva-div ()(/ arg1 arg2))
      (primitiva-multi ()(* arg1 arg2))
      (primitiva-concat ()(string-append arg1 arg2))
      (primitiva-mayor ()(transformar-bool (> arg1 arg2)))
      (primitiva-menor ()(transformar-bool (< arg1 arg2)))
      (primitiva-mayor-igual ()(transformar-bool (>= arg1 arg2)))
      (primitiva-menor-igual ()(transformar-bool (<= arg1 arg2)))
      (primitiva-diferente ()(transformar-bool (not(= arg1 arg2))))
      (primitiva-comparador-igual ()(transformar-bool (= arg1 arg2))))))


;apply primitive unitario

(define apply-primitive-un
  (lambda(prim arg)
        (cases primitiva-unaria prim
          (primitiva-longitud () (string-length arg))
          (primitiva-add1 () (+ arg 1))
          (primitiva-sub1 () (- arg 1))
          (primitiva-neg-bool () (transformar-bool (not (valor-verdad? arg))))
          )))


;Datatype procval

(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expresion?)
    (env environment?)))


;Apply Procedure

(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))




;Datatype environment
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms(list-of symbol?))
                       (vals(list-of scheme-value?))
                       (env environment?))
  (recursively-extended-env-record
   (proc-name symbol?)
   (ids (list-of symbol?))
   (body expresion?)
   (old-env environment?)))
(define scheme-value? (lambda (v) #t))



;empty-env

(define empty-env
  (lambda ()
    (empty-env-record)))


;extend-env
(define extend-env
  (lambda(syms vals env)
    (extended-env-record syms vals env)))


;apply-env
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env env sym))))
      (recursively-extended-env-record (proc-name ids body old-env)
                                       (if (eqv? sym proc-name)
                                           (closure ids body env)
                                           (find-var sym old-env))))))



(define find-var
  (lambda(sym env)
    (cases environment env
      (empty-env-record ()
                         "Error, variable does not exist")
      (extended-env-record (syms vals env)
                           (let((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (find-var sym env))))
      (recursively-extended-env-record (proc-name ids body old-env)
                                       (if (eqv? sym proc-name)
                                           (closure ids body env)
                                           (find-var sym old-env))))))
                             
                         

                      



;list-find-position
(define list-find-position
  (lambda (sym los)
    (list-index(lambda (syml) (eqv? syml sym))los)))

;list index

(define list-index
  (lambda(pred ls)
    (cond
      ((null? ls )#f)
      ((pred (car ls))0)
      (else (let((list-index-r(list-index pred (cdr ls))))
              (if (number? list-index-r)
                  (+ list-index-r 1)#f))))))


;Punto A @sumarDigitos  y evaluar @sumarDigitos(147) finEval

(eval-program
 (scan&parse
  "declarar-rec @mod10(@n) = { Si (@n < 10) { @n } sino { evaluar @mod10((@n ~ 10)) finEval } } en { declarar-rec @div10(@n) = { Si (@n < 10) { 0 } sino { (1 + evaluar @div10((@n ~ 10)) finEval) } } en { declarar-rec @sumarDigitos(@n) = { Si (@n < 10) { @n } sino { (evaluar @mod10(@n) finEval + evaluar @sumarDigitos(evaluar @div10(@n) finEval) finEval) } } en { evaluar @sumarDigitos(147) finEval } } }"))
;(interpretador)
;--> declarar-rec @mod10(@n) = {
;Si (@n < 10)
;{ @n }
;sino { evaluar @mod10((@n ~ 10)) finEval } }
;en {
;declarar-rec @div10(@n) =
;{ Si (@n < 10) { 0 }
;sino
;{ (1 + evaluar @div10((@n ~ 10)) finEval) } }
;en { declarar-rec @sumarDigitos(@n) =
;{ Si (@n < 10) { @n }
;sino {
;(evaluar @mod10(@n) finEval + evaluar @sumarDigitos(evaluar @div10(@n) finEval) finEval) } } en { evaluar @sumarDigitos(147) finEval } } }
;12



;PUNTO B @SumaNaturales

;Prueba recursiva con el 5
  (eval-program
 (scan&parse
  "declarar-rec @sumaNaturales(@n) = { Si (@n == 0) { 0 } sino { (@n + evaluar @sumaNaturales((@n ~ 1)) finEval) } } en { evaluar @sumaNaturales(5) finEval }"))

;> (interpretador)
;--> declarar-rec @sumaNaturales(@n) = {
;Si (@n == 0)
;{ 0
;} sino {
;(@n + evaluar @sumaNaturales((@n ~ 1)) finEval) }
;} en {
;evaluar @sumaNaturales(5) finEval }
;)
;)
;15




  (eval-program
 (scan&parse
  "declarar-rec @sumaNaturales(@n) = { Si (@n == 0) { 0 } sino { (@n + evaluar @sumaNaturales((@n ~ 1)) finEval) } } en { evaluar @sumaNaturales(10) finEval }"))

;> (interpretador)
;--> declarar-rec @sumaNaturales(@n) = {
;Si (@n == 0)
;{ 0
;} sino {
;(@n + evaluar @sumaNaturales((@n ~ 1)) finEval) }
;} en {
;evaluar @sumaNaturales(10) finEval }
;)
;)
;15



;Punto C @multiplicar(a,b)
(eval-program
 (scan&parse
  "declarar-rec @multiplicar(@a, @b) = {
     Si (@a == 0) {
       0
     } sino {
       (@b + evaluar @multiplicar( (@a ~ 1), @b ) finEval)
     }
   } en {
     evaluar @multiplicar(4,3) finEval
   }"))

;Punto D @sumaParesRango(a,b)
(eval-program
 (scan&parse
  "declarar-rec @sumaParesRango(@a, @b) = {
     declarar-rec @esPar(@n) = {
       Si (@n == 0) {
         1
       } sino {
         Si (@n == 1) {
           0
         } sino {
           evaluar @esPar((@n ~ 2)) finEval
         }
       }
     } en {
       Si (@a > @b) {
         0
       } sino {
         declarar (@par = evaluar @esPar(@a) finEval;) {
           Si (@par == 1) {
             (@a + evaluar @sumaParesRango((@a + 1), @b) finEval)
           } sino {
             evaluar @sumaParesRango((@a + 1), @b) finEval
           }
         }
       }
     }
   } en {
     evaluar @sumaParesRango(2, 7) finEval
   }"))


; Punto E - Decorador de Integrantes

(eval-program
 (scan&parse
  "declarar (
     @integrantes = procedimiento () { \"Jean_Andres_Jean_Ronald\" };
     @saludar = procedimiento (@f) {
                  procedimiento () {
                    (\"Hola_\" concat evaluar @f () finEval)
                  }
                };
     @decorate = evaluar @saludar (@integrantes) finEval;
   ) {
     evaluar @decorate () finEval
   }"))


; (interpretador)
;declarar (
;  @integrantes = procedimiento () { "Jean_Andres_Jean_Ronald" };
;  @saludar = procedimiento (@f) {
;               procedimiento () {
;                 ("Hola_" concat evaluar @f () finEval)
;               }
;             };
;  @decorate = evaluar @saludar (@integrantes) finEval;
;) {
;  evaluar @decorate () finEval
;}

