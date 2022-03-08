#lang racket
;; what are we trying to do here, because they say that it teaches about how the racket core i.e the compiler processes the code that we write.
;; They talk in terms of the language of the compiler being a recursive definition.  Language of the compiler in a sene means the language provided by the compiler which can be used to program things. So, it is the language it understands and also provides.
;; What do you mean by making this language extensible? Clearly the language understood by the compiler is only a recursive definition, but when we say extensible, we
;; mean that extensibe not from the perspective of the user, but from the perspectiver of the programmer using the language. That is, he can use the base language, to
;; the syntax for his convinience. But how does one extend the syntax for himself, when the compiler only understands the base language?
;; the base language itself gives you the facility. 


;; A LAM is one of:                         Its Interpretation is:

;; -- VAR                                   -- a variable ref

;; -- NUM                                   -- a literal constant

;; -- (lambda (VAR) LAM)                    -- a 1-argument function

;; -- (+ LAM LAM)                           -- an addition expression

;; -- (LAM LAM)                             -- a function application

;;

;; NUM is a (Racket) number

;;

;; VAR is a sequence of keyboard characters (not a Racket number)
;; The next two lines make the language extensible:
;; -- (let-syntax (VAR META) LAM)           -- a macro definition

;; -- (VAR LAM ...)                         -- a macro use IF VAR is declared

;;

;; META is a language for writing down functions that construct LAMs


;; how does the let-syntax enable the programmer to extend the language for himself. 
;; the meta line 35 is the sole reason. It helps you write functions that are syntax transformers/ 
;; Again the special thing about meta language is that it is a language to construct these object languages. 
;; So your object language LAM has the built in support of its source i.e the META language. 
;; How can Lisp give ths support when others couldn't? especially if they are made from the same metalanguage.
;; I dont know may be our LAM is very close to the meta language.  IN a sense, that our LAM is the meta language. Or maybe the LAM is racket, and meta is Lisp.
;; What property do you think makes a meta language a meta thing? And how does our LAM provide that to us?
;; This LAM is weird in the sense that it has META INSIDE IT -- THE Language it was created in. 


;; The simulation starts with turning sample programs represented in strings to syntax trees.  Because this is what happens and it is simulated in racket.
;; The question is, given the above LAM, why does the compiler make a syntax-tree?  The LAM above described both syntax and semantics(thanks to parens '('), but the program is written as a character stream and it needs to be converted to the language repr. And in this repr we use the struct called syntax.
;; One of the most important things governing the meaning is parens '(' and ')'
; A SyntaxTree is one of: 
; – (Syntax Number) 
; – (Syntax Symbol)
; – (Syntax (list SyntaxTree ... SyntaxTree))
;
;
;;(struct Syntax (e {stuff #:auto}) #:transparent #:auto-value 'stuff)
;; the difference between LAM and the syntaxtree is that th eformer emphasises the grammar more, and the latter the semantics.
;; but then the question is how come the list of trees is nnot defined in the language?
;; the list is implied from the (LAM LAM) thing. This can be seen as pair of LAM'S, which can be written as a list.
;; How is a pair related to a list. A list is a special pair. SO when I say pair of s-exps, for example (+ 5 5) is represented in pairs of s-exp as. 
;; one pointer of pair ponting to + and the other part pointing to another pair that points to 5&5 respectively. 
;; A pair is used to represent the pair part of s-exp.
;; A list is used to represent code i.e a list of s-exps.
;; a pair is a pair of s-exps(atom or pair).
;; a list is a pair of an s-exp and list.



(define program-as-text "((lambda (x) (+ x 10)) 42)")

(define macro-as-text
  (string-append
    "((lambda (y)"
    "   (let-syntax (plus10"
    "               (lambda (stx)"
    "                 (match stx"
    "                   [`(plus10 ,x) `(+ ,x 10)])))"
    "     (plus10 y)))"
    " 42)"))


(define another-example
  (string-append
    "((lambda (fun)"
    "   (fun "
    "    (let-syntax (syn (lambda (stx)"
    "                         23))"
    "      (syn (fun 1) 20))))"
    " (lambda (x) (+ x 1)))"))


(struct Syntax (e {stuff #:auto}) #:transparent #:auto-value 'stuff)

; S-expression -> SyntaxTree
(define (to-Syntax stx)
  (cond
    [(cons? stx) (Syntax (map to-Syntax stx))]
    [else (Syntax stx)]))


; String -> S-exp
; read string (of characters) and transform into an S-expression
#;(define (lex str) ; also known as READER 
 #; ... read ... to-Syntax ...)


(define (parse s table) ; also known as EXPANDER 
  (match (Syntax-e s)
    [(? symbol?) s]

    [(? number?) s]

    [`(,(Syntax lambda _) ,(Syntax `(,(Syntax (? symbol? parameter) _)) _) ,body)
      (Syntax (list 'lambda parameter (parse body table)))]

    [`(,(Syntax '+ _) ,lop ,rop)
      (Syntax (list '+ (parse lop table) (parse rop table)))]

    [_ (error 'parse "~s is not grammatical" s)]
    [`(,(Syntax 'let-syntax _) ,(Syntax `(,(Syntax (? symbol? name) _) ,rhs) _) ,body)
      (define transformer (make-transformer (from-Syntax rhs)))
      (parse body (extend-table name transformer table))]

    [`(,(Syntax (? (compose (in? table)) m) _) ,more ...)
      (define transformer (retrieve m table))
      (parse (transformer s) table)]

    [`(,fun ,arg) (Syntax (list 'app (parse fun table) (parse arg table)))]))

;; Now my question is the cases dont contain forms like define, let etc. And oddly enough, there is a form of +. SO why is it in that way, and also the table only keeps track of macro defnitions, where are the environment variable and function bindings?
;;  Okay so the example set that we are working with contains these forms only. SO all the others like define arent used here.
;;  Secondly, I think this small set is chosen because we they are trying to show us how macros are dealt differently. 
;;  BY first, storing an uncore form in the table, and then processing it. 

(define-syntax (ex stx)
  #'5)

(ex 1)
