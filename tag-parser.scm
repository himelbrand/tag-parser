(load "qq.scm")

(define reserved-words
'(and begin cond define do else if lambda
let let* letrec or quasiquote unquote
unquote-splicing quote set!))

(define (parse sexp)
	(cond ((myConst? sexp) `(const ,(unquotify sexp)))
		  ((myVar? sexp) `(var ,(unquotify sexp)))
		  ((if3? sexp) `(if3 ,(parse (cadr sexp)) ,(parse (caddr sexp)) ,(parse (cadddr sexp))))
		  ((if2? sexp) `(if3 ,(parse (cadr sexp)) ,(parse (caddr sexp)) ,(list 'const (if #f #f))))
		  ((myOr? sexp) `(or ,(map parse (cdr sexp))))
		  ((myAnd? sexp) `(if3 ,(parse (cadr sexp)) ,(if (> (length (cdr sexp)) 2)
													(parse `(and ,@(cddr sexp))) 
													(parse (caddr sexp))) ,(parse #f)))
		  ((myLambdaSimple? sexp) `(lambda-simple ,(cadr sexp) ,@(map parse (cddr sexp))))

		(else #f))
	  )



(define (myConst? exp)
  (cond ((eq? exp '()) #t)
  		((vector? exp) #t)
		((boolean? exp) #t)
		((number? exp) #t)
		((char? exp) #t)
		((string? exp) #t)
		((quote? exp) #t)
		(else #f)))

(define (myVar? exp)
	(and (not (member exp reserved-words))
		(symbol? exp)))

(define (myCond? exp)
	(cond ((if3? exp) #t)
	      ((if2? exp) #t)
	  	(else #f)))

(define (if3? exp)
	(and (list? exp) (eq? (length exp) 4) (eq? (car exp) 'if)))

(define (if2? exp)
	(and (list? exp) (eq? (length exp) 3) (eq? (car exp) 'if)))

(define (myOr? exp)
  	(and (list? exp) (eq? (car exp) 'or)))

(define (myAnd? exp)
  	(and (list? exp) (eq? (car exp) 'and)))

(define (myLambdaSimple? exp)
	(and (list? exp) (eq? (car exp) 'lambda) (list? (cadr exp)) (> (length exp) 2)))

(define (myLambdaOptional? exp)
	(and (list? exp) (eq? (car exp) 'lambda) (pair? (cadr exp)) (not (list? (cadr exp))) (> (length exp) 2)))  

(define (myLambdaVeriadic? exp)
	(and (list? exp) (eq? (car exp) 'lambda) (> (length exp) 2)))  
