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
		  ((myOr? sexp) (cond ((= (length sexp) 1) (parse '#f))
		  					  ((= (length sexp) 2) (parse (cadr sexp)))
		  					  (else `(or ,(map parse (cdr sexp))))))

		  ((myAnd? sexp) (cond [(= (length sexp) 2) (parse (cadr sexp))]
		  					   [(= (length sexp) 1) (parse #t)]
		  					 	[else (parse`(if ,(cadr sexp) ,(cond [(> (length (cdr sexp)) 2) `(and ,@(cddr sexp))]
		  												   			 [(= (length (cdr sexp)) 2) (caddr sexp)])
		  												   #f))]))
													
		  ((myLambdaSimple? sexp) `(lambda-simple ,(cadr sexp) ,(if (> (length (cddr sexp)) 1)
		  														 (parse `(begin ,@(cddr sexp)))
		  															(parse (caddr sexp)))))

		  ((myLambdaOptional? sexp) `(lambda-opt  ,(reverse (cdr (reverse (flatten (cadr sexp)))))  ,(car (reverse (flatten (cadr sexp)))) ,(if (> (length (cddr sexp)) 1)
		  																																	 (parse `(begin ,@(cddr sexp)))
		  																																		(parse (caddr sexp)))))			
		  ((myLambdaVeriadic? sexp) `(lambda-opt  ,(list) ,(cadr sexp) ,(if (> (length (cddr sexp)) 1)
		  																	(parse `(begin ,@(cddr sexp)))
		  																	(parse (caddr sexp)))))

		  ((myDefine? sexp) `(define ,(parse (cadr sexp)) ,(parse (caddr sexp))))
		  ((myMITDefine? sexp) `(define ,(parse (car (cadr sexp))) ,(parse `(lambda ,(cdr (cadr sexp))  ,@(cddr sexp)))))
		  ((mySet!? sexp) `(set ,(parse (cadr sexp)) ,(parse (caddr sexp)) ))
		  ((myAplication? sexp) `(applic ,(parse (car sexp)) ,(map parse (cdr sexp))))
		  ((myBegin? sexp) (if (= (length (cdr sexp)) 1)
		  					   (parse (cadr sexp))
		  						 `(seq 
		  						 	,(letrec ((fun (lambda (l) 
		  						 			(fold-left (lambda (init exp) 
		  											 (append init 
		  											 (if (myBegin? exp)
		  											 (fun (cdr exp))
		  											 (list (parse exp)))))
		  									'()
		  									l))))
		  						 (fun (cdr sexp)))
		  						 )))
		  						
		  ((myLet? sexp) (parse `((lambda ,(map (lambda (pair) (car pair)) (cadr sexp)) ,@(cddr sexp)) ,@(map (lambda (pair) (cadr pair)) (cadr sexp)))))
		  ((myLet*? sexp) (parse (if (< (length (cadr sexp)) 2)
		  							`(let ,(cadr sexp)  ,@(cddr sexp))
		  							`(let ,(list (car (cadr sexp)))  (let* ,(cdr (cadr sexp)) ,@(cddr sexp))))))
		  ((myLetrec? sexp) (parse `(let ,(map (lambda (pair) (list (car pair) #f)) (cadr sexp)) 
		  								 ,@(map (lambda (pair) `(set! ,(car pair) ,(cadr pair))) (cadr sexp))
		  								   ((lambda () ,@(cddr sexp))))))
		  ((myCond? sexp) (cond [(eq? (car (cadr sexp)) 'else) (parse `(begin ,@(cdr (cadr sexp))))]
		  						[(eq? (length (cdr sexp)) 1)  (parse `(if ,(car (cadr sexp)) (begin ,@(cdr (cadr sexp))) '#f))]
		  						[else  (parse `(if ,(car (cadr sexp)) (begin ,@(cdr (cadr sexp))) (cond ,@(cddr sexp))))]
		  					))
		  ((myQQ? sexp) (let ((expend (expand-qq (cadr sexp))))
		  				(parse expend)

		  ))

		  	;;(parse `(if ,(car (cadr sexp)) ,(cadr (cadr sexp)) ,(if (= (length (cdr sexp)) 1)
		  	;;																	  '#f
		  	;;																	   `(if )
		  	;;																   ) )))

		(else #f))
	  )

(define (myQQ? exp)
	(and (eq? (car exp) 'quasiquote)
		 (= (length exp) 2)))

(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))

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

(define (myMITDefineOrDefine? exp)
		(or (myMITDefine? exp) (myDefine? exp)))

(define (myCheckLambdaDefineOrder? exp)
	(letrec ((fun (lambda (l bool s)  
		  						(cond [(null? l) bool]
		  							  [(not s) (and bool (not (myMITDefineOrDefine? (car l))) (fun (cdr l) bool s))]
		  							  [s (if(myMITDefineOrDefine? (car l)) (fun (cdr l) bool s) (fun (cdr l) bool (not s)))] ))))
		  							  ;;[else (and (myDefine? (car l)) (fun (cdr l) bool))]))))
				    (if(myMITDefineOrDefine? (caddr exp))
				      (fun (cdddr exp) #t #t)
				  	  (fun (cdddr exp) #t #f))))

(define (myLambdaSimple? exp)
	(and (list? exp) (eq? (car exp) 'lambda) (list? (cadr exp)) (> (length exp) 2)
		  (myCheckLambdaDefineOrder? exp)
		))

(define (myLambdaOptional? exp)
	(and (list? exp) (eq? (car exp) 'lambda) (pair? (cadr exp)) (not (list? (cadr exp))) (> (length exp) 2) (myCheckLambdaDefineOrder? exp)))  

(define (myLambdaVeriadic? exp)
	(and (list? exp) (eq? (car exp) 'lambda) (> (length exp) 2) (myCheckLambdaDefineOrder? exp)))  




(define (myDefine? exp)
		(and (list? exp) (eq? (length exp) 3) (eq? (car exp) 'define) (myVar? (cadr exp))))

(define (myMITDefine? exp)
		(and (list? exp)
			 (> (length exp) 2)
			 (eq? (car exp) 'define)
			 (pair? (cadr exp))
			 (fold-left (lambda (init varBool) (and init varBool)) #t (map (lambda(var) (myVar? var)) (flatten (cadr exp))))
			))

(define (mySet!? exp)
		(and (list? exp)
			 (eq? (length exp) 3)
			 (eq? (car exp) 'set!)
			 (myVar? (cadr exp))))

(define (myAplication? exp)
		(and (list? exp)
			  (not (member (car exp) reserved-words))))

(define (myBegin? exp)
		(and (list? exp)
		(> (length exp) 1)
		(eq? (car exp) 'begin)))

(define (myLet? exp)
		(and (list? exp)
		     (> (length exp) 2)
		     (eq? (car exp) 'let)
		 	 (list? (cadr exp)) 
		 	 (fold-left (lambda (init exp) (and init (list? exp) (= (length exp) 2) (myVar? (car exp)))) #t (cadr exp))))

(define (myLet*? exp)
		(and (list? exp)
		     (> (length exp) 2)
		     (eq? (car exp) 'let*)
		 	 (list? (cadr exp)) 
		 	 (fold-left (lambda (init exp) (and init (list? exp) (= (length exp) 2) (myVar? (car exp)))) #t (cadr exp))	 	))

(define (myLetrec? exp)
		(and (list? exp)
		     (> (length exp) 2)
		     (eq? (car exp) 'letrec)
		 	 (list? (cadr exp)) 
		 	 (fold-left (lambda (init exp) (and init (list? exp) (= (length exp) 2) (myVar? (car exp)))) #t (cadr exp))	 ))

(define (myCond? exp)
	(and (list? exp)
		 (> (length exp) 1)
		(eq? (car exp) 'cond)
		(fold-left (lambda (init exp) (and init (not (eq? (car exp) 'else)))) #t (myRemoveLast (cdr exp)))
		(fold-left (lambda (init exp) (and init (list? exp) (> (length exp) 0) )) #t (cdr exp))	 ))

(define (myRemoveLast exp)
		(reverse (cdr (reverse exp))))

