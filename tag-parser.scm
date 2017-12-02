(load "qq.scm")

(define reserved-words
	'(and begin cond define do else if lambda
		let let* letrec or quasiquote unquote
	unquote-splicing quote set!))

(define (parse sexp)
	(cond ((myConst? sexp) `(const ,(unquotify sexp)))
		((myVar? sexp) `(var ,(unquotify sexp)))
		((if3? sexp) 
			(let ((pred (parse (cadr sexp)))
				(then (parse (caddr sexp)))
			(otherwise (parse (cadddr sexp))))
			(if (and pred then otherwise)
				`(if3 ,pred ,then ,otherwise)
			#f)))
		((if2? sexp) 
			(let ((pred (parse (cadr sexp)))
			(then (parse (caddr sexp))))
			(if (and pred then)
				`(if3 ,pred ,then ,(list 'const (if #f #f)))
				#f
			)))
		((myOr? sexp) (cond ((= (length sexp) 1) (parse '#f))
			((= (length sexp) 2) (parse (cadr sexp)))
			(else (let ((orConds (map parse (cdr sexp))))
				(if (> (length (filter (lambda(item) (not item)) orConds)) 0)
					#f
				`(or ,orConds))))))

		((myAnd? sexp) (cond ((= (length sexp) 2) (parse (cadr sexp)))
			((= (length sexp) 1) (parse #t))
			(else (parse `(if ,(cadr sexp) ,(cond ((> (length (cdr sexp)) 2) `(and ,@(cddr sexp)))
			((= (length (cdr sexp)) 2) (caddr sexp)))
		#f)))))

		((myLambdaSimple? sexp) 
			(let ((ls `(lambda-simple ,(cadr sexp) ,(if (> (length (cddr sexp)) 1)
				(parse `(begin ,@(cddr sexp)))
			(parse (caddr sexp))))))
			(if (and (list? ls) (> (length ls) 2) (= (length (filter (lambda (item)item ) (cddr ls))) (length (cddr ls))))
				ls
			#f)))

			((myLambdaOptional? sexp) `(lambda-opt  ,(reverse (cdr (reverse (flatten (cadr sexp)))))  ,(car (reverse (flatten (cadr sexp)))) ,(if (> (length (cddr sexp)) 1)
				(parse `(begin ,@(cddr sexp)))
			(parse (caddr sexp)))))			
			((myLambdaVeriadic? sexp) `(lambda-opt  ,(list) ,(cadr sexp) ,(if (> (length (cddr sexp)) 1)
				(parse `(begin ,@(cddr sexp)))
			(parse (caddr sexp)))))

		((myDefine? sexp) 
			(let ((varName (parse (cadr sexp)))
			(defenition (parse (caddr sexp))))
			(if(and varName defenition)
			`(define ,varName ,defenition)
			#f)
		))
		((myMITDefine? sexp) 
			(let ((varName (parse (car (cadr sexp))))
			(defenition (parse `(lambda ,(cdr (cadr sexp))  ,@(cddr sexp)))))
			(if (and varName defenition)
				`(define ,varName ,defenition)
			#f)
		))
		((mySet!? sexp) 
			(let ((varName (parse (cadr sexp)))
					(newVal (parse (caddr sexp))))
			(if (and varName newVal)
				`(set ,varName ,newVal)
				#f)))
		((myAplication? sexp) 
			(let ((funcName (parse (car sexp)))
					(args (map parse (cdr sexp))))
			(if (and funcName (> (length (filter (lambda(item) (not item)) orConds)) 0))
				`(applic ,funcName ,args)
				#f)))
		((myBegin? sexp) (cond ((= (length sexp) 1) (list 'const (if #f #f)))
			((= (length (cdr sexp)) 1) (parse (cadr sexp)))
			(else `(seq 
				,(letrec ((fun (lambda (l) 
					(fold-left (lambda (init exp) 
						(append init 
							(if (myBegin? exp)
								(fun (cdr exp))
							(list (parse exp)))))
					'()
				l))))
			(fun (cdr sexp)))
			))))

		((myLet? sexp) (parse `((lambda ,(map (lambda (pair) (car pair)) (cadr sexp)) ,@(cddr sexp)) ,@(map (lambda (pair) (cadr pair)) (cadr sexp)))))
		((myLet*? sexp) (parse (if (< (length (cadr sexp)) 2)
			`(let ,(cadr sexp)  ,@(cddr sexp))
		`(let ,(list (car (cadr sexp)))  (let* ,(cdr (cadr sexp)) ,@(cddr sexp))))))
		((myLetrec? sexp) (parse `(let ,(map (lambda (pair) (list (car pair) #f)) (cadr sexp)) 
			,@(map (lambda (pair) `(set! ,(car pair) ,(cadr pair))) (cadr sexp))
		((lambda () ,@(cddr sexp))))))
		((myCond? sexp) (cond ((eq? (car (cadr sexp)) 'else) (parse `(begin ,@(cdr (cadr sexp)))))
			((eq? (length (cdr sexp)) 1)  (parse `(if ,(car (cadr sexp)) (begin ,@(cdr (cadr sexp))) '#f)))
			(else  (parse `(if ,(car (cadr sexp)) (begin ,@(cdr (cadr sexp))) (cond ,@(cddr sexp)))))
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

(define (myDefineInLambda? exp)
(and (myDefine? exp) (not (equal? (cadr exp) (caddr exp)))))
(define (myCheckLambdaDefineOrder? exp)
	(letrec ((fun (lambda (lst bool startWithDefine?)
		(cond ((null? lst) bool)
			((not startWithDefine?) (and bool (not (myMITDefineOrDefine? (car lst))) (not (myDefineInLambda? (car lst))) (fun (cdr lst) bool startWithDefine?)))
			(startWithDefine? (if (myMITDefineOrDefine? (car lst)) 
				(if (myDefineInLambda? (car lst))
					(fun (cdr lst) bool startWithDefine?)
				#f ) 
			(fun (cdr lst) bool (not startWithDefine?)))) ))))		  							  
	(if(myMITDefineOrDefine? (caddr exp))
		(if (myDefineInLambda? (caddr exp)) (fun (cdddr exp) #t #t) #f)
	(fun (cdddr exp) #t #f))))



(define (myImpToProper lst)
	(letrec ((fun (lambda (l templ)
		(if (not (pair? l)) (append templ (list l)) 
		(fun (cdr l) (append  templ (list (car l))))))))
(fun lst '()))
)
(define (myHasDuplicates? l)
	(cond ((null? l) #f)
		((and (list? l))(not (not (member (car l) (cdr l)))))
	(else (myHasDuplicates? (cdr l)))))

(define (myAllVars? l)
	(cond ((null? l) #t)
		((and (not (pair? l))(myVar? l)) #t)
		((myVar? (car l)) (myAllVars? (cdr l)))	
	(else #f)))

(define (myLambdaSimple? exp)
	(and (list? exp) (eq? (car exp) 'lambda) (list? (cadr exp)) (> (length exp) 2)
		(not (myHasDuplicates? (cadr exp)))
		(fold-left (lambda (init varBool) (and init varBool)) #t (map (lambda(var) (myVar? var))  (cadr exp)))
		(myCheckLambdaDefineOrder? exp)
	))

(define (myLambdaOptional? exp)
	(and (list? exp) (eq? (car exp) 'lambda)
		(pair? (cadr exp)) 
		(not (list? (cadr exp))) 
		(> (length exp) 2) 
		(not (myHasDuplicates? (myImpToProper (cadr exp))))
		(myAllVars? (cadr exp))
		(myCheckLambdaDefineOrder? exp)
	))  

(define (myLambdaVeriadic? exp)
(and (list? exp) (eq? (car exp) 'lambda) (> (length exp) 2) (myVar? (cadr exp))(myCheckLambdaDefineOrder? exp)))  




(define (myDefine? exp)
		(and (list? exp) (eq? (length exp) 3) (eq? (car exp) 'define) (myVar? (cadr exp))));;Changed define - allowed nested defines in this assignment
(define (myMITDefine? exp)
	(and (list? exp)
		(> (length exp) 2)
		(eq? (car exp) 'define)
		(pair? (cadr exp))
		(fold-left (lambda (init varBool) (and init varBool)) #t (map (lambda(var) (myVar? var))  (cadr exp)))
			 (not (myHasDuplicates? (cdadr exp)));;TODO: nested defines?
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
		(>= (length exp) 1)
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

