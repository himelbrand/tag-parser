(load "qq.scm")
(load "tag-parser.scm")
(import (chezscheme))



(assert (equal? (parse '(set! x 3)) '(set (var x) (const 3)) ))
(assert (equal? (parse '(set! v (f x))) '(set	 (var v) (applic (var f) ((var x)))) ))

(assert (equal? (parse 'abc) '(var abc) ))
(assert (equal? (parse '123x) '(var 123x) ))
(assert (equal? (parse '123) '(const 123) ))
(assert (equal? (parse '()) '(const ()) ))
(assert (equal? (parse #\a) '(const #\a) ))
(assert (equal? (parse "oz") '(const "oz") ))
(assert (equal? (parse '(quote a)) '(const a) ))
(assert (equal? (parse '(quote (a b c))) '(const (a b c)) ))
(assert (equal? (parse '(quote (quote a b c))  ) '(const (quote a b c)) ))


(assert (equal? (parse '(if a b c)) '(if3 (var a) (var b) (var c)) ))
(assert (equal? (parse '(if (if a b c)'x '(x y z))) '(if3 (if3 (var a)(var b)(var c))(const x)(const (x y z))) ))
`(assert (equal? (parse '(if a b)) '(if3 (var a) (var b) (const ,(void) ) )))
`(assert (equal? (parse '(if a 4)) '(if3 (var a) (const 4) (const ,(void))) ))
`(assert (equal? (parse '(if #t 'abc)) '(if3 (const #t)(const abc)(const ,(void)) )))



(assert (equal? (parse '(begin x y)) '(seq ((var x) (var y))) ))
(assert (equal? (parse '(begin a)) '(var a) ))
`(assert (equal? (parse '(begin)) '(const ,(void)) ))

(assert (equal? (parse '(lambda (x) (display x) x)) '(lambda-simple (x)
 (seq ((applic (var display) ((var x))) (var x)))) ))

(assert (equal? (parse '(lambda (x) x)) '(lambda-simple (x) (var x)) ))
(assert (equal? (parse '(lambda (x) (+1 2))) '(lambda-simple (x) (applic (const 1) ((const 2)))) ))
(assert (equal? (parse '(lambda (x) (+ 1 2) a b c)) '(lambda-simple
  (x)
  (seq ((applic (var +) ((const 1) (const 2)))
         (var a)
         (var b)
         (var c)))) ))
		 
(assert (equal? (parse '(lambda (x . rest) x)) '(lambda-opt (x) rest (var x)) ))
(assert (equal? (parse '(lambda (x . rest) (display x) (newline) rest)) '(lambda-opt(x) rest
	(seq ((applic (var display) ((var x)))
			(applic (var newline) ())
			(var rest)))) ))
			
(assert (equal? (parse '(define x 5)) '(define (var x) (const 5)) ))
(assert (equal? (parse '(define x (lambda (x) x))) '(define (var x) (lambda-simple (x) (var x))) ))

(assert (equal? (parse '(define (id x) x)) '(define (var id) (lambda-simple (x) (var x))) ))
(assert (equal?  (parse '(define (foo x y z) (if x y z))) '(define (var foo) (lambda-simple (x y z) (if3 (var x) (var y)(var z)))) ))
(assert (equal?  (parse '(define (foo x y . z) (if x y z))) '(define (var foo) (lambda-opt (x y) z (if3 (var x) (var y) (var z)))) ))
(assert (equal?  (parse '(define (list . args) args)) '(define (var list) (lambda-opt () args(var args))) ))


(assert (equal?  (parse '(lambda args (if x y z))) '(lambda-opt () args (if3 (var x) (var y) (var z))) ))
(assert (equal?  (parse '(lambda args args)) '(lambda-opt () args (var args)) ))


(assert (equal?  (parse '(or (zero? x) (zero? y) (zero? z))) '(or ((applic (var zero?) ((var x)))(applic (var zero?) ((var y)))(applic (var zero?) ((var z))))) ))
 (assert (equal?  (parse '(or (or (f1 x) (f2 y)) (or (f3 z) (f4 w) (f5 r)) (and (f6 u) (f7 t)))) '(or ((or ((applic (var f1) ((var x))) (applic (var f2) ((var y)))))(or ((applic (var f3) ((var z)))(applic (var f4) ((var w)))(applic (var f5) ((var r)))))(if3 (applic (var f6) ((var u)))(applic (var f7) ((var t)))(const #f)))) ))
(assert (equal?  (parse '(lambda args args)) '(lambda-opt () args (var args)) ))

(assert (equal?  (parse '(let ((x 1) (y 2)) (display x)(+ x y))) '(applic(lambda-simple (x y)(seq ((applic (var display) ((var x)))(applic (var +) ((var x) (var y))))))((const 1) (const 2))) ))
(assert (equal?  (parse '(let ((x 2)) (let ((y a)) (+ x y) x))) 
'(applic
  (lambda-simple
    (x)
    (applic
      (lambda-simple
        (y)
        (seq ((applic (var +) ((var x) (var y))) (var x))))
      ((var a))))
  ((const 2))) ))

(assert (equal? (parse '(let* ((x 2) (y x)) (+ x y))) '(applic
  (lambda-simple
    (x)
    (applic
      (lambda-simple (y) (applic (var +) ((var x) (var y))))
      ((var x))))
  ((const 2))) ))
 