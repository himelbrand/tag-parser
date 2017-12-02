	(define goo (lambda ()
		(letrec ((x '(he said:)))
			(set-cdr! (last-pair x) '('ha 'ha))
				x)))
			
		(display (goo))
	(display (goo))

