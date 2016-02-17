; Sim

(use srfi-27) ; Random bit generator

(define K1 1.0)
(define K2 0.31622776601683794)
(define N 1000)
(define STEPS 100)

(define Zero-Position '(0 0 0))
(define (sqr x) (* x x))

(define *positions-of-aircrafts* '())
(define *received-datagram* '())

(define (init-positions-of-aircrafts position-of-aircrafts n)
  (if (> n 0)
      (let ([new-position (list (random-real) (random-real) (random-real))])
	(append (list new-position)
		(init-positions-of-aircrafts position-of-aircrafts (- n 1))))
      position-of-aircrafts))

(define (init-received-datagram received-datagram n)
  (make-list n 0))

(define (squared-distance p1 p2)
  (let ([p1x (car p1)]
	[p1y (cadr p1)]
	[p1z (caddr p1)]
	[p2x (car p2)]
	[p2y (cadr p2)]
	[p2z (caddr p2)])
    (let ([dx (- p1x p2x)]
	  [dy (- p1y p2y)]
	  [dz (- p1z p2z)])
      (+ (sqr dx) (sqr dy) (sqr dz)))))

(define (distance p1 p2)
  (sqrt (squared-distance p1 p2)))

(define (conductance p1 p2)
  (let ([c (exp (- (* (squared-distance p1 p2) K2)))])
    (* K1 c)))

(define (receive-datagram current-datagram conductances)
  (let ([ret '()])
    (for-each (lambda (conductance) (let ([r (random-real)])
				      (if (> r conductance)
					  (set! ret (append '(1) ret))
					  (set! ret (append '(0) ret)))))
	      conductances)
    (map (lambda (x y) (+ x y)) ret current-datagram)))

(define (sum xs)
  (fold (lambda (x a) (+ x a)) 0 xs))

(define (count xs)
  (fold (lambda (x a) (if (> x 0)
			  (+ a 1)
			  a))
	0
	xs))

(define (diff xs ys)
  (map (lambda (x y) (if (and (> x 0) (= y 0))
			 1
			 0))
       xs
       ys))

(define (diff-with-n xs ys n)
  (map (lambda (x y) (if (and (> x 0) (= y 0))
			 n
			 0))
       xs
       ys))

(define (main args)
  (let* ([p (init-positions-of-aircrafts *positions-of-aircrafts* N)]
	 [c (map (lambda (x) (conductance Zero-Position x)) p)])
    (let loop ([n 0]
	       [r (init-received-datagram *received-datagram* N)])
      (if (< n STEPS)
	  (let ([rr (receive-datagram r c)])
	    #;(print (sum (diff-with-n rr r n)))
	    (print (count rr))
	    (loop (+ n 1) rr))))))
