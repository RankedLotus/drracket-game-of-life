#lang racket
(require picturing-programs)
(require 2htdp/universe)


;2D ARRAY
;===================================================================
(define arr
  (make-vector (* 100 100) 0))

(define (toind x y)
  (+ x (* 100 y)))


;DRAWING STUFF
;===================================================================
(define (cc x)
  (+ 5 (* x 10)))

(define ec (rectangle 10 10 "outline" "black"))
ec

(define row (for/fold ([acc (rectangle 0 0 "solid" "transparent")])
          ([i (in-range 0 100)])
  (beside acc ec)))

(define grid (for/fold ([acc (rectangle 0 0 "solid" "transparent")])
          ([i (in-range 0 100)])
  (above acc row)))

;CELLULAR AUTOMATA
;===================================================================
(define (neighbor-sum vec real-x real-y)
  (define x 
  (match real-x
    [0 1]
    [99 98]
    [_ real-x]))

  (define y
  (match real-y
    [0 1]
    [99 98]
    [_ real-y]))
  
  (define n (vector-ref vec (toind x (- y 1))))
  (define nw (vector-ref vec (toind (- x 1) (- y 1))))
  (define w (vector-ref vec (toind (- x 1) y)))
  (define sw (vector-ref vec (toind (- x 1) (+ y 1))))
  (define s (vector-ref vec (toind x (+ y 1))))
  (define se (vector-ref vec (toind (+ 1 x) (+ y 1))))
  (define e (vector-ref vec (toind (+ x 1) y)))
  (define ne (vector-ref vec (toind (+ 1 x) (- y 1))))
  (define sum (+ n nw w sw s se e ne))
  sum)

(define (infection-rule vec x y)
  (define sum (neighbor-sum vec x y))
  (cond
    [(= sum 0) 0]
    [(= sum 1) 0]
    [(= sum 2) 1]
    [(= sum 3) (match (vector-ref vec (toind x y))
                 [0 1]
                 [1 0])]
    [else 0]))

;UNIVERSE
;===================================================================
;;INITIAL STATE
(define init arr)
(vector-set! arr (toind 10 17) 1)
(vector-set! arr (toind 55 67) 1)
(vector-set! arr 1 1)
(vector-set! arr (toind 10 18) 1)
(vector-set! arr (toind 11 17) 1)

;;TICK HANDLER
(define (tickh input)
  (define next-gen (make-vector (* 100 100) 0))
   (for* ([i (in-range 0 100)]
         [j (in-range 0 100)])
     (vector-set! next-gen (toind i j) (infection-rule input i j)))
  next-gen)

;;DRAW FUNCTION
(define (draw input)
  (for*/fold ([acc grid])
             ([i (in-range 0 100)]
              [j (in-range 0 100)])
      (define my-var (vector-ref input (toind i j)))
      (match my-var
        [0 acc]
        [1 (place-image (rectangle 10 10 "solid" "black") (cc i) (cc j) acc)]))
        )

(define (rs) ;(run-sim)
  (big-bang init
    (on-tick tickh)
    (to-draw draw)))