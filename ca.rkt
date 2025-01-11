#lang racket
(require picturing-programs)
(require 2htdp/universe)

;===================================================================
;===================================================================
;||                                                               ||
;||                 JUMP TO BOTTOM OF FILE                        ||
;||              FOR THE INTERACTION FUNCTION                     ||
;||                                                               ||
;===================================================================
;===================================================================
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

(define row (for/fold ([acc (rectangle 0 0 "solid" "transparent")])
          ([i (in-range 0 100)])
  (beside acc ec)))

(define grid
  (overlay
           (for/fold ([acc (rectangle 0 0 "solid" "transparent")])
          ([i (in-range 0 100)])
  (above acc row))
           (rectangle 1000 1000 "solid" "sky blue"))) ;<----- SET BACKGROUND COLOR HERE

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
  ;;RULESET (IF NEIGHBOR SUM == N THEN SET TO 1 OR 0)
  (cond
    [(= sum 0) 0]
    [(= sum 1) 0]
    [(= sum 2) 1]
    [(= sum 3) (match (vector-ref vec (toind x y))
                [0 1]
                [1 0])]
    [else 0]))
   

;UNIVERSE ANIMATION METHODS
;===================================================================
;;INITIAL STATE
(define init arr)
(vector-set! arr (toind 48 48) 1)
(vector-set! arr (toind 48 49) 1)
(vector-set! arr (toind 49 48) 1)
(vector-set! arr (toind 49 49) 1)

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
        [1 (place-image (rectangle 10 10 "solid" (match (neighbor-sum input i j)
                                                   [1 "white smoke"]
                                                   [2 "gainsboro"]
                                                   [3 "light gray"]
                                                   [4 "silver"]
                                                   [5 "gray"]
                                                   [6 "dark gray"]
                                                   [7 "dim gray"]
                                                   [_ "black"])

                                                   ) (cc i) (cc j) acc)]))
        )


;MAIN FUNCTION
;===================================================================
;;RS :: BOOL -> ANIMATION
(define (rs random?) ;(run-sim)
  (match random?
    [#t (for* ([i (in-range 0 100)]
         [j (in-range 0 100)])
     (vector-set! init (toind i j) (floor (/ (random 8) 7))))] ;;<----- CHANCES OF GENERATING A PIXEL ON INITIALIZATION
    [#f void])
  
  (big-bang init
    (on-tick tickh)
    (to-draw draw)))