#lang racket/base
(require racket/math
         racket/list
         2htdp/image
         2htdp/universe)

(define WIDTH 800)
(define HEIGHT 600)
(define BORDER 0.05)

(define ACTOR-SIZE (* WIDTH BORDER 0.5))
(define BULLET-SIZE (* ACTOR-SIZE 0.5))
(define DELTA (* BULLET-SIZE 0.5))

(define PLAYER
  (square ACTOR-SIZE "solid" "black"))
(define PBULLET
  (triangle BULLET-SIZE "solid" "black"))
(define INVADER
  (circle ACTOR-SIZE "solid" "red"))
(define IBULLET
  (star BULLET-SIZE "solid" "red"))

(struct posn (x y))
(struct st (player pbullets invaders ibullets))
(define initial-invaders
  (for*/list ([row (in-range 5)]
              [col (in-range 9)])
    (posn (* WIDTH BORDER 2 (add1 col))
          (* HEIGHT BORDER 2 (add1 row)))))
(define initial-st
  (st (posn (/ WIDTH 2) (* HEIGHT (- 1 BORDER))) '()
      initial-invaders '()))

(define (posn+ p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))
(define (move-player b dx)
  (struct-copy st b [player (posn+ (st-player b) dx 0)]))
(define (handle-key b ke)
  (cond
    [(key=? ke "left")  (move-player b (* -1 DELTA))]
    [(key=? ke "right") (move-player b (* +1 DELTA))]
    [(key=? ke " ")
     (struct-copy st b
                  [pbullets
                   (cons (posn+ (st-player b) 0 (* -1 ACTOR-SIZE))
                         (st-pbullets b))])]
    [else b]))

(define ((hit? p1) p2)
  (<= (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
               (sqr (- (posn-y p1) (posn-y p2)))))
      ACTOR-SIZE))
(define (hits? p ps)
  (ormap (hit? p) ps))
(define ((move-bullet yv) p)
  (posn+ p 0 (* yv DELTA)))
(define (on-screen? p)
  (and (<= 0 (posn-x p) WIDTH)
       (<= 0 (posn-y p) HEIGHT)))
;; XXX fix this
(define (move-invader p)
  (posn+ p
         (* DELTA 0.5 (- (random) 0.5))
         (* DELTA 0.2 (random))))
(define (maybe-shoot p)
  (cond
    [(<= (random) 0.002)
     (list (posn+ p 0 (* +1 ACTOR-SIZE)))]
    [else
     empty]))

(define (remove1 ? l)
  (cond
    [(empty? l) (values #f l)]
    [(? (first l)) (values #t (rest l))]
    [else
     (define-values (removed? remaining) (remove1 ? (rest l)))
     (values removed? (cons (first l) remaining))]))
(define (find-hit-pbullets pbs targets)
  (cond
    [(empty? pbs) (values empty targets)]
    [else
     (define-values (removed? remaining)
       (remove1 (hit? (first pbs)) targets))
     (define-values (rest-pbs rest-targets)
       (find-hit-pbullets (rest pbs) remaining))
     (if removed?
       (values rest-pbs rest-targets)
       (values (cons (first pbs) rest-pbs) rest-targets))]))

(define (move-invaders&bullets b)
  (define player (st-player b))
  (define moved-pbullets
    (filter on-screen? (map (move-bullet -2) (st-pbullets b))))
  (define moved-invaders
    ;; XXX partition and re-show
    (filter on-screen? (map move-invader (st-invaders b))))
  (define-values (left-pbullets0 left-invaders)
    (find-hit-pbullets moved-pbullets moved-invaders))
  (define moved-ibullets
    (filter on-screen? (map (move-bullet +1) (st-ibullets b))))
  (define new-ibullets
    (append-map maybe-shoot left-invaders))
  (define-values (left-pbullets left-ibullets)
    (find-hit-pbullets left-pbullets0 (append moved-ibullets new-ibullets)))
  (define targets (append left-invaders left-ibullets))
  (cond
    [(empty? targets)
     (stop-with "You win!")]
    [(hits? player targets)
     (stop-with "You lost!")]
    [else
     (st player left-pbullets left-invaders left-ibullets)]))

(define (draw-one sc img p)
  (place-image img (posn-x p) (posn-y p) sc))
(define (draw-some sc img l)
  (for/fold ([sc sc]) ([e (in-list l)])
    (draw-one sc img e)))
(define (draw-screen b)
  (let* ([sc (empty-scene WIDTH HEIGHT)]
         [sc (draw-one sc PLAYER (st-player b))]
         [sc (draw-some sc PBULLET (st-pbullets b))]
         [sc (draw-some sc INVADER (st-invaders b))]
         [sc (draw-some sc IBULLET (st-ibullets b))])
    sc))

(module+ main
  (big-bang initial-st
            (on-tick move-invaders&bullets)
            (on-key handle-key)
            (on-draw draw-screen)))
