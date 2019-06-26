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
(define DELTA (* BULLET-SIZE 0.5 0.5))
(define IDELTA (* DELTA 1/2))

(define BACKGROUND
  (empty-scene WIDTH HEIGHT "MidnightBlue"))
(define PLAYER
  (let* ([thruster (triangle (* 0.50 ACTOR-SIZE) "solid" #;"orange" "MidnightBlue")]
         [cockpit
          (ellipse (* 0.50 ACTOR-SIZE) (* 0.75 ACTOR-SIZE) "solid" "DeepSkyBlue")]
         [ship
          (triangle (* 1.50 ACTOR-SIZE) "solid" "gray")]
         [p (overlay cockpit ship)]
         [p (overlay/align/offset  "left" "bottom" thruster (* -0.15 ACTOR-SIZE) 0 p)]
         [p (overlay/align/offset "right" "bottom" thruster (* +0.15 ACTOR-SIZE) 0 p)])
    p))
(define PBULLET
  (overlay/align "middle" "bottom"
                 (ellipse (* 0.25 BULLET-SIZE) BULLET-SIZE "solid" "red")
                 (triangle BULLET-SIZE "solid" "orange")))
(define INVADER
  (overlay/align
   "middle" "bottom"
   (ellipse (* 2.0 ACTOR-SIZE) (* 0.5 ACTOR-SIZE) "solid" "gray")
   (overlay
    (triangle (* 0.75 ACTOR-SIZE) "solid" "LightGray")
    (circle (* 0.5 ACTOR-SIZE) "solid" "green"))))
(define IBULLET
  (ellipse (* 0.25 BULLET-SIZE) BULLET-SIZE "solid" "LimeGreen"))

(define ROWS 5)
(define COLS 9)
(define (x->col x)
  (round (sub1 (/ x (* WIDTH BORDER 2)))))
(define (col->x col)
  (* WIDTH BORDER 2 (add1 col)))
(define (y->row x)
  (round (sub1 (/ x (* HEIGHT BORDER 2)))))
(define (row->y row)
  (* HEIGHT BORDER 2 (add1 row)))

(struct posn (x y))
(struct st (player pbullets invaders ibullets))
(define initial-invaders
  (for*/list ([row (in-range ROWS)]
              [col (in-range COLS)])
    (posn (col->x col) (row->y row))))
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
    [(key=? ke "left")  (move-player b (* -3 DELTA))]
    [(key=? ke "right") (move-player b (* +3 DELTA))]
    [(key=? ke " ")
     (define pbullets (st-pbullets b))
     (if ((length pbullets) . < . 3)
       (struct-copy st b
                    [pbullets
                     (cons (posn+ (st-player b) 0 (* -1 ACTOR-SIZE))
                           pbullets)])
       b)]
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
(define (move-invader p)
  (define row (y->row (posn-y p)))
  (define col (x->col (posn-x p)))
  (cond
    [(even? row)
     (if (= col (sub1 COLS))
       (posn (col->x col) (row->y (add1 row)))
       (posn+ p (* +1 IDELTA) 0))]
    [else
     (if (= col 0)
       (posn (col->x col) (row->y (add1 row)))
       (posn+ p (* -1 IDELTA) 0))]))
(define (maybe-shoot p)
  (cond
    [(<= (random) 0.01)
     (list (posn+ p 0 (* +1 ACTOR-SIZE)))]
    [else
     empty]))

(define (remove? ? l)
  (cond
    [(empty? l) (values #f l)]
    [(? (first l)) (values #t (rest l))]
    [else
     (define-values (removed? remaining) (remove? ? (rest l)))
     (values removed? (cons (first l) remaining))]))
(define (find-hit-pbullets pbs targets)
  (cond
    [(empty? pbs) (values empty targets)]
    [else
     (define-values (removed? remaining)
       (remove? (hit? (first pbs)) targets))
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
(define LAST (current-inexact-milliseconds))
(define (draw-screen b)
  (let* ([sc BACKGROUND]
         [sc (draw-one sc PLAYER (st-player b))]
         [sc (draw-some sc PBULLET (st-pbullets b))]
         [sc (draw-some sc INVADER (st-invaders b))]
         [sc (draw-some sc IBULLET (st-ibullets b))])
    (define NOW (current-inexact-milliseconds))
    (begin0 (draw-one sc (text
                          (format "FPS: ~a" (real->decimal-string (/ 1000 (- NOW LAST))))
                          ACTOR-SIZE "white")
                      (posn (* 0.90 WIDTH) (* 0.95 HEIGHT)))
      (set! LAST NOW))))

(module+ main
  (big-bang initial-st
            (on-tick move-invaders&bullets 1/60)
            (on-key handle-key)
            (on-draw draw-screen)))
