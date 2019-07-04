#lang racket/base
(require racket/math
         racket/class
         (only-in racket/draw the-color-database)
         racket/list
         2htdp/image
         lux
         lux/chaos/gui
         lux/chaos/gui/key
         mode-lambda
         mode-lambda/static
         mode-lambda/backend/gl)

(define WIDTH 800)
(define HEIGHT 600)
(define BORDER 0.05)

(define ACTOR-SIZE (* WIDTH BORDER 0.5))
(define BULLET-SIZE (* ACTOR-SIZE 0.5))
(define DELTA (* BULLET-SIZE 0.5 0.5))
(define IDELTA (* DELTA 0.5))

(define sdb (make-sprite-db))
(define-syntax-rule (define-sprite id val)
  (begin
    (add-sprite!/value sdb 'id val)
    (define id 'id)))

(define BACKGROUND '())
(define-sprite PLAYER
  (let* ([thruster (triangle (* 0.50 ACTOR-SIZE) "solid" #;"orange" "MidnightBlue")]
         [cockpit
          (ellipse (* 0.50 ACTOR-SIZE) (* 0.75 ACTOR-SIZE) "solid" "DeepSkyBlue")]
         [ship
          (triangle (* 1.50 ACTOR-SIZE) "solid" "gray")]
         [p (overlay cockpit ship)]
         [p (overlay/align/offset  "left" "bottom" thruster (* -0.15 ACTOR-SIZE) 0.0 p)]
         [p (overlay/align/offset "right" "bottom" thruster (* +0.15 ACTOR-SIZE) 0.0 p)])
    p))
(define-sprite PBULLET
  (overlay/align "middle" "bottom"
                 (ellipse (* 0.25 BULLET-SIZE) BULLET-SIZE "solid" "red")
                 (triangle BULLET-SIZE "solid" "orange")))
(define-sprite INVADER
  (overlay/align
   "middle" "bottom"
   (ellipse (* 2.0 ACTOR-SIZE) (* 0.5 ACTOR-SIZE) "solid" "gray")
   (overlay
    (triangle (* 0.75 ACTOR-SIZE) "solid" "LightGray")
    (circle (* 0.5 ACTOR-SIZE) "solid" "green"))))
(define-sprite IBULLET
  (ellipse (* 0.25 BULLET-SIZE) BULLET-SIZE "solid" "LimeGreen"))

(define ROWS 5)
(define COLS 9)
(define (x->col x)
  (* 1.0 (round (sub1 (/ x (* WIDTH BORDER 2))))))
(define (col->x col)
  (* WIDTH BORDER 2.0 (add1 col)))
(define (y->row x)
  (* 1.0 (round (sub1 (/ x (* HEIGHT BORDER 2))))))
(define (row->y row)
  (* HEIGHT BORDER 2.0 (add1 row)))

(define cdb (compile-sprite-db sdb))
(save-csd! cdb "csd" #:debug? #t)
(define ml-layers
  (make-vector 8 (layer (* WIDTH 0.5) (* HEIGHT 0.5))))
(define ml-draw
  (stage-draw/dc cdb WIDTH HEIGHT (vector-length ml-layers)))
(define BG (send the-color-database  find-color "MidnightBlue"))
(define BG-R (send BG red))
(define BG-G (send BG green))
(define BG-B (send BG blue))

(define (word-result v)
  (word #:return v #:tick (λ () #f)))

(struct posn (x y))
(struct st (player pbullets invaders ibullets ks)
  #:methods gen:word
  [(define (word-label w ft)
     (lux-standard-label "Spacey Defenders" ft))
   (define (word-event w e)
     (cond
       [(eq? 'close e)
        (word-result "You quit! :(")]
       [(key-event? e)
        (key-state-update! (st-ks w) e)
        w]
       [else
        w]))
   (define (word-output w)
     (ml-draw ml-layers '() (draw-screen w)
              #:r BG-R #:g BG-G #:b BG-B))
   (define (word-tick w)
     (move-invaders&bullets (handle-key w)))])

(define initial-invaders
  (for*/list ([row (in-range ROWS)]
              [col (in-range COLS)])
    (posn (col->x col) (row->y row))))
(define initial-st
  (st (posn (* WIDTH 0.5) (* HEIGHT (- 1.0 BORDER))) '()
      initial-invaders '()
      (make-key-state)))

(define (posn+ p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))
(define (move-player b dx)
  (struct-copy st b [player (posn+ (st-player b) dx 0)]))
(define (handle-key b)
  (define ks (st-ks b))
  (cond
    [(key-state-set?! ks 'left)  (move-player b (* -3 DELTA))]
    [(key-state-set?! ks 'right) (move-player b (* +3 DELTA))]
    [(key-state-set?! ks #\space)
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
       (posn+ p (* +1.0 IDELTA) 0.0))]
    [else
     (if (= col 0)
       (posn (col->x col) (row->y (add1 row)))
       (posn+ p (* -1.0 IDELTA) 0.0))]))
(define (maybe-shoot p)
  (cond
    [(<= (random) 0.01)
     (list (posn+ p 0.0 (* +1.0 ACTOR-SIZE)))]
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
     (word-result "You win!")]
    [(hits? player targets)
     (word-result "You lost!")]
    [else
     (struct-copy st b
                  [pbullets left-pbullets]
                  [invaders left-invaders]
                  [ibullets left-ibullets])]))

(define (draw-one sc img p)
  (cons sc (sprite (posn-x p) (posn-y p) (sprite-idx cdb img))))
(define (draw-some sc img l)
  (for/fold ([sc sc]) ([e (in-list l)])
    (draw-one sc img e)))
(define (draw-screen b)
  (let* ([sc BACKGROUND]
         [sc (draw-one sc PLAYER (st-player b))]
         [sc (draw-some sc PBULLET (st-pbullets b))]
         [sc (draw-some sc INVADER (st-invaders b))]
         [sc (draw-some sc IBULLET (st-ibullets b))])
    sc))

(module+ main
  (call-with-chaos
   (make-gui #:width WIDTH
             #:height HEIGHT
             #:mode gui-mode)
   (λ ()
     (fiat-lux initial-st))))
