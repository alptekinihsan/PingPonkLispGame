#lang racket

(provide
  move-coord
  move-ball
  move-paddle
  set-paddle-pos
  set-paddle-moving
  set-left-moving
  set-right-moving
  vertical-ball-bounce
  check-paddle-block
  serve-ball)

(require 2htdp/universe
  htdp/testing
  "dbgmsg.rkt"
  "constants.rkt"
  "structs.rkt")

(check-expect (move-coord 0 0 0) 0)
(check-expect (move-coord 100 1 10) 110)
(check-expect (move-coord 100 -1 10) 90)

(define (move-coord current dir speed)
  (inexact->exact (round (+ current (* dir speed)))))


(check-expect (move-ball (make-ball (make-position 100 100) (make-direction -0.5 0.5) 2))
                 (make-ball (make-position 99 101) (make-direction -0.5 0.5) 2))

(define (move-ball ball)
  (make-ball (make-position 
               (move-coord (position-x (ball-pos ball)) (direction-dx (ball-dir ball)) (ball-speed ball))
               (move-coord (position-y (ball-pos ball)) (direction-dy (ball-dir ball)) (ball-speed ball)))
               (ball-dir ball)
               (ball-speed ball)))
      


(check-expect (move-paddle-vert 100 0.5 2) 101)
(check-expect (move-paddle-vert (- BOTTOM PADDLE-HEIGHT 2) 0.75 12) (- BOTTOM PADDLE-HEIGHT 2))
(check-expect (move-paddle-vert (+ TOP 2) -0.75 12) (+ TOP 2))

(define (move-paddle-vert current-y dir speed)

  (min (max (inexact->exact (round (+ current-y (* dir speed)))) (+ TOP 2)) (- BOTTOM PADDLE-HEIGHT 2))) 

(define (move-paddle paddle)
  (make-paddle (make-position 
                 (position-x (paddle-pos paddle))
                 (move-paddle-vert 
                   (position-y (paddle-pos paddle)) 
                   (direction-dy (paddle-dir paddle)) 
                   (paddle-speed paddle)))
               (paddle-dir paddle)
               (paddle-speed paddle)))


(define (vertical-bounce ball x y dx dy speed)

    (if (< dy 0)

       (if (< y TOP)

         (make-ball (make-position x TOP)
                    (make-direction dx (- 0 dy))
                    speed)
         ball)

       (if (> y BOTTOM)

         (make-ball (make-position x BOTTOM)
                    (make-direction dx (- 0 dy))
                    speed)
         ball))) 

(define (vertical-ball-bounce ball)
  (vertical-bounce ball
          (position-x (ball-pos ball))
          (position-y (ball-pos ball))
          (direction-dx (ball-dir ball))
          (direction-dy (ball-dir ball))
          (ball-speed ball)))

(define (score-a-point world side)

    (if (string=? side "left")

      (pong-world-set-sound (make-pong-world    
        
        (if (< (pong-world-left-score world) 8) "sağ oyuncu başlıyor." "sol oyuncu kazandı.")
        (make-ball initial-position (make-direction -0.5 0) INITIAL-SPEED)
        (pong-world-left-paddle world)
        (pong-world-right-paddle world)
        (+ (pong-world-left-score world) 1)
        (pong-world-right-score world)) "missed")
      (pong-world-set-sound (make-pong-world    
        (if (< (pong-world-right-score world) 8) "sol oyuncu başlıyor." "sağ oyuncu kazandı.")
        (make-ball initial-position (make-direction 0.5 0) INITIAL-SPEED)
        (pong-world-left-paddle world)
        (pong-world-right-paddle world)
        (pong-world-left-score world)
        (+ (pong-world-right-score world) 1))
        "missed")))


(define (horizontal-bounce-y world left-paddle-y right-paddle-y x y dx dy speed)

    (if (< dx 0)

       (if (< x LEFT)

         (if (and (> y (- left-paddle-y MARGIN)) (< y (+ left-paddle-y PADDLE-HEIGHT MARGIN)))

           (pong-world-set-sound (pong-world-set-ball world 
                (make-ball (make-position LEFT y)
                  (make-direction (- 0 dx) (vary-dy-by-intersection left-paddle-y y))
                  (vary-speed-by-intersection speed left-paddle-y y))) "paddle")

           (score-a-point world "right"))

         world)

       (if (> x RIGHT)

         (if (and (> y (- right-paddle-y MARGIN)) (< y (+ right-paddle-y PADDLE-HEIGHT MARGIN)))

           (pong-world-set-sound (pong-world-set-ball world 
                (make-ball (make-position RIGHT y)
                  (make-direction (- 0 dx) (vary-dy-by-intersection right-paddle-y y))
                  (vary-speed-by-intersection speed right-paddle-y y))) "paddle")

         (score-a-point world "left"))

       world)))


(define (horizontal-bounce world x y dx dy speed)
  (horizontal-bounce-y world 
                     (position-y (paddle-pos (pong-world-left-paddle world))) 
                     (position-y (paddle-pos (pong-world-right-paddle world))) x y dx dy speed))

(define (vary-dy-by-intersection paddle-y intersect-y)
  (/ (- intersect-y (+ paddle-y (/ PADDLE-HEIGHT 2))) PADDLE-HEIGHT))


(define (vary-speed-by-intersection current-speed paddle-y intersect-y)

  (min MAXIMUM-SPEED 

       (round (+ current-speed 
                 (* (/ (abs (- intersect-y (+ paddle-y (/ PADDLE-HEIGHT 2)))) 
                    (/ PADDLE-HEIGHT 2)) 3)))))

(define (check-paddle-block world)
  (horizontal-bounce world           
          (position-x (ball-pos (pong-world-ball world)))
          (position-y (ball-pos (pong-world-ball world)))
          (direction-dx (ball-dir (pong-world-ball world)))
          (direction-dy (ball-dir (pong-world-ball world)))
          (ball-speed (pong-world-ball world))))


(define (set-paddle-moving paddle dir speed)
  (make-paddle 
    (paddle-pos paddle)
    dir
    speed))

(define (set-paddle-pos paddle pos)
  (make-paddle 
    pos
    (paddle-dir paddle)
    (paddle-speed paddle)))


(define (set-left-moving world dir speed)
  (pong-world-set-left-paddle world (set-paddle-moving (pong-world-left-paddle world) dir speed)))


(define (set-right-moving world dir speed)
  (pong-world-set-right-paddle world (set-paddle-moving (pong-world-right-paddle world) dir speed)))

(define initial-position (make-position CENTER-HORZ CENTER-VERT))

(define (serve-ball world dx speed)
  (pong-world-set-status 
    (pong-world-set-ball world 
      (make-ball initial-position
        (make-direction dx 0)
        speed)) 
    "in-play"))
