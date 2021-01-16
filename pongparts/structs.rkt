#lang racket

(provide 
 (struct-out position) 
 (struct-out direction)
 UP-DIR
 DOWN-DIR
 (struct-out ball)
 (struct-out paddle)
 (struct-out key-msg)
 (struct-out pong-world)
 pong-world-set-status
 pong-world-set-ball
 pong-world-set-left-paddle
 pong-world-set-right-paddle
 pong-world-set-left-score
 pong-world-set-right-score
 pong-world-set-sound
 create-initial-world)

(require "constants.rkt")

(struct position [x y]
  #:prefab
  #:constructor-name make-position)


(struct direction [dx dy]
  #:prefab
  #:constructor-name make-direction)


(define UP-DIR (make-direction 0 -1))


(define DOWN-DIR (make-direction 0 1))


(struct ball [pos dir speed] 
  #:prefab
  #:constructor-name make-ball)

(struct paddle [pos dir speed]
  #:prefab
  #:constructor-name make-paddle)


(struct key-msg
  [which-player press-or-release key]
  #:prefab
  #:constructor-name make-key-msg) 


(struct pong-world 
  [status ball left-paddle right-paddle left-score right-score [sound #:auto] ]
  #:prefab
  #:mutable
  #:auto-value "none"
  #:constructor-name make-pong-world)


(define (mutate-and-return setter instance field-val)
  (begin
    (apply setter (list instance field-val))
    instance))

(define (pong-world-set-status world status)
   (mutate-and-return set-pong-world-status! world status)) 
(define (pong-world-set-ball world ball)
   (mutate-and-return set-pong-world-ball! world ball)) 
(define (pong-world-set-left-paddle world left-paddle)
   (mutate-and-return set-pong-world-left-paddle! world left-paddle))
(define (pong-world-set-right-paddle world right-paddle)
   (mutate-and-return set-pong-world-right-paddle! world right-paddle))
(define (pong-world-set-left-score world left-score)
   (mutate-and-return set-pong-world-left-score! world left-score))
(define (pong-world-set-right-score world right-score)
   (mutate-and-return set-pong-world-right-score! world right-score))
(define (pong-world-set-sound world sound-name)
   (mutate-and-return set-pong-world-sound! world sound-name))



(define (create-initial-world initial-status initial-ball) (make-pong-world
                       initial-status
                       initial-ball
                       (make-paddle (make-position MARGIN (- CENTER-VERT (/ PADDLE-HEIGHT 2)))
                                    (make-direction 0 1)
                                    0)
                       (make-paddle (make-position RIGHT (- CENTER-VERT (/ PADDLE-HEIGHT 2)))
                                    (make-direction 0 1)
                                    0)
                       0
                       0))
