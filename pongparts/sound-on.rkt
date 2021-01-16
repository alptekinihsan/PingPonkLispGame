#lang racket

(provide play-pong-sound)

(require rsound)

(define BOOP-DURATION-MAC-PC 3000)

(define BOOP-DURATION-LINUX 12000)
(define BOOP-DURATION BOOP-DURATION-MAC-PC)

(define (play-pong-sound what)
  (begin
    (cond

      [(string=? what "pad") (play (make-tone 360 10 BOOP-DURATION))]
      [(string=? what "duvar") (play (make-tone 260 10 BOOP-DURATION))]
      [(string=? what "kaçırma") (play (make-tone 160 10 20000))]
      [else true]
    )
    true))
