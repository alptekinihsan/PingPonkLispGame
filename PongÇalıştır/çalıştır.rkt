#lang racket
;
;
;
;OYUNUN BAŞLAMASI İÇİN SPACE TUŞUNA BASMAMIZ YETERLİDİR.
;
;
;
(require 2htdp/universe
  2htdp/image
  htdp/testing
  "../pongparts/dbgmsg.rkt"
  "../pongparts/constants.rkt"
  "../pongparts/structs.rkt"
  "../pongparts/drawing.rkt"
  "../pongparts/moving.rkt"
  "../pongparts/events.rkt"
  "../pongparts/sound.rkt")


(define (main world)
  (if SHOW-PAD
    (big-bang world
            [name "Pong Dünyası"]
            [on-tick handle-tick]
            [to-draw draw-pong-world]
            [on-pad handle-key-down]
            [on-release handle-key-up]
            [on-mouse handle-mouse]
            [stop-when (lambda (world) (string=? (pong-world-status world) "Çıkış Yapılıyor")) draw-goodbye])
    (big-bang world
            [name "Pong Dünyası"]
            [on-tick handle-tick]
            [to-draw draw-pong-world]
            [on-key handle-key-down]
            [on-release handle-key-up]
            [on-mouse handle-mouse]
            [stop-when (lambda (world) (string=? (pong-world-status world) "Çıkış Yapılıyor")) draw-goodbye])))

(define initial-world (create-initial-world "sol oyuncu başlıyor" 
                      (make-ball (make-position CENTER-HORZ CENTER-VERT)
                        (make-direction 0.5 0)
                        INITIAL-SPEED)))
(main initial-world)