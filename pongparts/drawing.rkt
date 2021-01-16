#lang racket

(provide 
 display-msg
 draw-idle-game
 draw-pong-world
 draw-pong-client
 draw-goodbye)

(require 2htdp/universe
  2htdp/image
  "dbgmsg.rkt"
  "constants.rkt"
  "structs.rkt"
  "sound.rkt")


(define (display-msg msg font-size x y scene)
  (place-image (text/font msg font-size "blue"
             "impact" 'system 'normal 'light #f)
                    x y
                    scene))

(define (place-image-top-left image x y scene)
   (place-image image
      (+ x (round (/  (image-width image) 2)))
      (+ y (round (/ (image-height image) 2)))
      scene))

(define (draw-bg w h c)
  (place-image-top-left
  (rectangle w h "solid" c)
  0 0
  (empty-scene w h)))

(define (vertical-dashed-line w h x ys ye c bg)
  (if (< ys ye)
    (place-image-top-left
      (rectangle w h "solid" c)
      x ys
      (vertical-dashed-line w h x (+ ys (* 2 h)) ye c bg))
    bg))

(define (draw-net w h bg)
    (vertical-dashed-line BALL-SIZE BALL-SIZE (- (/ w 2) (/ BALL-SIZE 2)) 0 h "green" bg))

(define (draw-wall w)
  (rectangle w WALL-HEIGHT "solid" "green"))

(define (draw-playfield-bg w h)
  (overlay/align "left" "top"
    (draw-net WIDTH HEIGHT
      (overlay/align "right" "top"
        (draw-wall w)
        (overlay/align "right" "bottom"
          (draw-wall w)
          (draw-bg WIDTH HEIGHT "blue"))))
    (draw-bg SCREEN-WIDTH SCREEN-HEIGHT "gray")))


(define PLAYFIELD-BG (freeze (draw-playfield-bg WIDTH HEIGHT)))

(define (draw-ball size) (rectangle size size "solid" "red"))

(define BALL (freeze (draw-ball BALL-SIZE)))

(define PADDLE (freeze  (rectangle PADDLE-THICKNESS PADDLE-HEIGHT "solid" "black")))

(define (draw-paddle paddle background)
  (underlay/xy
    background
    (position-x (paddle-pos paddle)) 
    (position-y (paddle-pos paddle)) 
    PADDLE))
  
(define (draw-idle-game world)
  (draw-paddle (pong-world-left-paddle world)
    (draw-paddle (pong-world-right-paddle world)
      (place-image
        (text (number->string (pong-world-left-score world)) 98 "white")
        (- CENTER-HORZ 60) 80
        (place-image
          (text (number->string (pong-world-right-score world)) 98 "white")
          (+ CENTER-HORZ 60) 80
          PLAYFIELD-BG)))))
        
(define (draw-goodbye world)
   (display-msg "Goodbye!!" 48
      (- CENTER-HORZ 200) CENTER-VERT
      (draw-idle-game world)))

(define (draw-pong-client client-type world)
  (begin 
    (dbgmsg (string-append client-type " drawing " (pong-world-status world) "\n"))
    (play-pong-sound (pong-world-sound world))
    (cond
      [(string=? (pong-world-status world) "connecting")
       (display-msg "Waiting for connection" 36 
                    (- CENTER-HORZ 200) CENTER-VERT
                    (draw-idle-game world))]
      [(string=? (pong-world-status world) "in-play") 
       (place-image BALL 
                    (position-x (ball-pos (pong-world-ball world))) 
                    (position-y (ball-pos (pong-world-ball world)))
                    (draw-idle-game world))]
      [(string=? (pong-world-status world) "sol-oyuncu-serves")
       (display-msg "Hit space to serve" 36 
                    (- CENTER-HORZ 200) CENTER-VERT
                    (draw-idle-game world))]
      [(string=? (pong-world-status world) "sag-oyuncu-serves")
       (display-msg "Hit space to serve" 36
                    (+ CENTER-HORZ 200) CENTER-VERT
                    (draw-idle-game world))]
      [(string=? (pong-world-status world) "sol-oyuncu-won")
       (display-msg "You won!!!" 48
                    (- CENTER-HORZ 200) CENTER-VERT
                    (draw-idle-game world))]
      [(string=? (pong-world-status world) "sag-oyuncu-won")
       (display-msg "You won!!!" 48
                    (+ CENTER-HORZ 200) CENTER-VERT
                    (draw-idle-game world))]
      [else 
        (begin
          (dbgmsg (string-append "beraberlik: \"" (pong-world-status world) "\"\n"))
          (draw-idle-game world))])))


(define (draw-pong-world world)
     (draw-pong-client "" world))
