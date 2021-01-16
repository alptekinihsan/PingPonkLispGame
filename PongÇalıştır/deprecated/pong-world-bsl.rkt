;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pong-world-bsl) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)
(require "common/pong-sound.rkt")

(define SHOW-DEBUG-MSGS false)
(define screen-scale 1.5)
(define SCREEN-WIDTH (* 830 screen-scale))
(define SCREEN-HEIGHT (* 440 screen-scale))
(define SHOW-PAD true)
(define PAD-HEIGHT (round (* SCREEN-HEIGHT 0.2)))
(define WIDTH SCREEN-WIDTH)
(define HEIGHT (if SHOW-PAD (- SCREEN-HEIGHT PAD-HEIGHT) SCREEN-HEIGHT))

(define CENTER-HORZ (round (/ WIDTH 2)))
(define CENTER-VERT (round (/ HEIGHT 2)))
(define MARGIN 10)
(define WALL-HEIGHT 16)
(define PADDLE-HEIGHT 90)
(define PADDLE-THICKNESS 16)
(define PADDLE-SPEED 15)
(define BALL-SIZE 16)
(define INITIAL-SPEED 28)
(define MAXIMUM-SPEED 68)
(define TOP WALL-HEIGHT)
(define BOTTOM (- HEIGHT WALL-HEIGHT))
(define LEFT (+ PADDLE-THICKNESS MARGIN))
(define RIGHT (- WIDTH (+ PADDLE-THICKNESS MARGIN)))
(define-struct direction [dx dy])
(define UP-DIR (make-direction 0 -1))
(define DOWN-DIR (make-direction 0 1))
(define-struct ball [pos dir speed])
(define-struct paddle [pos dir speed])
(define-struct pong-world [status ball left-paddle right-paddle left-score right-score])

(define (main world)
  (if SHOW-PAD
    (big-bang world
            [name "Pong Dünyası"]
            [on-tick handle-tick]
            [to-draw draw-pong-world]
            [on-pad handle-key-down]
            [on-release handle-key-up]
            [on-mouse handle-mouse]
            [stop-when quitting? draw-goodbye])
    (big-bang world
            [name "Pong Dünyası"]
            [on-tick handle-tick]
            [to-draw draw-pong-world]
            [on-key handle-key-down]
            [on-release handle-key-up]
            [on-mouse handle-mouse]
            [stop-when quitting? draw-goodbye])))
(check-expect (move-coord 0 0 0) 0)
(check-expect (move-coord 100 1 10) 110)
(check-expect (move-coord 100 -1 10) 90)

(define (move-coord current dir speed)
  (round (+ current (* dir speed))))
(check-expect (move-ball (make-ball (make-posn 100 100) (make-direction -0.5 0.5) 2))
                 (make-ball (make-posn 99 101) (make-direction -0.5 0.5) 2))

(define (move-ball ball)
  (make-ball (make-posn 
               (move-coord (posn-x (ball-pos ball)) (direction-dx (ball-dir ball)) (ball-speed ball))
               (move-coord (posn-y (ball-pos ball)) (direction-dy (ball-dir ball)) (ball-speed ball)))
               (ball-dir ball)
               (ball-speed ball)))
      

(check-expect (move-paddle-vert 100 0.5 2) 101)
(check-expect (move-paddle-vert (- BOTTOM PADDLE-HEIGHT 2) 0.75 12) (- BOTTOM PADDLE-HEIGHT 2))
(check-expect (move-paddle-vert (+ TOP 2) -0.75 12) (+ TOP 2))

(define (move-paddle-vert current-y dir speed)
  (min (max (round (+ current-y (* dir speed))) (+ TOP 2)) (- BOTTOM PADDLE-HEIGHT 2))) 
(define (move-paddle paddle)
  (make-paddle (make-posn 
                 (posn-x (paddle-pos paddle))
                 (move-paddle-vert 
                   (posn-y (paddle-pos paddle)) 
                   (direction-dy (paddle-dir paddle)) 
                   (paddle-speed paddle)))
               (paddle-dir paddle)
               (paddle-speed paddle)))

(define (vertical-bounce ball x y dx dy speed)

    (if (< dy 0)

       (if (< y TOP)
         (if (play-sound "wall") (make-ball (make-posn x TOP)
                    (make-direction dx (- 0 dy))
                    speed) ball)
         ball)
       (if (> y BOTTOM)
         (if (play-sound "wall") (make-ball (make-posn x BOTTOM)
                    (make-direction dx (- 0 dy))
                    speed) ball)
         ball))) 
(define (vertical-ball-bounce ball)
  (vertical-bounce ball
          (posn-x (ball-pos ball))
          (posn-y (ball-pos ball))
          (direction-dx (ball-dir ball))
          (direction-dy (ball-dir ball))
          (ball-speed ball)))
(define (pong-world-set-ball world ball)
  (make-pong-world
     (pong-world-status world)
     ball
     (pong-world-left-paddle world)
     (pong-world-right-paddle world)
     (pong-world-left-score world)
     (pong-world-right-score world)))
(define (pong-world-set-status world status)
  (make-pong-world
     status
     (pong-world-ball world)
     (pong-world-left-paddle world)
     (pong-world-right-paddle world)
     (pong-world-left-score world)
     (pong-world-right-score world)))

(define (score-a-point world side)
  (if (play-sound "missed")
    (if (eq? side "left")
      (make-pong-world    
        (if (< (pong-world-left-score world) 8) "sağ oyuncu başlıyor" "sol oyuncu kazandı !")
        (serve-ball -0.5)
        (pong-world-left-paddle world)
        (pong-world-right-paddle world)
        (+ (pong-world-left-score world) 1)
        (pong-world-right-score world))
      (make-pong-world    
        (if (< (pong-world-right-score world) 8) "sol oyuncu başlıyor" "sağ oyuncu kazandı !")
        (serve-ball 0.5)
        (pong-world-left-paddle world)
        (pong-world-right-paddle world)
        (pong-world-left-score world)
        (+ (pong-world-right-score world) 1)))
    world))


(define (horizontal-bounce-y world left-paddle-y right-paddle-y x y dx dy speed)
    (if (< dx 0)

       (if (< x LEFT)

         (if (and (> y (- left-paddle-y MARGIN)) (< y (+ left-paddle-y PADDLE-HEIGHT MARGIN)))

           (if (play-sound "çubuk") (pong-world-set-ball world 
                (make-ball (make-posn LEFT y)
                  (make-direction (- 0 dx) (vary-dy-by-intersection left-paddle-y y))
                  (vary-speed-by-intersection speed left-paddle-y y))) world)
 
           (score-a-point world "sağ"))

         world)

       (if (> x RIGHT)
   
         (if (and (> y (- right-paddle-y MARGIN)) (< y (+ right-paddle-y PADDLE-HEIGHT MARGIN)))

           (if (play-sound "paddle") (pong-world-set-ball world 
                (make-ball (make-posn RIGHT y)
                  (make-direction (- 0 dx) (vary-dy-by-intersection right-paddle-y y))
                  (vary-speed-by-intersection speed right-paddle-y y))) world)

         (score-a-point world "left"))

       world)))
    

(define (horizontal-bounce world x y dx dy speed)
  (horizontal-bounce-y world 
                     (posn-y (paddle-pos (pong-world-left-paddle world))) 
                     (posn-y (paddle-pos (pong-world-right-paddle world))) x y dx dy speed))

(define (vary-dy-by-intersection paddle-y intersect-y)
  (/ (- intersect-y (+ paddle-y (/ PADDLE-HEIGHT 2))) PADDLE-HEIGHT))

(define (vary-speed-by-intersection current-speed paddle-y intersect-y)
  
  (min MAXIMUM-SPEED 
       (round (+ current-speed 
                 (* (/ (abs (- intersect-y (+ paddle-y (/ PADDLE-HEIGHT 2)))) 
                    (/ PADDLE-HEIGHT 2)) 3)))))

(define (check-paddle-block world)
  (horizontal-bounce world           
          (posn-x (ball-pos (pong-world-ball world)))
          (posn-y (ball-pos (pong-world-ball world)))
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

(define (set-left-paddle world left-paddle)
  (make-pong-world
    (pong-world-status world)
    (pong-world-ball world)
    left-paddle 
    (pong-world-right-paddle world) 
    (pong-world-left-score world)
    (pong-world-right-score world)))

(define (set-right-paddle world right-paddle)
  (make-pong-world
    (pong-world-status world)
    (pong-world-ball world)
    (pong-world-left-paddle world) 
    right-paddle 
    (pong-world-left-score world)
    (pong-world-right-score world)))

(define (set-left-moving world dir speed)
  (set-left-paddle world (set-paddle-moving (pong-world-left-paddle world) dir speed)))

(define (set-right-moving world dir speed)
  (set-right-paddle world (set-paddle-moving (pong-world-right-paddle world) dir speed)))

(define initial-posn (make-posn CENTER-HORZ CENTER-VERT))

(define (serve-ball initial-dx)
  (make-ball initial-posn
     (make-direction initial-dx 0)
     INITIAL-SPEED))
                                
(define initial-world (make-pong-world 
                        "sol oyuncu başlıyor"
                        (serve-ball 0.5)
                        (make-paddle (make-posn MARGIN (- CENTER-VERT (/ PADDLE-HEIGHT 2))) 
                                     (make-direction 0 1) 
                                     0) 
                        (make-paddle (make-posn RIGHT (- CENTER-VERT (/ PADDLE-HEIGHT 2))) 
                                     (make-direction 0 1) 
                                     0)
                        0
                        0))

(check-expect (quitting? (pong-world-set-status initial-world "çıkılıyor")) true)
(check-expect (quitting? (pong-world-set-status initial-world "oynanıyor")) false)

(define (quitting? world)
  (eq? (pong-world-status world) "çıkılıyor"))

(define (draw-goodbye world)
   (display-msg "Güle Güle !!" 48
      (- CENTER-HORZ 200) CENTER-VERT
      (draw-idle-game world)))

(define (draw-bg w h c)
  (empty-scene w h c))

(define (draw-net w h bg)
    (scene+line bg 
       (/ w 2) 0 (/ w 2) h (make-pen "white" 14 "dot" "butt" "bevel")))

(define (draw-wall w)
  (rectangle w WALL-HEIGHT "solid" "white"))

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

(define (draw-ball size) (rectangle size size "solid" "white"))

(define BALL (freeze (draw-ball BALL-SIZE)))

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

(define (display-msg msg font-size x y scene)
  (place-image (text/font msg font-size "white"
             "impact" 'system 'normal 'light #f)
                    x y
                    scene))
  
(define (draw-pong-world world)
  (if (dbgmsg (string-append "çiziliyor " (number->string (random 10)) "\n"))
  (cond 
    [(eq? (pong-world-status world) "oynanıyor") 
       (place-image BALL 
                    (posn-x (ball-pos (pong-world-ball world))) 
                    (posn-y (ball-pos (pong-world-ball world)))
                    (draw-idle-game world))]
    [(eq? (pong-world-status world) "sol oyuncu başlıyor")
       (display-msg "Sayı atışı" 36 
                    (- CENTER-HORZ 200) CENTER-VERT
                    (draw-idle-game world))]
    [(eq? (pong-world-status world) "sağ oyuncu başlıyor")
       (display-msg "Sayı atışı" 36
                    (+ CENTER-HORZ 200) CENTER-VERT
                    (draw-idle-game world))]
    [(eq? (pong-world-status world) "sol oyuncu kazandı !")
       (display-msg "Sen kazandın !!!" 48
                    (- CENTER-HORZ 200) CENTER-VERT
                    (draw-idle-game world))]
    [(eq? (pong-world-status world) "sağ oyuncu kazandı !")
       (display-msg "Sen kazandın !!!" 48
                    (+ CENTER-HORZ 200) CENTER-VERT
                    (draw-idle-game world))])
PLAYFIELD-BG))

(define (handle-tick world)
  (if (eq? (pong-world-status world) "oynanıyor")
    (check-paddle-block 
      (make-pong-world 
        (pong-world-status world)
        (vertical-ball-bounce (move-ball (pong-world-ball world)))
        (move-paddle (pong-world-left-paddle world))
        (move-paddle (pong-world-right-paddle world))
        (pong-world-left-score world)
        (pong-world-right-score world)))
    (make-pong-world 
      (pong-world-status world)
      (pong-world-ball world)
      (move-paddle (pong-world-left-paddle world))
      (move-paddle (pong-world-right-paddle world))
      (pong-world-left-score world)
      (pong-world-right-score world))))
    
(define (draw-paddle paddle background)
  (underlay/xy
    background
    (posn-x (paddle-pos paddle)) 
    (posn-y (paddle-pos paddle)) 
    PADDLE))
  
(define PADDLE (freeze  (rectangle PADDLE-THICKNESS PADDLE-HEIGHT "solid" "white")))

(define (serve world)
  (make-pong-world 
    "oynanıyor"
    (pong-world-ball world)
    (pong-world-left-paddle world)
    (pong-world-right-paddle world)
    (pong-world-left-score world)
    (pong-world-right-score world))) 

(check-expect (handle-key-down initial-world "w")  (set-left-moving initial-world UP-DIR PADDLE-SPEED))
(check-expect (handle-key-down initial-world "s")  (set-left-moving initial-world DOWN-DIR PADDLE-SPEED))
(check-expect (handle-key-down initial-world "up")  (set-right-moving initial-world UP-DIR PADDLE-SPEED))
(check-expect (handle-key-down initial-world "down")  (set-right-moving initial-world DOWN-DIR PADDLE-SPEED))

(define (handle-key-down world a-key)
  (cond
    [(key=? a-key "w") 
       (set-left-moving world UP-DIR PADDLE-SPEED)]
    [(key=? a-key "s")
       (set-left-moving world DOWN-DIR PADDLE-SPEED)]
    [(key=? a-key "up") 
       (set-right-moving world UP-DIR PADDLE-SPEED)]
    [(key=? a-key "down") 
       (set-right-moving world DOWN-DIR PADDLE-SPEED)]
    [else world]))

(check-expect (handle-key-up initial-world "w")  (set-left-moving initial-world UP-DIR 0))
(check-expect (handle-key-up initial-world "s")  (set-left-moving initial-world UP-DIR 0))
(check-expect (handle-key-up initial-world "up")  (set-right-moving initial-world UP-DIR 0))
(check-expect (handle-key-up initial-world "down")  (set-right-moving initial-world UP-DIR 0))

(define (handle-key-up world a-key)
  (cond
    [(or (key=? a-key "w") (key=? a-key "s"))
      
      (set-left-moving world UP-DIR 0)]
    [(or (key=? a-key "up") (key=? a-key "down"))
      
      (set-right-moving world UP-DIR 0)]
    
    [(key=? a-key "escape") 
      (pong-world-set-status world "çıkılıyor")]
    [(key=? a-key " ")
        (cond
          [(eq? (pong-world-status world) "oynanıyor") world]
          [(or (eq? (pong-world-status world) "sol oyuncu başlıyor")
               (eq? (pong-world-status world) "sağ oyuncu başlıyor"))
            (serve world)]
          [else initial-world])]
    [else world]))
(check-expect (handle-mouse initial-world 100 100 "drag") (serve initial-world))

(define (handle-mouse world x y mouseevent)
  (if (or (string=? mouseevent "drag") (string=? mouseevent "tuş çekme"))
     (cond 
        [(or (eq? (pong-world-status world) "sol oyuncu kazandı !")
             (eq? (pong-world-status world) "sağ oyuncu kazandı !"))
           initial-world]
        [(and (eq? (pong-world-status world) "sol oyuncu başlıyor")
              (< x CENTER-HORZ))
           (serve world)] 
        [(and (eq? (pong-world-status world) "sağ oyuncu başlıyor")
              (> x CENTER-HORZ))
           (serve world)]
        [else (if (> x CENTER-HORZ)

          (set-right-paddle world (make-paddle (make-posn RIGHT (min y (- BOTTOM PADDLE-HEIGHT 2))) 
                                     (make-direction 0 1) 
                                     0))

          (set-left-paddle world (make-paddle (make-posn MARGIN (min y (- BOTTOM PADDLE-HEIGHT 2))) 
                                     (make-direction 0 1) 
                                     0)))])      
   world))

(define (dbgmsg msg) 
  (if SHOW-DEBUG-MSGS
    (or (string? (write-file 'stdout msg)) true)
    true))
