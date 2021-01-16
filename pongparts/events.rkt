#lang racket

(provide handle-tick
         handle-key-down
         handle-key-up
         handle-mouse)

(require 2htdp/universe
  htdp/testing
  "dbgmsg.rkt"
  "constants.rkt"
  "structs.rkt"
  "moving.rkt")

(define blank-world (create-initial-world "sol oyuncu başlıyor." 
                      (make-ball (make-position CENTER-HORZ CENTER-VERT)
                        (make-direction 0.5 0)
                        INITIAL-SPEED)))

(define (handle-tick world)
  (if (string=? (pong-world-status world) "in-play")
    (check-paddle-block
      (let
        ([original-ball-dy (direction-dy (ball-dir (pong-world-ball world)))]
        [new-ball (vertical-ball-bounce (move-ball (pong-world-ball world)))])
        (pong-world-set-sound
         (make-pong-world 
          (pong-world-status world)
          new-ball
          (move-paddle (pong-world-left-paddle world))
          (move-paddle (pong-world-right-paddle world))
          (pong-world-left-score world)
          (pong-world-right-score world))
         (if (eq? original-ball-dy (direction-dy (ball-dir new-ball))) "none" "wall"))))

    (make-pong-world 
      (pong-world-status world)
      (pong-world-ball world)
      (move-paddle (pong-world-left-paddle world))
      (move-paddle (pong-world-right-paddle world))
      (pong-world-left-score world)
      (pong-world-right-score world))))


(check-expect (handle-key-down blank-world "w")  (set-left-moving blank-world UP-DIR PADDLE-SPEED))
(check-expect (handle-key-down blank-world "s")  (set-left-moving blank-world DOWN-DIR PADDLE-SPEED))
(check-expect (handle-key-down blank-world "up")  (set-right-moving blank-world UP-DIR PADDLE-SPEED))
(check-expect (handle-key-down blank-world "down")  (set-right-moving blank-world DOWN-DIR PADDLE-SPEED))

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
 

(check-expect (handle-key-up blank-world "w")  (set-left-moving blank-world UP-DIR 0))
(check-expect (handle-key-up blank-world "s")  (set-left-moving blank-world UP-DIR 0))
(check-expect (handle-key-up blank-world "up")  (set-right-moving blank-world UP-DIR 0))
(check-expect (handle-key-up blank-world "down")  (set-right-moving blank-world UP-DIR 0))

(define (handle-key-up world a-key)
  (cond
    [(or (key=? a-key "w") (key=? a-key "s"))
     
      (set-left-moving world UP-DIR 0)]
    [(or (key=? a-key "up") (key=? a-key "down"))
    
      (set-right-moving world UP-DIR 0)]
    
    [(key=? a-key "escape") 
      (pong-world-set-status world "quitting")]
    [(key=? a-key " ")
      (begin
        (dbgmsg (string-append "Başlama alanı: " 
                  (pong-world-status world) "\n"))
        (cond

          [(string=? (pong-world-status world) "in-play") world]

          [(string=? (pong-world-status world) "sol oyuncu başlıyor.")
            (serve-ball world 0.5 INITIAL-SPEED)]
          [(string=? (pong-world-status world) "sağ oyuncu başlıyor.")
            (serve-ball world -0.5 INITIAL-SPEED)]
          [else (pong-world-set-status blank-world "sol oyuncu başlıyor.")]))]
    [else 
      (begin
        (dbgmsg "in handle-key-up default")
        world)]))


(check-expect (handle-mouse blank-world 100 100 "drag") (serve-ball blank-world 0.5 INITIAL-SPEED))

(define (handle-mouse world x y mouseevent)
  (if (or (string=? mouseevent "drag") (string=? mouseevent "button-down"))
     (cond 
        [(or (eq? (pong-world-status world) "sol oyuncu kazandı.")
             (eq? (pong-world-status world) "sağ oyuncu kazandı."))
           blank-world]
        [(and (eq? (pong-world-status world) "sol oyuncu başlıyor.")
              (< x CENTER-HORZ))
           (serve-ball world 0.5 INITIAL-SPEED)] 
        [(and (eq? (pong-world-status world) "sağ oyuncu başlıyor.")
              (> x CENTER-HORZ))
           (serve-ball world -0.5 INITIAL-SPEED)]
        [else (if (> x CENTER-HORZ)
       
          (pong-world-set-right-paddle world (make-paddle (make-position RIGHT (min y (- BOTTOM PADDLE-HEIGHT 2))) 
                                     (make-direction 0 1) 
                                     0))

          (pong-world-set-left-paddle world (make-paddle (make-position MARGIN (min y (- BOTTOM PADDLE-HEIGHT 2))) 
                                     (make-direction 0 1) 
                                     0)))])      
   world))
