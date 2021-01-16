#lang racket

(provide dbgmsg)

(define SHOW-DEBUG-MSGS false)

(define (dbgmsg msg) 
  (if SHOW-DEBUG-MSGS
    (display msg)
    true))
