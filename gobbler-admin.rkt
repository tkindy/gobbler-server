#lang racket

(require 2htdp/image)
(require 2htdp/universe)

;; An AdminState is a Server2ClientMessage

;; An AdminMessage is one of:
;; - '(size N)
;; - 'go

;; A HandlerState is one of:
;; - AdminState
;; - (make-package AdminState AdminMessage)

(define ADMIN-CLIENT "admin")
(define PORT 20000)

(define SIZE 0)
(define BACKGROUND null)

;; IPAddr N -> AdminState
;; Run the admin client
(define (main host size)
  (set! SIZE size)
  (set! BACKGROUND (empty-scene SIZE SIZE))
  
  (big-bang '()
    [on-key     (build-key-handler size)]
    [to-draw    render]
    [on-receive receive-msg]
    [register   host]
    [port       PORT]
    [name       ADMIN-CLIENT]))

;; N -> (AdminState KeyEvent -> HandlerResult)
;; Build the key handler
(define (build-key-handler size)
  (λ (state key)
    (match key
      [" " (make-package state 'go)]
      ["s" (make-package state `(size ,size))]
      [_ state])))

;; AdminState -> Image
;; Draw the world
(define (render state)
  (match state
    [`(waiting ,num-players _)
     (render-waiting num-players)]
    [`(,(or 'countdown 'playing) ,players ,foods ,ticks _)
     (render-ready players foods ticks)]
    [_ BACKGROUND]))

;; N -> Image
;; Render the waiting state
(define (render-waiting num-players)
  (place-image (text (format "~a players waiting" num-players)
                     24
                     'black)
               (/ SIZE 2)
               (/ SIZE 2)
               BACKGROUND))

;; [Listof PlayerMessage] [Listof FoodMessage] N -> Image
;; Render a ready state
(define (render-ready players foods ticks)
  BACKGROUND)

;; AdminState sexp? -> AdminState
(define (receive-msg state sexp)
  sexp)