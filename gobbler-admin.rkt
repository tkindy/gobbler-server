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
(define FOOD-IMG (square 5 'solid 'green))

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
    [`(waiting ,num-players ,_)
     (render-waiting num-players)]
    [`(,(or 'countdown 'playing) ,players ,foods ,ticks ,_)
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
  (let* ([+foods (render-foods foods BACKGROUND)]
         [+players (render-players players +foods)]
         [+timer   (render-timer ticks +players)])
    +timer))

;; [Listof FoodMessage] Image -> Image
;; Draw the foods on the image
(define (render-foods foods img)
  (foldr (λ (f i)
           (place-image FOOD-IMG
                        (first f)
                        (second f)
                        i))
         img
         foods))

;; [Listof PlayerMessage] Image -> Image
;; Draw the players on the image
(define (render-players players img)
  ;; N -> Image
  ;; Draw the player having eaten n foods
  (define (player-img n)
    (circle (+ 5 n) 'solid 'yellow))
  ;; PlayerMessage Image -> Image
  ;; Draw the player on the image
  (define (render-player player img)
    (let* ([x (second player)]
           [y (third player)]
           [pi (player-img (fourth player))]
           [+pi (place-image pi x y img)]
           [name (text (first player) 16 'black)]
           [+name (place-image name x y +pi)])
      +name))
     
  (foldr render-player img players))

;; N Image -> Image
;; Draw the timer on the image
(define (render-timer ticks img)
  (let* ([timer (text (number->string ticks) 16 'black)]
         [+timer (place-image timer 25 25 img)])
    +timer))

;; AdminState sexp? -> AdminState
(define (receive-msg state sexp)
  sexp)