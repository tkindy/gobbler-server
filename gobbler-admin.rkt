#lang racket

(require 2htdp/image)
(require 2htdp/universe)

(struct state [msg winners] #:transparent)
;; An AdminState is a (state Server2ClientMessage [Listof string?])
;; - msg: the last message received from the server
;; - winners: the winners of the last game

;; An AdminMessage is one of:
;; - '(size N)
;; - 'go

;; A HandlerState is one of:
;; - AdminState
;; - (make-package AdminState AdminMessage)

(define ADMIN-CLIENT "admin")
(define PORT 20000)

(define FOOD-SIZE 10)
(define PLAYER-BASE-SIZE 10)

(define SIZE 0)
(define BACKGROUND null)
(define FOOD-IMG (square FOOD-SIZE 'solid 'green))

;; IPAddr N -> AdminState
;; Run the admin client
(define (main host size)
  (set! SIZE size)
  (set! BACKGROUND (empty-scene SIZE SIZE))
  
  (big-bang (state '() '())
    [on-key     (build-key-handler size)]
    [to-draw    render]
    [on-receive receive-msg]
    [register   host]
    [port       PORT]
    [name       ADMIN-CLIENT]))

;; N -> (AdminState KeyEvent -> HandlerResult)
;; Build the key handler
(define (build-key-handler size)
  (λ (s key)
    (match key
      [" " (printf "starting game\n")
           (make-package s 'go)]
      ["s" (printf "updating size to ~a~n" size)
           (make-package s `(size ,size))]
      ["d" (printf "dropping leftover worlds\n")
           (make-package s 'drop)]
      [_ s])))

;; AdminState -> Image
;; Draw the world
(define (render s)
  (match (state-msg s)
    [`(waiting ,num-players ,_)
     (render-waiting num-players (state-winners s))]
    [`(,(or 'countdown 'playing) ,players ,foods ,ticks ,_)
     (render-ready players foods ticks)]
    [_ BACKGROUND]))

;; N [Listof string?] -> Image
;; Render the waiting state
(define (render-waiting num-players winners)
  (define winners-str
    (if (cons? winners)
        (foldr (λ (n s)
                 (string-append s "\n" n))
               (first winners)
               (rest winners))
        ""))
  (place-image (text (format "~a players waiting~n~a" num-players winners-str)
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
    (circle (+ PLAYER-BASE-SIZE n) 'solid 'yellow))
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
(define (receive-msg s sexp)
  (define new-winners
    (match sexp
      [`(game-over ,winners) winners]
      [_ (state-winners s)]))
  
  (state sexp new-winners))