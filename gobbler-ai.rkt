#lang htdp/isl+

(require "provide.rkt")

;; ===================================================================================================
(define SIZE 600)

;; ===================================================================================================
;; The Gobble Server communicates with its clients according to the following protocol: 

(define WAITING 'waiting)
(define COUNTDOWN 'countdown)
(define PLAYING 'playing)
(define GAME-OVER 'game-over)

;; A Server2ClientMessage is one of:
#;
(list WAITING
      N  ;; the number of players waiting and
      N) ;; the number of players needed to play
#;
(list COUNTDOWN
      [Listof TurkeyMessage] ;; the other turkeys that are about to play the game
      [Listof FoodMessage]   ;; the locations of the initial foods
      N                      ;; the amount of time left before the game begins
      [Maybe TurkeyMessage]) ;; the receiving player's turkey, or false if observing
#;
(list PLAYING
      [Listof TurkeyMessage]  ;; other turkeys currently playing
      [Listof FoodMessage]    ;; the locations of the foods on the board
      N                       ;; the amount of time left before the game ends
      [Maybe TurkeyMessage])  ;; the receiving player's turkey, or #false if observing
#;
(list GAME-OVER
      [Listof String])        ;; a description of how the game ended (winners)

;; A TurkeyMessage is a
#;
(list
 String ;; name of turkey
 N      ;; x coordinate 
 N      ;; y coordinate 
 N)     ;; number of food items eaten 

;; A FoodMessage is a
#;
(list N  ;; x coordinate 
      N) ;; y coordinate 

;; ===================================================================================================
;; A gobble client communicates with the Gobble Server according to the following protocol: 

(define WAYPOINT 'waypoint)
;; A Client2ServerMessage is a
#;
(list WAYPOINT
      N  ;; the x coordinate
      N) ;; the y coordinate 

;; ===================================================================================================
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/abstraction)

;; ---------------------------------------------------------------------------------------------------
(define MF "blue")
(define SF "orange")
(define CF "red")
(define OTHER-COLOR "yellow")
(define FOOD-COLOR "green")
(define TEXT-COLOR "black")

(define TEXT-SIZE 22) 

(define FOOD (square 40 'solid FOOD-COLOR))

(define BACKGROUND (empty-scene SIZE SIZE))

(define GOBBLER-SERVER "dictionary.ccs.neu.edu")
(define GOBBLER-PORT   20000)

;; ---------------------------------------------------------------------------------------------------
;; State = (list TurkeyMessage Server2ClientMessage)
;; INTERPRETATION Here the TurkeyMessage specifies the current waypoint that the client has chosen. 

;; IP String ColorString -> State 
(define (main where handle color)
  ;; this initial state temporarily breaks types "for free"
  (big-bang (list (list handle 0 0 0) "dummy message")
            (to-draw    (render color))
            (on-receive incoming)
            (stop-when  game-over? (render-over color))
            (name       handle)
            (register   where)
            (port       GOBBLER-PORT)))

;; ---------------------------------------------------------------------------------------------------
;; State -> Boolean
;; did the player receive a 'game over' message?

(check-expect (game-over? `(stuff (,GAME-OVER "you won"))) #true)
(check-expect (game-over? `(stuff old)) #false)

(define (game-over? s)
  (match s
    [`(,my-properties (,(? (is? GAME-OVER)) ,x ...)) #true]
    [else #false]))

;; ---------------------------------------------------------------------------------------------------
;; State Server2ClientMessage -> StateOrPackage
;; an incoming message is supplemented with the chosen color (from the old state)

(check-expect (incoming '(("red" 0 0 0) old) 'new) '(("red" 0 0 0) new)) ;; break type discipline

(define (incoming s msg)
  (match s
    [`(,my-properties ,last-msg) (pick-new-waypoint my-properties msg)]))

;; ColorString State -> StateOrPackage
(define (pick-new-waypoint my-properties msg)
  (match msg
    [`(,(? (is? PLAYING))   ,others ,foods ,n ,me) (pick me others foods my-properties msg)]
    [else (list my-properties msg)]))

;; [Maybe Turkey] [Listof Turkey] [Listof Food] (list N N) N Server2ClientMessage -> StateOrPackage

(check-expect (pick (list "chris" 0 0 0) '() (list (list 3 4) (list 12 5))  (list "chris" 0 0 0) 'msg)
              (make-package (list (list "chris" 3 4 0) 'msg) (list WAYPOINT 3 4)))

(define (pick me others food my-properties msg)
  (if (boolean? me)
      (list my-properties msg)
      (local ((define nxt (pick-closest me others food)))
        (match my-properties
          [`(,name ,x ,y ,size)
           (if (equal? (list x y) nxt)
               (list my-properties msg)
               (local ((define new-x (first nxt))
                       (define new-y (second nxt))
                       (define new-s (list name new-x new-y size)))
                 (make-package (list new-s msg) (list WAYPOINT new-x new-y))))]))))

;; Turkey [Listof Turkey] [Listof Food] -> (list x y)
;; choose the closest piece of food as the next 
(define (pick-closest me others foods)
  (match me
    [`(,name ,x ,y ,size) 
     (local ((define distances
               (map (lambda (o) (match o [`(,x1 ,y1) (list (distance x y x1 y1) x1 y1)])) foods)))
       (rest (argmin first distances)))]))

(check-expect (distance 0 0 12 5) 13)
(check-expect (distance 3 4 15 9) 13)
(define (distance x y x1 y1)
  (sqrt
   (+ (sqr (- x x1)) (sqr (- y y1)))))

  
;; ---------------------------------------------------------------------------------------------------
;; ColorString -> [State -> Image]
;; render the state of the game 

(define (render my-color)
  (lambda (s)
    (match s
      [`(,me ,last-message)
       (match last-message
         [`(,(? (is? WAITING))   ,players ,min-players) (render-waiting players min-players)]
         [`(,(? (is? COUNTDOWN)) ,others ,foods ,n ,me) (render-helper others foods n me my-color)]
         [`(,(? (is? PLAYING))   ,others ,foods ,n ,me) (render-helper others foods n me my-color)]
         [`(,(? (is? GAME-OVER)) s)                     (render-over s)]
         [_                                             BACKGROUND])])))

;; N N -> Image 
(define (render-waiting p# min)
  (local ((define p#-str  (number->string p#))
          (define min-str (number->string min))
          (define msg1 (string-append p#-str " are waiting, "))
          (define msg2 (string-append min-str " are needed"))
          (define txt (above (text msg1 22 TEXT-COLOR) (text msg2 22 TEXT-COLOR))))
    (place-image txt 22 44 BACKGROUND)))

;; ColorString -> [State -> Image]
(define (render-over my-color)
  (lambda (s)
    (match s 
      [`(,me (,(? (is? GAME-OVER)) ,s))
       (local ((define txts
                 (foldr (λ (n img) (above (text n TEXT-SIZE TEXT-COLOR) img)) empty-image s)))
         (place-image txts (/ SIZE 2) (/ SIZE 2) BACKGROUND))])))

;; [Listof TurkeyMessage] [Listof FoodMessage] N [Maybe TurkeyMessage] ColorString -> Image
(define (render-helper others food n me my-color)
  (add-me me my-color (add-time n (add-food* food (add-turkey* others BACKGROUND)))))

;; [Listof TurkeyMessage] Image -> Image 
(define (add-turkey* turkeys img)
  (foldr (λ (t img) (place-turkey t OTHER-COLOR img)) img turkeys))

;; [Listof FoodMessage] Image -> Image 
(define (add-food* foods img)
  (foldr place-food img foods))

;; N Image -> Image 
(define (add-time n img)
  (place-image (text (number->string n) TEXT-SIZE TEXT-COLOR) 22 44 img))

;; [Maybe TurkeyMessage] ColorString Image -> Image 
(define (add-me turkey-or-false color img)
  (cond
    [(boolean? turkey-or-false) img]
    [else (place-turkey turkey-or-false color img)]))

;; TurkeyMessage ColorString Image -> Image 
(define (place-turkey t color img)
  (match t
    [`(,name ,x ,y ,size) (place-image (turkey-with-name size name color) x y img)]))

;; N String ColorString -> Image 
(define (turkey-with-name size name color)
  (overlay (text name 22 TEXT-COLOR) (circle (+ 30 size) 'solid color)))

;; TurkeyMessage ColorString Image -> Image 
(define (place-food t img)
  (match t
    [`(,x ,y) (place-image FOOD x y img)]))

;; ---------------------------------------------------------------------------------------------------
;; auxiliaries

;; Symbol -> [Any -> Boolean]
;; generate a predicate to check whether the given value is the symbol s

(check-expect ((is? WAITING) WAITING) #true)
(check-expect ((is? WAITING) (random 1)) #false)

(define (is? s)
  (lambda (x)
    (and (symbol? x) (symbol=? x s))))

;; ---------------------------------------------------------------------------------------------------
;; launching the server locally 

(require (prefix-in server: "gobbler-server.rkt"))

(define (start with-server?)
  (if with-server?
      (launch-many-worlds (main LOCALHOST "christopher" CF)
                          (main LOCALHOST "matthias"    MF)
                          (server:main GOBBLER-PORT 30))
      (launch-many-worlds (main GOBBLER-SERVER "christopher" CF)
                          (main GOBBLER-SERVER "matthias"    MF)
                          (main GOBBLER-SERVER "matthias"    SF))))
