#lang racket

(require 2htdp/universe)

#|

==============
GOBBLER SERVER
==============

The Single Player Game
--------
The Gobbler game is a thanksgiving-themed game where the player controls a
turkey and attempts to eat all on-screen food before any number of NPC turkeys
can get to it before the timer runs out. The player controls their own turkey
with a mouse, clicking on the screen to create 'waypoints' to which their turkey
will travel.


The Multi-player Game
--------
The multiplayer game will have three phases: a waiting phase, a countdown phase,
and a playing phase.

I   Waiting Phase
In this phase, clients are waiting for enough players to join the game for it to
run. Clients will receive a WaitingMessage (defined below) when they join and
then whenever there are any changes.

II  Countdown Phase
In this phase, there have been enough players to run the game. Exactly enough
players waiting to play are selected, and the initial state is sent to clients,
along with how long is left in the countdown. This CountdownMessage is sent on
each clock tick. This phase ends at the end of the countdown.

III Playing Phase
This phase, to a player, will operate much like the single-player game, the only
change being that what was once NPCs are now being controlled by other players.
To clients, the changes are more substantial. On each clock tick, the server
will calculate the next state and send it to the clients. When a client detects
a change in waypoint, it will send this information to the server. As foods are
eaten, new ones will appear. The game ends when the time runs out.
At the end of the game, a GameOverMessage is sent to clients, the clients that
just played are put into the end of the waiting list, and the game returns to
the waiting phase.

If at any point a new client connects, that world is added to the end of a
waiting list.

|#

;; =====================================
;; DATA DEFINITIONS
;; =====================================

;; A Posn is a (posn N N)
;; represents a Cartesian coordinate pair
(struct posn [x y] #:transparent)

;; A GobblerUniverse is one of:
;; - Waiting
;; - Countdown
;; - Playing

;; A GobblerBundle is one of:
;; - GobblerUniverse
;; - (make-bundle GobblerUniverse [Listof Outgoing] '())

;; An Outgoing is a (make-mail iworld? Server2ClientMessage)
;; Represents a message that is to be sent to the iworld

(struct turkey [loc food-eaten waypoint] #:transparent)
(struct player [iworld turkey] #:transparent)
(struct game [queue] #:transparent #:mutable)
(struct waiting game [] #:transparent)
(struct ready game [players foods time-left] #:transparent)
(struct countdown ready [] #:transparent)
(struct playing ready [] #:transparent)

;; A Turkey is a (turkey posn? N posn?)
;; - represents a playable turkey

;; A Player is a (make-player iworld? turkey?)
;; - represents currently-playing player information

;; A Game is a (game [Listof iworld?])
;; represents the base state of the game
;; - queue: the players currently waiting to play

;; A Waiting is a (waiting [Listof iworld?])
;; represents the phase where not enough players have joined

;; A Ready is a (ready [Listof iworld?]
;;                     [Listof player?]
;;                     [Listof posn?]
;;                     N)
;; represents a world that is ready to play
;; - players: the players currently playing the game
;; - foods: the locations of the foods
;; - time-left: the time left in this phase, in ticks

;; A Countdown is a (countdown [Listof iworld?]
;;                             [Listof player?]
;;                             [Listof posn?]
;;                             N)
;; represents the phase just before starting a new game

;; A Playing is a (make-playing [Listof iworld?]
;;                              [Listof player?]
;;                              [Listof posn?]
;;                              N)
;; represents the phase where the game is being played


;; =====================================
;; PROTOCOLS
;; =====================================

;; A Server2ClientMessage is one of:
;; - WaitingMessage
;; - CountdownMessage
;; - PlayingMessage
;; - GameOverMessage
;; Represents a message that is sent from the server to the clients

(define WAITING 'waiting)
;; A WaitingMessage is a (list 'waiting N N)
;; Represents information in the waiting phase where:
;; - the first number represents the number of players waiting and
;; - the second number represents the number of players needed to play

(define COUNTDOWN 'countdown)
;; A CountdownMessage is a (list 'countdown
;;                               [Listof TurkeyMessage]
;;                               [Listof FoodMessage]
;;                               N
;;                               [Maybe TurkeyMessage]
;; Represents information on a pre-game countdown where:
;; - the [Listof TurkeyMessage] is the other turkeys that are about to play
;;   the game
;; - the [Listof FoodMessage] is the locations of the foods on the board
;; - the N is the amount of time left before the game begins
;; - the [Maybe TurkeyMessage] is the receiving player's turkey, or false if
;;   they are observing

(define PLAYING 'playing)
;; A PlayingMessage is a (list 'playing
;;                             [Listof TurkeyMessage]
;;                             [Listof FoodMessage]
;;                             N
;;                             [Maybe TurkeyMessage]
;; Represents information on an in-progress game where:
;; - the [Listof TurkeyMessage] is other turkeys currently playing
;; - the [Listof FoodMessage] is the locations of the foods on the board
;; - the N is the amount of time left before the game ends
;; - the [Maybe TurkeyMessage] is the receiving player's turkey, or false if
;;   they are observing

(define GAME-OVER 'game-over)
;; A GameOverMessage is a (list 'game-over OutcomeMessage)
;; Represents information on the outcome of the game

;; An OutcomeMessage is a string?
;; Represents the name of the winner of the game

;; A TurkeyMessage is a (list N N N)
;; Represents the location of a turkey where:
;; - the first number is its x coordinate
;; - the second number is its y coordinate
;; - the third number is the number of foods it has eaten

;; A FoodMessage is a (list N N)
;; Represents the location of a food where:
;; - the first number is its x coordinate
;; - the second number is its y coordinate

;; A Client2ServerMessage is a WaypointMessage
;; Represents a message that is sent from the client to the server

;; A WaypointMessage is a (list 'waypoint N N)
;; Represents a message from the client to update their waypoint such that:
;; - the first number is the x coordinate
;; - the second number is the y coordinate

;; N -> N
;; Convert seconds to game ticks
(define (seconds->ticks s)
  (* s TICKS-PER-SECOND))

;; =====================================
;; CONSTANTS
;; =====================================

(define TKY-STEP 10)
(define TICKS-PER-SECOND 28)
(define COUNTDOWN-TICKS (seconds->ticks 3))
(define GAME-TICKS (seconds->ticks 60))

(define INITIAL-STATE (waiting '()))


;; =====================================
;; SERVER
;; =====================================

;; number? -> GobblerUniverse
;; Run the server
(define (main the-port)
  (universe INITIAL-STATE
            [port the-port]
            [on-new queue-world!]
            [on-disconnect drop-world]
            [on-tick advance-game (/ TICKS-PER-SECOND)]
            [on-msg update-waypoint]))

;; GobblerUniverse iworld? -> GobblerUniverse
;; Queue the new player
(define (queue-world! uni world)
  (define new-queue (cons world (game-queue uni)))
  (set-game-queue! uni new-queue)
  uni)

;; GobblerUniverse iworld? -> GobblerBundle
;; Remove player from the game
(define (drop-world uni world)
  uni)

;; GobblerUniverse -> GobblerBundle
;; Advance the game state
(define (advance-game uni)
  uni)

;; GobblerUniverse iworld? sexp? -> GobblerBundle
;; Update the waypoint of the player
(define (update-waypoint uni world sexp)
  uni)


;; =====================================
;; UTILS
;; =====================================

;; two helper functions that rely on domain knowledge from geometry

;; REVISED SIGNATURE
;; Posn Posn Number -> Posn
;; compute a Posn that is by delta closer to q than p
;; unless p is alreay delta-close to q

(define (move-toward origin destination delta)
  (cond
    [(close? origin destination delta) destination]
    [else
     (posn+ origin (posn* (/ delta (size (posn- destination origin)))
                          (posn- destination origin)))]))

;; Posn Posn Number -> Boolean
;; is the distance between p and q strictly less than delta (delta-close)
(define (close? p q delta)
  (< (distance p q) delta))

;; A Vec is Posn.
;; INTEREPRATION When we call a Posn a Vec, we think of it 'directionally'
;; as in x over on the horizontal and y over on the vertical axis from here

;; Posn Posn -> Number
;; compute the distance between p and q
(define (distance p q)
  (size (posn- p q)))

;; Vec -> Number
;; determine the size (length) of p
(define (size p)
  (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))

;; Number Vec -> Vec
;; multiply s componentwise with v
(define (posn* s v)
  (posn (* s (posn-x v)) (* s (posn-y v))))

;; Posn Posn -> Vec
;; subtract q from p componentwise to obtain a vector
(define (posn- p q)
  (posn (- (posn-x p) (posn-x q)) (- (posn-y p) (posn-y q))))

;; Posn Vec -> Posn
;; add q to p componentwise
(define (posn+ p q)
  (posn (+ (posn-x p) (posn-x q)) (+ (posn-y p) (posn-y q))))


;; =====================================
;; TESTS
;; =====================================

(module+ test
  (require rackunit)

  ;; =====================================
  ;; TESTING DATA & UTILS
  ;; =====================================

  (define POSN0      null)
  (define POSN1      null)
  (define TURKEY0    null)
  (define TURKEY1    null)
  (define TURKEY2    null)
  (define TURKEY1.1  null)
  (define PLAYER1    null)
  (define PLAYER2    null)
  (define PLAYER3    null)
  (define PLAYER1.1  null)
  (define WAITING0   null)
  (define WAITING1   null)
  (define WAITING2   null)
  (define COUNTDOWN0 null)
  (define COUNTDOWN1 null)
  (define COUNTDOWN2 null)
  (define COUNTDOWN3 null)
  (define COUNTDOWN4 null)
  (define PLAYING0   null)
  (define PLAYING1   null)
  (define PLAYING1.1 null)
  (define PLAYING2   null)
  (define PLAYING3   null)
  (define PLAYING4   null)

  ;; Initialize all the test data
  (define (fixture)
    (set! POSN0 (posn 40 40))
    (set! POSN1 (posn 100 30))

    (set! TURKEY0 (turkey (posn 10 10)  1 (posn 50 30)))
    (set! TURKEY1 (turkey (posn 45 50)  0 (posn 45 50)))
    (set! TURKEY2 (turkey (posn 30 100) 0 (posn 50 50)))

    (set! TURKEY1.1 (turkey (posn 10 10) 1 (posn 30 20)))

    (set! PLAYER1 (player iworld1 TURKEY0))
    (set! PLAYER2 (player iworld2 TURKEY1))
    (set! PLAYER3 (player iworld3 TURKEY2))

    (set! PLAYER1.1 (player iworld1 TURKEY1.1))

    (set! WAITING0   (waiting '()))
    (set! WAITING1   (waiting `(,iworld1)))
    (set! WAITING2   (waiting `(,iworld1 ,iworld2)))
    (set! COUNTDOWN0 (countdown '() `(,PLAYER1 ,PLAYER2)
                                `(,POSN0 ,POSN1) COUNTDOWN-TICKS))
    (set! COUNTDOWN1 (countdown `(,iworld3) `(,PLAYER1 ,PLAYER2)
                                `(,POSN0 ,POSN1) COUNTDOWN-TICKS))
    (set! COUNTDOWN2 (countdown `(,iworld3) `(,PLAYER2) `(,POSN0 ,POSN1)
                                COUNTDOWN-TICKS))
    (set! COUNTDOWN3 (countdown '() `(,PLAYER1 ,PLAYER2)
                                `(,POSN0 ,POSN1) (- COUNTDOWN-TICKS 1)))
    (set! COUNTDOWN4 (countdown '() `(,PLAYER1 ,PLAYER2)
                                `(,POSN0 ,POSN1) 1))
    (set! PLAYING0   (playing '() `(,PLAYER1 ,PLAYER2) `(,POSN0 ,POSN1)
                              GAME-TICKS))
    (set! PLAYING1   (playing `(,iworld3) `(,PLAYER1 ,PLAYER2)
                              `(,POSN0 ,POSN1) GAME-TICKS))
    (set! PLAYING1.1 (playing `(,iworld3) `(,PLAYER1.1 ,PLAYER2)
                              `(,POSN0 ,POSN1) GAME-TICKS))
    (set! PLAYING2   (playing `(,iworld3) `(,PLAYER2) `(,POSN0 ,POSN1)
                              GAME-TICKS))
    (set! PLAYING3   (playing '() `(,PLAYER1 ,PLAYER2) `(,POSN0 ,POSN1)
                              (- GAME-TICKS 1)))
    (set! PLAYING4   (playing '() `(,PLAYER1 ,PLAYER2) `(,POSN0 ,POSN1)
                              1)))

  ;; [A] [Listof (-> A)] -> String
  ;; Run all the given tests
  (define (run-tests tests)
    (for ([test tests])
      (run-test test))
    "all tests run")

  ;; [A] (-> A) -> A
  ;; Initialize the test data and run the test
  (define (run-test test-f)
    (fixture)
    (test-f))

  (define tests
    (list

     ;; =====================================
     ;; SERVER TESTS
     ;; =====================================
     (λ ()
       (check-equal? (queue-world! WAITING0 iworld1) WAITING1)
       (check-equal? (queue-world! COUNTDOWN0 iworld3) COUNTDOWN1)
       (check-equal? (queue-world! PLAYING0 iworld3) PLAYING1))

     (λ ()
       (check-equal? (drop-world WAITING1 iworld1) WAITING0)
       (check-equal? (drop-world COUNTDOWN1 iworld3) COUNTDOWN0)
       (check-equal? (drop-world COUNTDOWN1 iworld1) COUNTDOWN2)
       (check-equal? (drop-world PLAYING1 iworld3) PLAYING0)
       (check-equal? (drop-world PLAYING1 iworld1) PLAYING2))

     (λ ()
       (check-equal? (advance-game WAITING0) WAITING0)
       (check-equal? (advance-game WAITING1) WAITING1)
       (check-equal? (advance-game COUNTDOWN0) COUNTDOWN3)
       (check-equal? (advance-game COUNTDOWN4) PLAYING0)
       (check-equal? (advance-game PLAYING0) PLAYING3)
       (check-equal? (advance-game PLAYING4) WAITING2))

     (λ ()
       (check-equal? (update-waypoint PLAYING1 iworld1 '(here is some garbage))
                     PLAYING1)
       (check-equal? (update-waypoint PLAYING1 iworld1 2)
                     PLAYING1)
       (check-equal? (update-waypoint PLAYING1 iworld3 '(waypoint 30 20))
                     PLAYING1)
       (check-equal? (update-waypoint PLAYING1 iworld1 '(waypoint 30 20))
                     PLAYING1.1))

     ;; =====================================
     ;; UTILS TESTS
     ;; =====================================
     (λ ()
       (check-true (close? (move-toward (posn 12 5) (posn 24 10) 13)
                           (posn 24 10)
                           .1))
       (check-true (close? (move-toward (posn 12 5) (posn 24 10) 6.5)
                           (posn 18 7.5)
                           .1))
       (check-true (close? (move-toward (posn 12 5) (posn 24 10) 14)
                           (posn 24 10)
                           .1))
       (check-true (close? (move-toward (posn 12 5) (posn 24 10) 13)
                           (posn 24 10)
                           .1))
       (check-true (close? (move-toward (posn 12 5) (posn 24 10) 6.5)
                           (posn 18 7.5)
                           .1))
       (check-true (close? (move-toward (posn 12 5) (posn 24 10) 14)
                           (posn 24 10)
                           .1)))

     (λ ()
       (check-true (close? (posn 10 10) (posn 10 9) 2.0))
       (check-true (close? (posn 10 10) (posn 10 9) 0.8)))

     (λ ()
       (check-equal? (distance (posn 3 4) (posn 0 0)) 5))

     (λ ()
       (check-equal? (size (posn 12 5)) 13))

     (λ ()
       (check-equal? (posn* 2 (posn 1 3)) (posn 2 6)))

     (λ ()
       (check-equal? (posn- (posn 3 2) (posn 3 8)) (posn 0 -6)))

     (λ ()
       (check-equal? (posn+ (posn 3 2) (posn 3 8)) (posn 6 10)))))

  (run-tests tests))
