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

;; DATA DEFINITIONS

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

(struct gobbler [loc food-eaten waypoint] #:transparent)
(struct player [iworld gobbler] #:transparent)
(struct game [queue] #:transparent)
(struct waiting game [] #:transparent)
(struct ready game [players foods time-left] #:transparent)
(struct countdown ready [] #:transparent)
(struct playing ready [] #:transparent)

;; A Gobbler is a (gobbler posn? N posn?)
;; - represents a playable turkey

;; A Player is a (make-player iworld? gobbler?)
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

;; PROTOCOLS

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
;;                               [Listof GobblerMessage]
;;                               [Listof FoodMessage]
;;                               N
;;                               [Maybe GobblerMessage]
;; Represents information on a pre-game countdown where:
;; - the [Listof GobblerMessage] is the other gobblers that are about to play
;;   the game
;; - the [Listof FoodMessage] is the locations of the foods on the board
;; - the N is the amount of time left before the game begins
;; - the [Maybe GobblerMessage] is the receiving player's gobbler, or false if
;;   they are observing

(define PLAYING 'playing)
;; A PlayingMessage is a (list 'playing 
;;                             [Listof GobblerMessage]
;;                             [Listof FoodMessage]
;;                             N
;;                             [Maybe GobblerMessage]
;; Represents information on an in-progress game where:
;; - the [Listof GobblerMessage] is other gobblers currently playing
;; - the [Listof FoodMessage] is the locations of the foods on the board
;; - the N is the amount of time left before the game ends
;; - the [Maybe GobblerMessage] is the receiving player's gobbler, or false if
;;   they are observing

(define GAME-OVER 'game-over)
;; A GameOverMessage is a (list 'game-over OutcomeMessage)
;; Represents information on the outcome of the game

;; An OutcomeMessage is a string?
;; Represents the name of the winner of the game

;; A GobblerMessage is a (list N N N)
;; Represents the location of a gobbler where:
;; - the first number is its x coordinate
;; - the second number is its y coordinate
;; - the third number is the number of foods it has eaten

;; A FoodMessage is a (list N N)
;; Represents the location of a gobbler where:
;; - the first number is its x coordinate
;; - the second number is its y coordinate

;; A Client2ServerMessage is a WaypointMessage
;; Represents a message that is sent from the client to the server

;; A WaypointMessage is a (list 'waypoint N N)
;; Represents a message from the client to update their waypoint such that:
;; - the first number is the x coordinate
;; - the second number is the y coordinate

;; Constants
(define TKY-STEP 10)
(define TICKS-PER-SECOND 28)
(define COUNTDOWN-TICKS (* TICKS-PER-SECOND 3))

(define INITIAL-STATE (waiting '()))

;; number? -> ???
;; Run the server
(define (main the-port)
  (universe INITIAL-STATE
            [port the-port]
            [on-new queue-world]
            [on-disconnect drop-world]
            [on-tick advance-game (/ TICKS-PER-SECOND)]
            [on-msg update-waypoint]))

;; GobblerUniverse iworld? -> GobblerBundle
;; Queue the new player
(define (queue-world uni world)
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

;; N -> N
;; Convert seconds to game ticks
(define (seconds->ticks s)
  (* s TICKS-PER-SECOND))

;; ----------------------------------------------------------------------------
;; two helper functions that rely on domain knowledge from geometry

;; REVISED SIGNATURE 
;; Posn Posn Number -> Posn 
;; compute a Posn that is by delta closer to q than p
;; unless p is alreay delta-close to q

(check-within (move-toward (make-posn 12 5) (make-posn 24 10) 13)
              (make-posn 24 10)
              .1)
(check-within (move-toward (make-posn 12 5) (make-posn 24 10) 6.5)
              (make-posn 18 7.5)
              .1)

;; NEW TEST ;; MODIFIED October 2, per request by Antone on behalf of the class
(check-within (move-toward (make-posn 12 5) (make-posn 24 10) 14)
              (make-posn 24 10)
              .1)

(define (move-toward origin destination delta)
  (cond
    [(close? origin destination delta) destination]
    [else
     (posn+ origin (posn* (/ delta (size (posn- destination origin)))
                          (posn- destination origin)))]))

;; Posn Posn Number -> Boolean
;; is the distance between p and q strictly less than delta (delta-close)
(check-expect (close? (make-posn 10 10) (make-posn 10 9) 2.0) #true)
(check-expect (close? (make-posn 10 10) (make-posn 10 9) 0.8) #false)
(define (close? p q delta)
  (< (distance p q) delta))

;; ----------------------------------------------------------------------------
;; a library of positions (Cartesian points) and vectors (steps between points)

;; Vec is Posn.
;; INTEREPRATION When we call a Posn a Vec, we think of it 'directionally'
;; as in x over on the horizontal and y over on the verticla axis from here 

;; Posn Posn -> Number
;; compute the distance between p and q
(check-expect (distance (make-posn 3 4) (make-posn 0 0)) 5)
(define (distance p q)
  (size (posn- p q)))

;; Vec -> Number 
;; determine the size (length) of p
(check-expect (size (make-posn 12 5)) 13)
(define (size p)
  (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))

;; Number Vec -> Vec
;; multiply s componentwise with v
(check-expect (posn* 2 (make-posn 1 3)) (make-posn 2 6))
(define (posn* s v)
  (make-posn (* s (posn-x v)) (* s (posn-y v))))

;; Posn Posn -> Vec
;; subtract q from p componentwise to obtain a vector
(check-expect (posn- (make-posn 3 2) (make-posn 3 8)) (make-posn 0 -6))
(define (posn- p q)
  (make-posn (- (posn-x p) (posn-x q)) (- (posn-y p) (posn-y q))))

;; Posn Vec -> Posn
;; add q to p componentwise
(check-expect (posn+ (make-posn 3 2) (make-posn 3 8)) (make-posn 6 10))
(define (posn+ p q)
  (make-posn (+ (posn-x p) (posn-x q)) (+ (posn-y p) (posn-y q))))

(module+ test
  (require rackunit)

  (define POSN0 (posn 40 40))
  (define POSN1 (posn 100 30))

  (define GOBBLER0 (gobbler (posn 10 10)  1 (posn 50 30)))
  (define GOBBLER1 (gobbler (posn 45 50)  0 (posn 45 50)))
  (define GOBBLER2 (gobbler (posn 30 100) 0 (posn 50 50)))
  
  (define PLAYER1 (player iworld1 GOBBLER0))
  (define PLAYER2 (player iworld2 GOBBLER1))
  (define PLAYER3 (player iworld3 GOBBLER2))
    
  (define WAITING0   (waiting '()))
  (define WAITING1   (waiting `(,iworld1)))
  (define COUNTDOWN0 (countdown '() `(,PLAYER1 ,PLAYER2)
                                `(,POSN0 ,POSN1) COUNTDOWN-TICKS))
  (define COUNTDOWN1 (countdown `(,iworld3) `(,PLAYER1 ,PLAYER2)
                                `(,POSN0 ,POSN1) COUNTDOWN-TICKS))
  (define COUNTDOWN2 (countdown `(,iworld3) `(,PLAYER2) `(,POSN0 ,POSN1)
                                COUNTDOWN-TICKS))
  (define COUNTDOWN3 (countdown '() `(,PLAYER1 ,PLAYER2)
                                `(,POSN0 ,POSN1) (- COUNTDOWN-TICKS 1)))
  (define PLAYING0   (playing '() `(,PLAYER1 ,PLAYER2) `(,POSN0 ,POSN1) 300))
  (define PLAYING1   (playing `(,iworld3) `(,PLAYER1 ,PLAYER2)
                              `(,POSN0 ,POSN1) 300))
  (define PLAYING1   (playing `(,iworld3) `(,PLAYER2) `(,POSN0 ,POSN1) 300))
  
  (check-equal? (queue-world WAITING0 iworld1) WAITING1)
  (check-equal? (queue-world COUNTDOWN0 iworld3) COUNTDOWN1)
  (check-equal? (queue-world PLAYING0 iworld3) PLAYING1)

  (check-equal? (drop-world WAITING1 iworld1) WAITING0)
  (check-equal? (drop-world COUNTDOWN1 iworld3) COUNTDOWN0)
  (check-equal? (drop-world COUNTDOWN1 iworld1) COUNTDOWN2)
  (check-equal? (drop-world PLAYING1 iworld3) PLAYING0)
  (check-equal? (drop-world PLAYING1 iworld1) PLAYING2)

  (check-equal? (advance-game WAITING0) WAITING0)
  (check-equal? (advance-game WAITING1) WAITING1)
  (check-equal? (advance-game COUNTDOWN0) COUNTDOWN3)
                
  "all tests run")















