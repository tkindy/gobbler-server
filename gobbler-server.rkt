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

;; A GobblerUniverse is one of:
;; - Waiting
;; - Countdown
;; - Playing

;; A GobblerBundle is one of:
;; - GobblerUniverse
;; - (make-bundle GobblerUniverse [Listof Outgoing] '())

;; An Outgoing is a (make-mail iworld? ServerMessage)
;; Represents a message that is to be sent to the iworld

(struct gobbler [loc food-eaten waypoint])
;; A Gobbler is a (gobbler posn? N posn?)
;; - loc: the current position of the turkey
;; - food-eaten: the number of pieces of food eaten
;; - waypoint: the current destination of the turkey

(define-struct player [iworld gobbler])
;; A Player is a (make-player iworld? gobbler?)
;; - iworld: the world connection struct
;; - gobbler: the gobbler controlled by this player
;; Maybe we will add player number here, maybe not. Future step

(struct game [queue])
;; A Game is a (game [Listof iworld?])
;; represents the base state of the game
;; - queue: the players currently waiting to play

(struct waiting game [])
;; A Waiting is a (waiting [Listof iworld?])
;; represents the phase where not enough players have joined

(struct ready game [players foods time-left])
;; A Ready is a (ready [Listof iworld?]
;;                     [Listof player?]
;;                     [Listof posn?]
;;                     N)
;; represents a world that is ready to play
;; - players: the players currently playing the game
;; - foods: the locations of the foods
;; - time-left: the time left in this phase

(struct countdown ready [])
;; A Countdown is a (countdown [Listof iworld?]
;;                             [Listof player?]
;;                             [Listof posn?]
;;                             N)
;; represents the phase just before starting a new game

(struct playing ready [])
;; A Playing is a (make-playing [Listof iworld?]
;;                              [Listof player?]
;;                              [Listof posn?]
;;                              N)
;; represents the phase where the game is being played


;; PROTOCOLS

;; A ServerMessage is one of:
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

(define WON 'won)
(define LOST 'lost)
;; An OutcomeMessage is one of:
;; - 'won
;; - 'lost
;; Represents whether the receiving player won or lost

;; A GobblerMessage is a (list N N)
;; Represents the location of a gobbler where:
;; - the first number is its x coordinate
;; - the second number is its y coordinate

;; A FoodMessage is a (list N N)
;; Represents the location of a gobbler where:
;; - the first number is its x coordinate
;; - the second number is its y coordinate



;; A ClientMessage is a WaypointMessage
;; Represents a message that is sent from the client to the server

;; A WaypointMessage is a (list 'waypoint N N)
;; Represents a message from the client to update their waypoint such that:
;; - the first number is the x coordinate
;; - the second number is the y coordinate




