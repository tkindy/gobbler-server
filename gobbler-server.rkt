#! /bin/sh
#|
exec racket -tm "$0" ${1+"$@"}
|#
#lang racket

(provide
 ;; PortNumber [N] -> Void
 ;; consumes the portnumber and optionally the number of game ticks
 ;; run as ./gobbler-server port-number [game-ticks]
 main)

(require 2htdp/universe)

;; This will run at dictionary.ccs.neu.edu:20000

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
(struct game [admin seen-players queue] #:transparent)
(struct waiting game [] #:transparent)
(struct ready game [players foods time-left] #:transparent)
(struct countdown ready [] #:transparent)
(struct playing ready [] #:transparent)

;; A Turkey is a (turkey posn? N [Maybe posn?])
;; - represents a playable turkey

;; A Player is a (player iworld? turkey?)
;; - represents currently-playing player information

;; A Game is a (game [Maybe iworld?] [Listof (cons string? number?)] [Listof iworld?])
;; represents the base state of the game
;; - admin: the admin world, if connected
;; - seen-players: the players seen so far and the number of games they've played
;; - queue: the players currently waiting to play

;; A Waiting is a (waiting [Maybe iworld?] [Listof string?] [Listof iworld?])
;; represents the phase where not enough players have joined

;; A Ready is a (ready [Maybe iworld?]
;;                     [Listof (cons string? number?)]
;;                     [Listof iworld?]
;;                     [Listof player?]
;;                     [Listof posn?]
;;                     N)
;; represents a world that is ready to play
;; - players: the players currently playing the game
;; - foods: the locations of the foods
;; - time-left: the time left in this phase, in ticks

;; A Countdown is a (countdown [Maybe iworld?]
;;                             [Listof (cons string? number?)]
;;                             [Listof iworld?]
;;                             [Listof player?]
;;                             [Listof posn?]
;;                             N)
;; represents the phase just before starting a new game

;; A Playing is a (make-playing [Maybe iworld?]
;;                              [Listof (cons string? number?)]
;;                              [Listof iworld?]
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
;; - ErrorMessage
;; Represents a message that is sent from the server to the clients

(define WAITING 'waiting)
;; A WaitingMessage is a (list 'waiting N N)
;; Represents information in the waiting phase where:
;; - the first number represents the number of players waiting and
;; - the second number represents the number of players needed to play

(define COUNTDOWN 'countdown)
;; A CountdownMessage is a (list 'countdown
;;                               [Listof PlayerMessage]
;;                               [Listof FoodMessage]
;;                               N
;;                               [Maybe PlayerMessage]
;; Represents information on a pre-game countdown where:
;; - the [Listof PlayerMessage] is the other turkeys that are about to play
;;   the game
;; - the [Listof FoodMessage] is the locations of the foods on the board
;; - the N is the amount of time left before the game begins
;; - the [Maybe PlayerMessage] is the receiving player's turkey, or false if
;;   they are observing

(define PLAYING 'playing)
;; A PlayingMessage is a (list 'playing
;;                             [Listof PlayerMessage]
;;                             [Listof FoodMessage]
;;                             N
;;                             [Maybe PlayerMessage]
;; Represents information on an in-progress game where:
;; - the [Listof PlayerMessage] is other turkeys currently playing
;; - the [Listof FoodMessage] is the locations of the foods on the board
;; - the N is the amount of time left before the game ends
;; - the [Maybe PlayerMessage] is the receiving player's turkey, or false if
;;   they are observing

(define GAME-OVER 'game-over)
;; A GameOverMessage is a (list 'game-over OutcomeMessage)
;; Represents information on the outcome of the game

;; An ErrorMessage is a string?
;; Represents an error experienced by the server

;; An OutcomeMessage is a (Listof string?)
;; Represents the names of the winners of the game

;; A PlayerMessage is a (list string? N N N)
;; Represents the location of a player where:
;; - the string is the player's name
;; - the first number is their turkey's x coordinate
;; - the second number is their turkey's y coordinate
;; - the third number is the number of foods their turkey has eaten

;; A FoodMessage is a (list N N)
;; Represents the location of a food where:
;; - the first number is its x coordinate
;; - the second number is its y coordinate

;; A Client2ServerMessage is a WaypointMessage
;; Represents a message that is sent from the client to the server

(define WAYPOINT 'waypoint)
;; A WaypointMessage is a (list 'waypoint N N)
;; Represents a message from the client to update their waypoint such that:
;; - the first number is the x coordinate
;; - the second number is the y coordinate

;; An Admin2ServerMessage is one of:
;; - (list 'size N)  ; set the size of the play area
;; - 'go             ; start the game

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
(define MAX-FOODS 10)

(define INITIAL-STATE (waiting #f '() '()))

(define GAME-SIZE 600)
(define CLOSE 6)

(define OUTPUT "")
(define OUTPUT-ENABLED? #f)

(define ADMIN-CLIENT "admin")

;; the message template to send a client if an unexpected message was received
(define UNEXPECTED-MSG "unexpected message while in ~a state: ~s")

;; =====================================
;; SERVER
;; =====================================

(define (main p (gt GAME-TICKS))
  (set! GAME-TICKS (if (string? gt) (string->number gt) gt))
  (define the-port (if (string? p) (string->number p) p))
  (unless (and (number? the-port) (number? GAME-TICKS))
    (error 'gobbler-server "start with ./gobbler-server port-number [game-ticks]"))

  (set! OUTPUT (format "out/output~a.txt" (current-seconds)))
  (set! OUTPUT-ENABLED? #t)
  (log-info (format "running gobbler-server on port ~a for ~a ticks" the-port GAME-TICKS))

  (void
   (universe INITIAL-STATE
             [port          the-port]
             [on-new        sign-up]
             [on-disconnect drop-world]
             [on-tick       advance-game (/ TICKS-PER-SECOND)]
             [on-msg        receive-msg]
             [state         #t])))

;; GobblerUniverse iworld? -> GobblerBundle
;; Sign up the new world
(define (sign-up uni world)
  (cond
    [(not (string? (iworld-name world)))
     (error-bundle uni world "name must be a string")]
    [(admin? world)
     (log-admin "connected")
     (add-admin uni world)]
    [else
     (queue-world uni world)]))

;; iworld? -> boolean?
;; Is this world the admin?
(define (admin? world)
  (equal? (iworld-name world)
          ADMIN-CLIENT))

;; GobblerUniverse iworld? -> GobblerBundle
;; Add the admin to the universe
(define (add-admin uni world)
  (cond
    [(waiting? uni)
     (waiting world
              (game-seen-players uni)
              (game-queue uni))]
    [(countdown? uni)
     (countdown world
                (game-seen-players uni)
                (game-queue uni)
                (ready-players uni)
                (ready-foods uni)
                (ready-time-left uni))]
    [(playing? uni)
     (playing world
              (game-seen-players uni)
              (game-queue uni)
              (ready-players uni)
              (ready-foods uni)
              (ready-time-left uni))]))

;; GobblerUniverse iworld? -> GobblerUniverse
;; Queue the new player
(define (queue-world uni world)
  (let ([nu-q    (append (game-queue uni) (list world))]
        [nu-seen (extend-seen-players (game-seen-players uni) (iworld-name world))])
    (log-world "INFO" (iworld-name world) (format "connected (~a)" (length nu-q)))
    (cond
      [(waiting? uni)
       (waiting (game-admin uni) nu-seen nu-q)]
      [(countdown? uni)
       (countdown (game-admin uni) nu-seen nu-q (ready-players uni) (ready-foods uni) (ready-time-left uni))]
      [else
       (playing (game-admin uni) nu-seen nu-q (ready-players uni) (ready-foods uni) (ready-time-left uni))])))

;; [Listof string?] string? -> [Listof string?]
;; Add the new player name to the list, checking if they had already connected before
(define (extend-seen-players seen name)
  (if (assoc name seen)
      (begin
        (log-world "ERROR" name "!!!RECONNECT!!!")
        seen)
      (cons (cons name 0)
            seen)))

;; iworld? -> player?
;; Create a new player at a random location
(define (new-player w)
  ;; -> turkey?
  ;; Create a new turkey at a random location
  (define (new-turkey)
    (turkey (random-posn) 0 #f))
  (player w (new-turkey)))

;; -> [Listof posn?]
;; Generate a random list of food for the start of the game
(define (generate-food)
  (map (λ (n) (random-posn)) (range MAX-FOODS)))

;; -> posn?
;; Create a new posn at a random location
(define (random-posn)
  (posn (random GAME-SIZE)
        (random GAME-SIZE)))

;; GobblerUniverse iworld? boolean? -> GobblerUniverse
;; Remove player from the game
(define (drop-world uni world (too-long? #f))
  ;; [Listof player?] -> [Listof player?]
  (define (drop-player* players)
    (drop-player players world))
  (define new-queue (drop-queued (game-queue uni) world))
  (define seen-entry (assoc (iworld-name world) (game-seen-players uni)))

  (if (boolean? seen-entry)
      (log-world "ERROR" (iworld-name world) "No seen-players entry")
      (if (and (not too-long?) (not (playing? uni)) (equal? (cdr seen-entry) 1))
          (log-world "INFO"  (iworld-name world) "!!!SUCCESS!!!")
          (log-world "ERROR" (iworld-name world) "!!!BAD DISCONNECT!!!")))

  (cond
    [(waiting? uni)   (waiting (game-admin uni) (game-seen-players uni) new-queue)]
    [(countdown? uni) (countdown (game-admin uni)
                                 (game-seen-players uni)
                                 new-queue
                                 (drop-player* (ready-players uni))
                                 (ready-foods uni)
                                 (ready-time-left uni))]
    [(playing? uni)   (playing (game-admin uni)
                               (game-seen-players uni)
                               new-queue
                               (drop-player* (ready-players uni))
                               (ready-foods uni)
                               (ready-time-left uni))]))

;; [Listof iworld?] iworld? -> [Listof iworld?]
;; Drop the iworld from the game queue
(define (drop-queued queue world)
  (remove world queue iworld=?))

;; [Listof player?] iworld? -> [Listof player?]
;; Drop the player with the iworld from the game
(define (drop-player players world)
  (filter (λ (p) (not (iworld=? world (player-iworld p))))
          players))

;; -----------------------------------------------------------------------------
;; GobblerUniverse -> GobblerBundle
;; Advance the state of the universe
(define (advance-game uni)
  (cond
    [(waiting? uni) (advance-waiting uni)]
    [(countdown? uni) (advance-countdown uni)]
    [else (advance-playing uni)]))

;; waiting? -> GobblerBundle
;; Advance the waiting state
(define (advance-waiting uni)
  (let* ([waiting-msg (list WAITING
                            (length (game-queue uni))
                            0)]
         [mail (map (λ (w) (make-mail w waiting-msg))
                    (non-players uni))])
    (make-bundle uni mail '())))

;; GobblerUniverse -> [Listof iworld?]
;; Get the list of connected non-players (queue + admin)
(define (non-players uni)
  (if (game-admin uni)
      (cons (game-admin uni) (game-queue uni))
      (game-queue uni)))

;; countdown? -> GobblerBundle
;; Advance the countdown state
(define (advance-countdown uni)
  (define new-uni
    (if (<= (ready-time-left uni) 0)
        (begin
          (log-info "entering playing")
          (playing (game-admin uni)
                   (game-seen-players uni)
                   (game-queue uni)
                   (ready-players uni)
                   (ready-foods uni)
                   GAME-TICKS))
        (countdown (game-admin uni)
                   (game-seen-players uni)
                   (game-queue uni)
                   (ready-players uni)
                   (ready-foods uni)
                   (sub1 (ready-time-left uni)))))

  (ready-bundle new-uni))

;; ready? -> (U CountdownMessage PlayingMessage)
;; Build the bundle for the given ready universe
(define (ready-bundle uni)
  (define non-player-mail (ready-non-player-mail uni))
  (define player-mail (ready-player-mail uni))

  (make-bundle uni
               (append non-player-mail player-mail)
               '()))

;; ready? -> [Listof Outgoing]
;; Build the mail for the users in the queue
(define (ready-non-player-mail uni)
  (map (λ (world) (make-mail world (ready-msg uni #f)))
       (non-players uni)))

;; ready? -> [Listof Outgoing]
;; Build the mail for the players in the game
(define (ready-player-mail uni)
  (map (λ (p) (make-mail (player-iworld p) (ready-msg uni p)))
       (ready-players uni)))

;; ready? [Maybe player?] -> (U CountdownMessage PlayingMessage)
;; Build a ready message for the given player
(define (ready-msg uni aplayer)
  (define msg-type (if (countdown? uni) COUNTDOWN PLAYING))
  (list msg-type
        (map player-msg (ready-players uni))
        (map food-msg (ready-foods uni))
        (ready-time-left uni)
        (if (boolean? aplayer) #f (player-msg aplayer))))

;; player? -> PlayerMessage
;; Build a player message
(define (player-msg p)
  (define t (player-turkey p))
  (define loc (turkey-loc t))
  (list (iworld-name (player-iworld p))
        (posn-x loc)
        (posn-y loc)
        (turkey-food-eaten t)))

;; posn? -> FoodMessage
;; Build a food message
(define (food-msg f)
  (list (posn-x f) (posn-y f)))

;; playing? -> GobblerBundle
;; Move all the turkeys toward their goal and update the time left
(define (advance-playing game)
  ; TODO LOG OUT WINNER
  ;; [Listof (cons string? boolean?)] [Listof player?]
  ;; Update the seen list
  (define (update-seen seen player-names)
    (map (λ (entry)
           (if (member (car entry) player-names)
               (cons (car entry) (add1 (cdr entry)))
               entry))
         seen))

  (if (<= (ready-time-left game) 0)
      (let ([player-worlds (map player-iworld (ready-players game))])
        (log-info "entering waiting")
        (make-bundle (waiting (game-admin game)
                              (update-seen (game-seen-players game) (map iworld-name player-worlds))
                              (append (game-queue game)
                                      player-worlds))
                     (game-over-mails game)
                     '()))
      (next-playing-state game)))

;; playing? -> [Listof mail?]
;; Produce the mails to send to the players after the game is over
(define (game-over-mails game)
  (define winners (choose-winners (ready-players game)))
  (log-info (format "~a win!" winners))
  
  (define game-over-msg (list GAME-OVER winners))
  (define player-worlds (map player-iworld (ready-players game)))

  (map (λ (world) (make-mail world game-over-msg))
       (append (non-players game) player-worlds)))

;; [Listof player?] -> [Listof string?]
;; Pick the player(s) who ate the most food
(define (choose-winners players)
  ;; player? -> N
  ;; get the number of foods the player's turkey has eaten
  (define (num-foods p)
    (turkey-food-eaten (player-turkey p)))
  ;; player? -> string?
  ;; get the player's name
  (define (name p)
    (iworld-name (player-iworld p)))
  (second
   (foldr (λ (p acc)
            (define foods (num-foods p))
            (cond
              [(> foods (first acc))
               (list foods (list (name p)))]
              [(= foods (first acc))
               (list foods
                     (cons (name p)
                           (second acc)))]
              [else acc]))
          (list 0 '())
          players)))


;; playing? -> GobblerBundle
;; Update a game in progress
(define (next-playing-state game)
  (define new-data
    (update-turkeys-and-food
     (ready-players game)
     (ready-foods game)))
  (define new-uni
    (playing (game-admin game)
             (game-seen-players game)
             (game-queue game)
             (move-all-players (first new-data))
             (second new-data)
             (sub1 (ready-time-left game))))

  (ready-bundle new-uni))

;; [Listof player?] [Listof posn?] -> (list [Listof player?] [Listof posn?])
;; Fatten turkeys who ate food, remove the food they ate, and generate new food
(define (update-turkeys-and-food lot lof)
  ;; posn? (list [Listof player?] [Listof posn?]) -> (list [Listof player?] [Listof posn?])
  ;; Update the turkeys with the given food (fatten the first turkey to eat it
  ;; and remove it if it is eaten)
  (define (update-turkeys-with-food afood sofar)
    (list (fatten-first-turkey (first sofar) afood)
          (if (was-eaten? (first sofar) afood)
              (cons (random-posn) (second sofar))
              (cons afood (second sofar)))))
  (foldr update-turkeys-with-food (list lot '()) lof))

;; fatten-first-turkey : [Listof player?] posn? -> [Listof player?]
;; Fatten the first turkey to eat this food (if any)
(define (fatten-first-turkey all-turkeys afood)
  (cond [(empty? all-turkeys) '()]
        [else
         (if (turkey-eat-food? (player-turkey (first all-turkeys)) afood)
             (cons (fatten-player (first all-turkeys)) (rest all-turkeys))
             (cons (first all-turkeys) (fatten-first-turkey (rest all-turkeys) afood)))]))

;; player? -> player?
;; Increase the size of the given player's turkey
(define (fatten-player p)
  (player (player-iworld p)
          (fatten-turkey (player-turkey p))))

;; turkey? -> turkey?
;; Increase the size of the given turkey
(module+ test
  (check-equal? (fatten-turkey (turkey (posn 1 2) 10 (posn 3 4)))
                (turkey (posn 1 2) 11 (posn 3 4))))

(define (fatten-turkey t)
  (turkey (turkey-loc t)
          (add1 (turkey-food-eaten t))
          (turkey-waypoint t)))

;; was-eaten? : [Listof player?] Food -> Boolean
;; Was the given food eaten by any player?
(define (was-eaten? all-players afood)
  (ormap (λ (p) (turkey-eat-food? (player-turkey p) afood)) all-players))

;; [Listof player?] -> [Listof player?]
;; Move all the players' turkeys towards their goal
(define (move-all-players lop)
  (map move-player lop))

;; player? -> player?
;; Move a single player's turkey towards its goal if it exists
(define (move-player aplayer)
  (player (player-iworld aplayer)
          (move-turkey (player-turkey aplayer))))

;; turkey? -> turkey?
;; Move the turkey towards its goal
(define (move-turkey aturkey)
  (turkey (translate-loc (turkey-loc aturkey) (turkey-waypoint aturkey) TKY-STEP)
          (turkey-food-eaten aturkey)
          (turkey-waypoint aturkey)))

;; posn? [Maybe posn?] N -> posn?
;; translate the given Posn (if any) toward the goal
(define (translate-loc loc goal close)
  (cond
    [(false? goal) loc]
    [(posn? goal) (move-toward loc goal close)]))

;; Turkey Posn -> Boolean
;; Has the turkey eaten the food?
(define (turkey-eat-food? aturkey afood)
  (close? (turkey-loc aturkey) afood CLOSE))

;; ---------------------------------------------------------------------------------------------------
;; GobblerUniverse iworld? sexp? -> GobblerBundle
;; Handle a message received from a client
(define (receive-msg uni world sexp)
  (cond
    [(admin? world) (administrate uni world sexp)]
    [(not (waypoint-message? sexp)) (error-bundle uni world (format UNEXPECTED-MSG (universe-state uni) sexp))]
    [(waiting? uni)   uni]
    [(countdown? uni) uni]
    [(playing? uni) (update-waypoint uni world sexp)]))

;; GobblerUniverse iworld? string? -> GobblerBundle
;; Produce a bundle that sends the world an error message and disconnects from it
(define (error-bundle uni world msg)
  (log-world "ERROR" (iworld-name world) msg)
  (make-bundle (drop-world uni world) (list (make-mail world msg)) (list world)))

;; GobblerUniverse -> symbol?
;; Get the universe state
(define (universe-state uni)
  (cond
    [(waiting? uni)   WAITING]
    [(countdown? uni) COUNTDOWN]
    [(playing? uni)   PLAYING]))

;; GobblerUniverse iworld? sexp? -> GobblerBundle
;; Handle the admin message
(define (administrate uni world sexp)
  (define (admin-error msg)
    (log-admin msg)
    (make-bundle uni
                 `(,(make-mail world (format "~a~n" msg)))
                 '()))
  
  (if (waiting? uni)
      (match sexp
        [`(size ,(? real? s)) (set! GAME-SIZE s)
                              (log-admin (format "set size to ~a" GAME-SIZE))
                              uni]
        ['go (start-game uni)]
        ['drop (drop-remaining-worlds uni)]
        [_ (admin-error (format "Unknown command: ~a" sexp))])
      (admin-error (format "Can't alter server configuration; ~a" (universe-state uni)))))

;; waiting? -> GobblerUniverse
;; Start the game with all the players
(define (start-game uni)
  (log-info "entering countdown")
  (countdown (game-admin uni)
             (game-seen-players uni)
             '()
             (map new-player (game-queue uni))
             (generate-food)
             COUNTDOWN-TICKS))

;; waiting? -> GobblerUniverse
;; Drop the rest of the worlds that haven't gracefully disconnected already
(define (drop-remaining-worlds uni)
  (log-admin "dropping overdue worlds")
  
  (define msg "took too long to disconnect")
  (define mail (map (λ (w)
                      (log-world "ERROR"
                                 (iworld-name w)
                                 (format "!!!BAD DISCONNECT!!! ~a" msg))
                      (make-mail w (format "~a~n" msg)))
                    (game-queue uni)))

  (set! OUTPUT (format "out/output~a.txt" (current-seconds)))
  (make-bundle (waiting (game-admin uni)
                        '()
                        '())
               mail
               (game-queue uni)))

;; GobblerUniverse iworld? sexp? -> GobblerBundle
;; Update the waypoint of the player
(define (update-waypoint uni world sexp)
  (playing (game-admin uni)
           (game-seen-players uni)
           (game-queue uni)
           (update-waypoint/players (ready-players uni) world sexp)
           (ready-foods uni)
           (ready-time-left uni)))

;; [Listof player?] iworld? WaypointMessage -> [Listof player?]
;; Update the waypoint of the player
(define (update-waypoint/players players world waypoint-msg)
  (define waypoint (waypoint-message->posn waypoint-msg))

  (if (on-screen? waypoint)
      (map (λ (p)
             (if (iworld=? (player-iworld p) world)
                 (let ([t (player-turkey p)])
                   (player world
                           (turkey (turkey-loc t)
                                   (turkey-food-eaten t)
                                   waypoint)))
                 p))
           players)
      players))

;; =====================================
;; UTILS
;; =====================================

;; sexp? -> Boolean
;; Determines if the given sexp is a well-formed waypoint message
(define (waypoint-message? sexp)
  (and (list? sexp)
       (= (length sexp) 3)
       (symbol? (first sexp))
       (symbol=? (first sexp) WAYPOINT)
       (real? (second sexp))
       (real? (third sexp))))

;; WaypointMessage -> Posn
;; Converts the given waypoint message into a posn
(define (waypoint-message->posn waypoint)
  (posn (second waypoint) (third waypoint)))

;; Posn -> Boolean
;; Determines if the waypoint is valid
(define (on-screen? posn)
  (and (in-range? (posn-x posn) 0 GAME-SIZE)
       (in-range? (posn-y posn) 0 GAME-SIZE)))

;; number? number? number? -> boolean?
;; Determine if n is between min (inclusive) and max (exclusive)
(define (in-range? n min max)
  (and (>= n min)
       (< n max)))

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
;; LOGGING
;; =====================================

;; string? string? string? -> Void
;; Log the message pertaining to the world at the given severity
(define (log-world sev name l)
  (log-severity sev (format "<~a> ~a" name l)))

;; string? -> Void
;; Log the info message
(define (log-info l)
  (log-severity "INFO" l))

;; string? -> Void
;; Log the error message
(define (log-error l)
  (log-severity "ERROR" l))

;; string? -> Void
;; Log the admin message
(define (log-admin l)
  (log-severity "ADMIN" l))

;; string? string? -> Void
;; Log the message at the given severity
(define (log-severity sev l)
  (log (format "[~a] ~a~n" sev l)))

;; string? -> Void
;; Output the line to the file and STDOUT
(define (log l)
  (display l)
  (when OUTPUT-ENABLED?
    (let ([out (open-output-file OUTPUT #:exists 'append)])
      (display l out)
      (close-output-port out))))


;; =====================================
;; TESTS
;; =====================================

(module+ test
  (require rackunit)

  ;; =====================================
  ;; TESTING DATA & UTILS
  ;; =====================================
  (define POSN0   (posn 40 40))
  (define POSN1   (posn 100 30))
  (define POSN2   (posn 20 70))
  (define POSN2.1 (posn 26 62))
  (define POSN3   (posn 50 30))
  (define POSN3.1 (posn 30 20))
  (define POSN4   (posn 45 50))
  (define POSN5   (posn 30 100))
  (define POSN6   (posn 50 50))

  (define TURKEY0   (turkey POSN2 1 POSN3))
  (define TURKEY0.1 (turkey POSN2 1 POSN3.1))
  (define TURKEY0.2 (turkey POSN2.1 1 POSN3))

  (define TURKEY1   (turkey POSN4 0 POSN4))
  (define TURKEY1.1 (turkey POSN4 1 POSN4))

  (define TURKEY2   (turkey POSN5 0 POSN6))
  (define TURKEY2.1 (turkey POSN5 1 POSN6))

  (define PLAYER1   (player iworld1 TURKEY0))
  (define PLAYER1.1 (player iworld1 TURKEY0.1))
  (define PLAYER1.2 (player iworld1 TURKEY0.2))

  (define PLAYER2   (player iworld2 TURKEY1))

  (define PLAYER3   (player iworld3 TURKEY2))

  (define SEEN0 '())
  (define SEEN1 (cons (cons "iworld1" 0)
                      SEEN0))
  (define SEEN1.1 (cons (cons "iworld1" 1)
                        SEEN0))
  (define SEEN2 (cons (cons "iworld2" 0)
                      SEEN1))
  (define SEEN2.1 (cons (cons "iworld2" 1)
                        SEEN1.1))
  (define SEEN3 (cons (cons "iworld3" 0)
                      SEEN2))

  (define WAITING0   (waiting #f SEEN0 '()))
  (define WAITING1   (waiting #f SEEN1 `(,iworld1)))
  (define WAITING1.1 (waiting #f SEEN1 '()))
  (define WAITING2   (waiting #f SEEN2 `(,iworld1 ,iworld2)))
  (define WAITING2.1 (waiting #f SEEN2.1 `(,iworld1 ,iworld2)))
  (define COUNTDOWN0 (countdown #f SEEN2 '() `(,PLAYER1 ,PLAYER2)
                                `(,POSN0 ,POSN1) COUNTDOWN-TICKS))
  (define COUNTDOWN1 (countdown #f SEEN3 `(,iworld3)
                                `(,PLAYER1 ,PLAYER2)
                                `(,POSN0 ,POSN1) COUNTDOWN-TICKS))
  (define COUNTDOWN1.1 (countdown #f SEEN3 '()
                                  `(,PLAYER1 ,PLAYER2)
                                  `(,POSN0 ,POSN1) COUNTDOWN-TICKS))
  (define COUNTDOWN2 (countdown #f SEEN3 `(,iworld3)
                                `(,PLAYER2) `(,POSN0 ,POSN1)
                                COUNTDOWN-TICKS))
  (define COUNTDOWN3 (countdown #f SEEN2 '() `(,PLAYER1 ,PLAYER2)
                                `(,POSN0 ,POSN1) (- COUNTDOWN-TICKS 1)))
  (define COUNTDOWN4 (countdown #f SEEN2 '() `(,PLAYER1 ,PLAYER2)
                                `(,POSN0 ,POSN1) 0))
  (define PLAYING0   (playing #f SEEN2 '() `(,PLAYER1 ,PLAYER2)
                              `(,POSN0 ,POSN1) GAME-TICKS))
  (define PLAYING0.1 (playing #f SEEN2 '() `(,PLAYER1.2 ,PLAYER2) `(,POSN0 ,POSN1)
                              (- GAME-TICKS 1)))
  (define PLAYING1   (playing #f SEEN3 `(,iworld3) `(,PLAYER1 ,PLAYER2)
                              `(,POSN0 ,POSN1) GAME-TICKS))
  (define PLAYING1.1 (playing #f SEEN3`(,iworld3) `(,PLAYER1.1 ,PLAYER2)
                              `(,POSN0 ,POSN1) GAME-TICKS))
  (define PLAYING1.2 (playing #f SEEN3 `(,iworld3) `(,PLAYER2)
                              `(,POSN0 ,POSN1) GAME-TICKS))
  (define PLAYING1.3 (playing #f SEEN3 '() `(,PLAYER1 ,PLAYER2)
                              `(,POSN0 ,POSN1) GAME-TICKS))
  (define PLAYING2   (playing #f SEEN3 `(,iworld3) `(,PLAYER2) `(,POSN0 ,POSN1)
                              GAME-TICKS))
  (define PLAYING3   (playing #f SEEN2 '() `(,PLAYER1 ,PLAYER2) `(,POSN0 ,POSN1)
                              0))

  (define BUNDLE0    (make-bundle
                      WAITING2.1
                      (list (make-mail iworld1 `(,GAME-OVER ("iworld1")))
                            (make-mail iworld2 `(,GAME-OVER ("iworld1"))))
                      '()))
  (define BUNDLE1    (make-bundle WAITING0 '() '()))
  (define BUNDLE2    (make-bundle WAITING1 `(,(make-mail iworld1 '(waiting 1 0))) '()))

  (define player-msg0   '("iworld1" 20 70 1))
  (define player-msg0.1 '("iworld1" 26 62 1))
  (define player-msg1   '("iworld2" 45 50 0))

  (define players0   (list player-msg0 player-msg1))
  (define players0.1 (list player-msg0.1 player-msg1))

  (define foods0 '((40 40) (100 30)))

  (define msg-core '((("iworld1" 20 70 1)
                      ("iworld2" 45 50 0))
                     ((40 40) (100 30))))

  (define msg0 (list 'countdown players0 foods0 83 player-msg0))
  (define msg1 (list 'countdown players0 foods0 83 player-msg1))
  (define msg2 (list 'playing players0 foods0 1680 player-msg0))
  (define msg3 (list 'playing players0 foods0 1680 player-msg1))
  (define msg4 (list 'playing players0.1 foods0 1679 player-msg0.1))
  (define msg5 (list 'playing players0.1 foods0 1679 player-msg1))

  (define (mails msg0 msg1)
    (list
     (make-mail iworld1 msg0)
     (make-mail iworld2 msg1)))

  (define mails0 (mails msg0 msg1))
  (define mails1 (mails msg2 msg3))
  (define mails2 (mails msg4 msg5))

  (define BUNDLE3 (make-bundle COUNTDOWN3 mails0 '()))
  (define BUNDLE4 (make-bundle PLAYING0 mails1 '()))
  (define BUNDLE5 (make-bundle PLAYING0.1 mails2 '()))

  (define BUNDLE6
    (make-bundle
     PLAYING1.2
     `(,(make-mail iworld1 (format UNEXPECTED-MSG "playing" '(here is some garbage))))
     `(,iworld1)))
  (define BUNDLE7
    (make-bundle
     WAITING1.1
     `(,(make-mail iworld1 (format UNEXPECTED-MSG "waiting" '(waypoint 30 20))))
     `(,iworld1)))

  ;; =====================================
  ;; SERVER TESTS
  ;; =====================================
  (check-equal? (queue-world WAITING0 iworld1) WAITING1)
  (check-equal? (queue-world WAITING1 iworld2) WAITING2)
  (check-equal? (queue-world COUNTDOWN0 iworld3) COUNTDOWN1)
  (check-equal? (queue-world PLAYING0 iworld3) PLAYING1)

  (check-equal? (drop-world WAITING1 iworld1) WAITING1.1)
  (check-equal? (drop-world COUNTDOWN1 iworld3) COUNTDOWN1.1)
  (check-equal? (drop-world PLAYING1 iworld3) PLAYING1.3)

  (check-equal? (drop-world COUNTDOWN1 iworld1) COUNTDOWN2)
  (check-equal? (drop-world PLAYING1 iworld1) PLAYING2)

  (check-equal? (drop-queued '() iworld1) '())
  (check-equal? (drop-queued `(,iworld1) iworld1) '())
  (check-equal? (drop-queued `(,iworld1 ,iworld2 ,iworld3) iworld2)
                `(,iworld1 ,iworld3))

  (check-equal? (drop-player '() iworld1) '())
  (check-equal? (drop-player `(,PLAYER1) iworld1) '())
  (check-equal? (drop-player `(,PLAYER1 ,PLAYER2 ,PLAYER3) iworld2)
                `(,PLAYER1 ,PLAYER3))

  (check-equal? (advance-game WAITING0) BUNDLE1)
  (check-equal? (advance-game WAITING1) BUNDLE2)
  (check-equal? (advance-game COUNTDOWN0) BUNDLE3)
  (check-equal? (advance-game COUNTDOWN4) BUNDLE4)
  (check-equal? (advance-game PLAYING0) BUNDLE5)
  (check-equal? (advance-game PLAYING3) BUNDLE0)

  (check-true (turkey-eat-food? TURKEY1 (posn 45 50)))
  (check-false (turkey-eat-food? TURKEY2 (posn 100 200)))

  (check-equal? (receive-msg PLAYING1 iworld1 '(here is some garbage))
                BUNDLE6)
  (check-equal? (receive-msg PLAYING1 iworld3 '(waypoint 30 20))
                PLAYING1)
  (check-equal? (receive-msg PLAYING1 iworld1 '(waypoint -1000 -1000))
                PLAYING1)
  (check-equal? (receive-msg PLAYING1 iworld1 '(waypoint 30 20))
                PLAYING1.1)
  (check-equal? (receive-msg PLAYING1 iworld3 '(waypoint 30 20))
                PLAYING1)
  (check-equal? (receive-msg PLAYING1 iworld1 '(waypoint -1000 -1000))
                PLAYING1)
  (check-equal? (update-waypoint/players `(,PLAYER1 ,PLAYER2) iworld1 '(waypoint 30 20))
                `(,PLAYER1.1 ,PLAYER2))

  ;; =====================================
  ;; UTILS TESTS
  ;; =====================================

  (check-true (waypoint-message? '(waypoint 3 3)))
  (check-true (waypoint-message? '(waypoint -23 234)))
  (check-false (waypoint-message? '(waypoint 1 2 3)))
  (check-false (waypoint-message? '(waypoint 2)))
  (check-false (waypoint-message? '(garbage 1 2)))
  (check-false (waypoint-message? 'waypoint))
  (check-false (waypoint-message? '(1 2)))
  (check-false (waypoint-message? '(1 2 3)))
  (check-false (waypoint-message? 4))

  (check-equal? (waypoint-message->posn '(waypoint 0 0)) (posn 0 0))
  (check-equal? (waypoint-message->posn '(waypoint 10 100)) (posn 10 100))
  (check-equal? (waypoint-message->posn '(waypoint -10 10)) (posn -10 10))

  (check-true (on-screen? (posn 0 0)))
  (check-true (on-screen? (posn (sub1 GAME-SIZE) (sub1 GAME-SIZE))))
  (check-false (on-screen? (posn GAME-SIZE GAME-SIZE)))
  (check-false (on-screen? (posn -1 -1)))

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
                      .1))

  (check-true (close? (posn 10 10) (posn 10 9) 2.0))
  (check-false (close? (posn 10 10) (posn 10 9) 0.8))
  (check-equal? (distance (posn 3 4) (posn 0 0)) 5)
  (check-equal? (size (posn 12 5)) 13)
  (check-equal? (posn* 2 (posn 1 3)) (posn 2 6))
  (check-equal? (posn- (posn 3 2) (posn 3 8)) (posn 0 -6))
  (check-equal? (posn+ (posn 3 2) (posn 3 8)) (posn 6 10))

  (check-true (andmap on-screen? (generate-food)))

  "all tests run")

; (main 20000)
