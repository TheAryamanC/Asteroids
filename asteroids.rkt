;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname asteroids) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require "./define_super_struct.rkt")
(require "./asteroids_lib.rkt")

;;; Type definitions

;; This is the base type of all objects on screen.
;; However, this is an "abstract" type.  We will never say (make-game-object ...), we'll
;; make different *subtypes* of game-object.
(define-struct game-object
  (position velocity orientation rotational-velocity)
  #:methods
  ;; update!: game-object -> void
  ;; Update object for the next frame.
  ;; This is a default method; it will be used by any subtypes that don't
  ;; define their own update! method.
  (define (update! me)
    ;; Do nothing
    (void))
  
  ;; destroy!: game-object -> void
  ;; Destroys the game object
  ;; This is a default method; it will be used by any subtypes that don't
  ;; define their own destroy! method.
  (define (destroy! me)
    (set! all-game-objects
          (remove me all-game-objects)))
  
  ;; render: game-object -> image
  ;; Draws the game-object.
  ;; There is no default method for render, since there is no default
  ;; appearance for objects. You must fill in a render method for your
  ;; subclass.
  
  ;; radius: game-object -> number
  ;; Size of the game object for purposes of detecting collisions.
  ;; There is no default method for radius, since there's no default
  ;; size for objects. You must fill in a radius method for your
  ;; subclass.
  )

(check-satisfied update! procedure?)
(check-satisfied destroy! procedure?)

;; This is the type for the player's ship.
;; There will always be exactly one of these, and it will be stored
;; in the global variable the-player.
(define-struct (player game-object)
  () ; no special properties
  #:methods
  ;; FILL IN THE FOLLOWING METHODS
  
  ;; update!: player -> void
  ;; Accelerate if the engines are firing.
  (define (update! p)
    (if firing-engines?
        (set-game-object-velocity! p
                                   (posn-+ (game-object-velocity p)
                                           (make-posn
                                            (* (/ 3 2) (posn-x (forward-direction p)))
                                            (* (/ 3 2) (posn-y (forward-direction p))))))
        (void)))
  
  ;; render: player -> image
  ;; Draw the player's ship
  (define (render p)
    (isosceles-triangle 32
                        16
                        "solid"
                        "white"))
  
  ;; radius: player -> number
  ;; Size of the object (for collision detection)
  (define (radius p)
    16)
  )

(check-satisfied
 (make-player (make-posn 400 300)
              (make-posn 0 0)
              0
              0)
 game-object?)
(check-satisfied render procedure?)
(check-satisfied radius procedure?)

;; This is the type for the asteroids.
;; Asteroids come in different sizes, so they have a radius
;; field in addition to their color field.
(define-struct (asteroid game-object)
  (radius color)
  #:methods
  ;; FILL THESE IN
  
  ;; render: asteroid -> image
  ;; Draw the asteroid
  (define (render a)
    (circle (asteroid-radius a)
            "solid"
            (asteroid-color a)))
  
  ;; radius: asteroid -> number
  ;; Size of the asteroid
  (define (radius a)
    (asteroid-radius a))
  )

(check-satisfied
 (make-asteroid (make-posn (random 800) (random 600))
                (random-velocity)
                0
                0
                (random-float 10 30)
                (random-color))
 game-object?)

;; This is the type for normal missiles.
(define-struct (missile game-object)
  (lifetime)
  #:methods
  ;; FILL THESE IN
  
  ;; update!: missile -> void
  ;; Decrement missile lifetime and destroy if necessary.
  (define (update! m)
    (if (= (missile-lifetime m) 0)
        (destroy! m)
        (set-missile-lifetime! m (- (missile-lifetime m) 1))))
  
  ;; render: missile -> image
  ;; Draw the missile
  (define (render m)
    (circle 2
            "solid"
            "white"))
  
  ;; radius: missile -> number
  ;; Size of the missile
  (define (radius m)
    2)
  )

(check-satisfied
 (make-missile (make-posn 420 350)
               (make-posn 5 3)
               0
               0
               100)
 game-object?)

;;
;; HEAT SEEKER MISSILE HERE
;;
(define-struct (heat-seeker missile) ;subtype of missile
  () ;no extra fields
  #:methods
  
  (define (update! hs-m)
    (if (= (missile-lifetime hs-m) 0)
        (destroy! hs-m)
        (begin (if (equal? (closest-asteroid-to hs-m) false) ;(closest-asteroid-to hs-m) is not boolean
                   (void)
                   (set-game-object-velocity! hs-m
                                              (posn-+ (game-object-velocity hs-m)
                                                      (make-posn
                                                       (* (/ 5 2) (posn-x (heading-of (closest-asteroid-to hs-m) hs-m)))
                                                       (* (/ 5 2) (posn-y (heading-of (closest-asteroid-to hs-m) hs-m)))))))
               (set-missile-lifetime! hs-m (- (missile-lifetime hs-m) 1))))) ;similar to player's position
  
  (define (render hs-m)
    (circle 2
            "solid"
            "red"))
  
  (define (radius hs-m)
    2)
  )

(check-satisfied make-heat-seeker procedure?)
(check-satisfied
 (make-heat-seeker (make-posn 420 350)
                   (make-posn 5 3)
                   0
                   0
                   100)
 missile?)

;;
;; UFO HERE
;;
(define-struct (ufo game-object) ;subtype of game-object
  () ;no extra fields
  #:methods
  
  (define (update! u)
    (set-game-object-velocity! u
                               (posn-* (/ 1 3)
                                       (posn-- (game-object-position the-player)
                                               (game-object-position u)))))

  (define (render u)
    (star 25
          "solid"
          "red"))
  
  (define (radius u)
    20)

  (define (destroy! u)
    (set-game-object-position! u
                               (make-posn 100
                                          100)))
  )

(check-satisfied make-ufo procedure?)
(check-satisfied
 (make-ufo (make-posn 400 300)
           (make-posn 0 0)
           0
           0)
 game-object?)

;;;
;;; Don't modify the code below
;;;

;;; Tracking game objects
(define all-game-objects '())
(check-satisfied all-game-objects list?)

;;; Main asteroids game
(define (asteroids)
  (link-and-start-asteroids-game))
