;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname homework5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Anna Yang, Christopher Swagler, Ian Agudelo
;;Homework 5

;;Exercise 1
;;part a
;;a Material is one of
;;- "silver"
;;- "gold"
;;- "pewter"
;;the Material of a charm bracelet can either be silver, gold, or pewter
(define ex-silver "silver")
(define ex-gold "gold")
(define ex-pewter "pewter")
(define (material-temp mat)
  (cond
    ((string=? "silver" mat) ...)
    ((string=? "gold" mat) ...)
    ((string=? "pewter" mat) ...)))

(define-struct charm [description material rest-charm-bracelet])
;;a CharmBracelet is one of
;;- "clasp"
;;- (make-charm String Material CharmBracelet)
;;INTERPRETATION: a CharmBracelet is either a "clasp" representing the terminal charm
;;or a (make-charm description material rest-charm-bracelet) with description being
;;a String representing the ornamental figure, material of the charm is either "silver"
;;"gold" or "pewter," and rest-charm-bracelet after the charm

(define ex-clasp "clasp")
(define ex-blueberry-charm (make-charm "blueberry" ex-silver ex-clasp))
(define ex-cat-charm (make-charm "cat" ex-gold ex-blueberry-charm))
(define ex-flower-charm (make-charm "flower" ex-pewter ex-cat-charm))
(define ex-unicorn-charm (make-charm "unicorn" ex-silver ex-flower-charm))
(define ex-double-heart-charm (make-charm "double heart" ex-gold ex-unicorn-charm))
(define ex-skull-charm (make-charm "skull" ex-pewter ex-double-heart-charm))

(define (charm-temp ch)
  (cond
    ((string? ch) ...)
    ((charm? ch)
     (... (charm-description ch) ...
          (material-temp (charm-material ch)) ...
          (charm-temp (charm-rest-charm-bracelet ch)) ...))))

;;part b
;;bracelet-cost : CharmBracelet -> Natural
;;given a CharmBracelet, a price as a Natural number is returned, specifically $15 for each
;;gold charm, $12 for each silver charm, $10 for each pewter charm, and $0 for the clasp
(check-expect (bracelet-cost ex-clasp) 0)
(check-expect (bracelet-cost ex-blueberry-charm) 12)
(check-expect (bracelet-cost ex-cat-charm) 27)
(check-expect (bracelet-cost ex-flower-charm) 37)
(check-expect (bracelet-cost ex-unicorn-charm) 49)
(check-expect (bracelet-cost ex-double-heart-charm) 64)
(check-expect (bracelet-cost ex-skull-charm) 74)
(define (bracelet-cost ch)
  (cond
    ((string? ch) 0)
    ((charm? ch)
     (+ (material-price (charm-material ch))
        (bracelet-cost (charm-rest-charm-bracelet ch))))))

;;material-price : Material -> Natural
;;given a Material of a charm, returns the price of that material as either $15 for a
;;gold charm, $12 for a silver charm, $10 for a pewter charm
(check-expect (material-price ex-silver) 12)
(check-expect (material-price ex-gold) 15)
(check-expect (material-price ex-pewter) 10)
(define (material-price mat)
  (cond
    ((string=? "silver" mat) 12)
    ((string=? "gold" mat) 15)
    ((string=? "pewter" mat) 10)))

;;Exercise 2
;;part a
(define-struct colored-bead [color size rest-bracelet])
;;a FancyBracelet is one of
;;- CharmBracelet
;;- (make-colored-bead String Natural FancyBracelet)
;;INTERPRETATION: a FancyBracelet is either a CharmBracelet which is either a "clasp" representing
;;the terminal charm or a (make-charm description material rest-charm-bracelet) with description being
;;a String representing the ornamental figure, material of the charm is either "silver"
;;"gold" or "pewter," and rest-charm-bracelet after the charm;
;;or a (make-colored-bead color size rest-bracelet), where color is a String, size is a Natural,
;;and rest-bracelet after the bead

(define ex-fancy-bracelet1 ex-clasp)
(define ex-fancy-bracelet2 ex-blueberry-charm)
(define ex-fancy-bracelet3 ex-cat-charm)
(define ex-fancy-bracelet4 ex-flower-charm)
(define ex-fancy-bracelet5 ex-unicorn-charm)
(define ex-fancy-bracelet6 ex-double-heart-charm)
(define ex-fancy-bracelet7 ex-skull-charm)
(define ex-fancy-bracelet8 (make-colored-bead "blue" 5 ex-fancy-bracelet1))
(define ex-fancy-bracelet9 (make-colored-bead "red" 10 ex-fancy-bracelet7))

(define (fancy-temp f)
  (cond
    ((or (string? f) (charm? f)) (charm-temp f))
    ((colored-bead? f)
     (... (colored-bead-color f) ...
          (colored-bead-size f) ...
          (fancy-temp (colored-bead-rest-bracelet f)) ...))))

;;part b
;;count-charms : FancyBracelet -> Natural
;;given a FancyBracelet, returns the number of charms (and not beads) as a natural number

(check-expect (count-charms ex-fancy-bracelet1) 0)
(check-expect (count-charms ex-fancy-bracelet2) 1)
(check-expect (count-charms ex-fancy-bracelet3) 2)
(check-expect (count-charms ex-fancy-bracelet4) 3)
(check-expect (count-charms ex-fancy-bracelet5) 4)
(check-expect (count-charms ex-fancy-bracelet6) 5)
(check-expect (count-charms ex-fancy-bracelet7) 6)
(check-expect (count-charms ex-fancy-bracelet8) 0)
(check-expect (count-charms ex-fancy-bracelet9) 6)
(define (count-charms f)
  (cond
    ((or (string? f) (charm? f)) (charm->number f))
    ((colored-bead? f)
     (count-charms (colored-bead-rest-bracelet f)))))

;;charm->number : CharmBracelet -> Natural
;;given a CharmBracelet, returns the number of charms as a natural
(check-expect (charm->number ex-clasp) 0)
(check-expect (charm->number ex-blueberry-charm) 1)
(check-expect (charm->number ex-cat-charm) 2)
(check-expect (charm->number ex-flower-charm) 3)
(check-expect (charm->number ex-unicorn-charm) 4)
(check-expect (charm->number ex-double-heart-charm) 5)
(check-expect (charm->number ex-skull-charm) 6)
(define (charm->number ch)
  (cond
    ((string? ch) 0)
    ((charm? ch)
     (add1 (charm->number (charm-rest-charm-bracelet ch))))))

;;part c
;;upgrade-bracelet : FancyBracelet String String -> FancyBracelet
;;given a FancyBracelet, a bead color as a string, and a requested figure as a string
;;returns a FancyBracelet where beads of given bead color are replaced with charms of the
;;requested figure in the Material silver
(check-expect (upgrade-bracelet ex-fancy-bracelet1 "green" "blueberry") ex-fancy-bracelet1)
(check-expect (upgrade-bracelet ex-fancy-bracelet2 "red" "unicorn") ex-fancy-bracelet2)
(check-expect (upgrade-bracelet ex-fancy-bracelet8 "blue" "unicorn")
              (make-charm "unicorn" ex-silver ex-clasp))
(check-expect (upgrade-bracelet ex-fancy-bracelet9 "red" "flower")
              (make-charm "flower" ex-silver ex-fancy-bracelet7))
(check-expect (upgrade-bracelet ex-fancy-bracelet9 "blue" "flower") ex-fancy-bracelet9)
(define (upgrade-bracelet f col fig)
  (cond
    ((or (string? f) (charm? f)) f)
    ((and (colored-bead? f) (string=? (colored-bead-color f) col))
     (make-charm fig ex-silver (upgrade-bracelet (colored-bead-rest-bracelet f) col fig)))
    (else f)))

;;Exercise 3
(define-struct student [firstname lastname gpa on-coop])
; A Student is a (make-student String String Number Boolean)
; Interpretation: A (make-student fn ln g c) represents a
; Northeastern student whose first name is fn and last name is ln, with 
; cumulative grade point average g, and for whom c is #true if they are
; currently doing a coop experience this term and #false otherwise.
(define student1 (make-student "Jane" "Smith" 4.0 #true))
(define student2 (make-student "Ashok" "Singhal" 0.0 #false))
(define (student-templ st)
  (... (student-firstname st) ...
       (student-lastname st) ...
       (student-gpa st) ...
       (student-on-coop st) ...))

;;part a
;;a ListOfStudents is one of
;;- '()
;;- (cons student ListOfStudents)
;;INTERPRETATION: a ListOfStudents is either empty or a cons of students
(define (LOS-temp los)
  (cond
    ((empty? los) ...)
    ((cons? los) (... (student-templ (first los)) ... (LOS-temp (rest los)) ...))))
(define ex-los1 '())
(define ex-los2 (cons student1 ex-los1))
(define ex-los3 (cons student2 ex-los2))

;;part b
;;count-coop-students : ListOfStudents -> Natural
;;given a ListOfStudents, returns the number of students who are on coop
(check-expect (count-coop-students ex-los1) 0)
(check-expect (count-coop-students ex-los2) 1)
(check-expect (count-coop-students ex-los3) 1)
(define (count-coop-students los)
  (cond
    ((empty? los) 0)
    ((cons? los) (+ (check-coop (first los)) (count-coop-students (rest los))))))

;;check-coop : student -> Natural
;;given a student, returns 1 if they're on coop and 0 if not
(check-expect (check-coop student1) 1)
(check-expect (check-coop student2) 0)
(define (check-coop st)
  (if (student-on-coop st) 1 0))

;;part c
;;exchange-coop-students : ListOfStudents -> ListOfStudents
;;given a ListOfStudents, returns the ListOfStudents with each student's coop status flipped
(check-expect (exchange-coop-students ex-los1) ex-los1)
(check-expect (exchange-coop-students ex-los2)
              (cons (make-student "Jane" "Smith" 4.0 #false) ex-los1))
(check-expect (exchange-coop-students ex-los3)
              (cons (make-student "Ashok" "Singhal" 0.0 #true)
                    (cons (make-student "Jane" "Smith" 4.0 #false) ex-los1)))
(define (exchange-coop-students los)
  (cond
    ((empty? los) '())
    ((cons? los) (cons (flip-coop (first los)) (exchange-coop-students (rest los))))))

;;flip-coop : student -> student
;;given a student, flips their coop status and returns the student
(check-expect (flip-coop student1) (make-student "Jane" "Smith" 4.0 #false))
(check-expect (flip-coop student2) (make-student "Ashok" "Singhal" 0.0 #true))
(define (flip-coop st)
  (make-student (student-firstname st)
                (student-lastname st)
                (student-gpa st)
                (not (student-on-coop st))))
