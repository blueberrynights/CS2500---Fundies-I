;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Homework11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Anna Yang & Sarah Tong
;; Homework 11

;; Exercise 1:
; list-prefix? : [List-of Numbers] [List-of Numbers] -> Boolean
; given 2 [List-of Numbers], determines if the first list if a prefix of the second list
; where all elements in first list appear in order as the first n elements of the second list
; and if both [List-of Numbers] are empty, returns false because there are no elements appearing

(define ex-lon1 (list 1 2 3 4 5))
(define ex-lon2 (list 1 2 3 4 5 6 7 8))
(define ex-lon3 (list -5 -7 2 -12))
(define ex-lon4 '())
(define ex-lon5 '())

(check-expect (list-prefix? ex-lon1 ex-lon2) #t)
(check-expect (list-prefix? ex-lon3 ex-lon2) #f)
(check-expect (list-prefix? ex-lon2 ex-lon1) #f)
(check-expect (list-prefix? ex-lon4 ex-lon1) #t)
(check-expect (list-prefix? ex-lon1 ex-lon4) #f)
(check-expect (list-prefix? ex-lon4 ex-lon5) #t)

(define (list-prefix? lon1 lon2)
  (cond
    [(empty? lon1) #t]
    [(and (cons? lon1) (empty? lon2)) #f]
    [(and (cons? lon1) (cons? lon2)) (and (= (first lon1) (first lon2))
                                          (list-prefix?  (rest lon1) (rest lon2)))]))

;; Exercise 2:
; max-splice: [List-of Numbers] [List-of Numbers] -> [List-of Numbers]
; given 2 [List-of Numbers], creates the shortest resulting list that begins with all elements of
; first list in order and ends with all elements in second list in order while allowing overlaps
; to shorten the result
(check-expect (max-splice '(1 2 3 4) '(2 3 4 5)) '(1 2 3 4 5))
(check-expect (max-splice '() '(2 3 4 5)) '(2 3 4 5))
(check-expect (max-splice '(1 2 3 4) '()) '(1 2 3 4))
(check-expect (max-splice '() '()) '())
(check-expect (max-splice '(1 2 3 4) '(2 2 3 4 5)) '(1 2 3 4 2 2 3 4 5))
(check-expect (max-splice '(1 2) '(1 2 3)) '(1 2 3))

(define (max-splice lon1 lon2)
  (cond
    [(and (empty? lon1) (empty? lon2)) empty]
    [(and (empty? lon1) (cons? lon2)) lon2]
    [(and (cons? lon1) (empty? lon2)) lon1]
    [(and (cons? lon1) (cons? lon2))
     (if (list-prefix? lon1 lon2) lon2 (if (list-prefix? (rest lon1) (rest lon2))
                                           (append lon1 lon2)
                                           (cons (first lon1) lon2)))]))

;; Exercise 3:
; valid-results?: [List-of X] [List-of Functions] [List-of X] -> Boolean
; given 3 lists of equal length, for each member i of the first list, applies function at the
; corresponding position in the second list to i and compares output with the result of the
; corresponding position in the third list, if all lists are empty, returns true
(define ex-inputlist1 (list 0 1 2))
(define ex-inputlist2 '())
(define ex-functionlist1 (list add1 add1 add1))
(define ex-functionlist2 (list sub1 sub1 sub1))
(define ex-functionlist3 '())
(define ex-returnlist1 (list -1 0 1))
(define ex-returnlist2 (list 1 2 3))
(define ex-returnlist3 '())

(check-expect (valid-results? ex-inputlist1 ex-functionlist1 ex-returnlist1) #f)
(check-expect (valid-results? ex-inputlist1 ex-functionlist1 ex-returnlist2) #t)
(check-expect (valid-results? ex-inputlist1 ex-functionlist2 ex-returnlist1) #t)
(check-expect (valid-results? ex-inputlist2 ex-functionlist3 ex-returnlist3) #t)

(define (valid-results? lox1 lof lox2)
  (cond
    [(and (empty? lox1) (empty? lof) (empty? lox2)) #t]
    [(and (cons? lox1) (cons? lof) (cons? lox2))
     (and (= ((first lof) (first lox1)) (first lox2))
          (valid-results? (rest lox1) (rest lof) (rest lox2)))]))

;; Exercise 4:
(define-struct assignment (role person))
; An Assignment is a (make-assignment Symbol [Maybe String])
; INTERPRETATION: an assignment represents the assignments a certain person can have to a role
(define ex-assignment1 (make-assignment 'A "Bob"))
(define ex-assignment2 (make-assignment 'B "Job"))
(define ex-assignment3 (make-assignment 'B #f))
(define (assignment-temp a)
  (... (assignment-role a) ...
       (assignment-person a) ...))

; A [Maybe String] is one of:
; - #f
; - String

; assign: [List-of Role] [List-of Person] -> [List-of Assignment]
; matches up members of a [List-of Symbols] with members of a [List-of Strings] and returns a
; [List-of Assignment] where if the [List-of Strings] is shorter then unpaired Symbols are paired
; with #f and if the [List-of Strings] is longer, extra Strings are ignored

(define ex-lor1 (list 'A 'B 'C))
(define ex-lop1 (list "Bob" "Job" "Hob"))
(define ex-lop2 (list "Bob" "Job" "Hob" "Yob"))
(define ex-lop3 (list "Yob"))
(define ex-lor '())
(define ex-lop '())

(define (listofrole-temp lor)
  (cond
    [(empty? lor) ...]
    [(cons? lor) (... (first lor) ...
                      (rest lor) ...)]))

(define (listofperson-temp lop)
  (cond
    [(empty? lop) ...]
    [(cons? lop) (... (first lop) ...
                      (rest lop) ...)]))

(define (listofassign-temp loa)
  (cond
    [(empty? loa) ...]
    [(cons? loa) (... (first loa) ...
                      (rest loa) ...)]))

(check-expect (assign ex-lor ex-lop) '())
(check-expect (assign ex-lor1 ex-lop1) (list ex-assignment1
                                             ex-assignment2
                                             (make-assignment 'C "Hob")))
(check-expect (assign ex-lor1 ex-lop2) (list ex-assignment1
                                             ex-assignment2
                                             (make-assignment 'C "Hob")))
(check-expect (assign ex-lor1 ex-lop3) (list (make-assignment 'A "Yob")
                                             ex-assignment3
                                             (make-assignment 'C #f)))

(define (assign lor lop)
  (cond
    [(empty? lor) '()]
    [(and (cons? lor) (empty? lop)) (cons (make-assignment (first lor) #f)
                                          (assign (rest lor) lop))]
    [(and (cons? lor) (cons? lop)) (cons (make-assignment (first lor) (first lop))
                                         (assign (rest lor) (rest lop)))]))

;; Exercise 5:
(define-struct bt [value left right])
; A BT (Binary Tree) is one of:
; - 'none
; - (make-bt Symbol BT BT)
(define (bt-temp bt)
  (cond
    [(symbol? bt) ...]
    [(bt? bt) (... (bt-value bt) ...
                   (bt-temp (bt-left bt)) ...
                   (bt-temp (bt-right bt)) ...)]))

; tree-equiv: BT BT -> Boolean
; given 2 Binary Trees, determines if they are equivalent where additionally, if each contains
; subtrees equivalent to one of the subtrees in the corresponding tree the binary trees are
; considered equivalent
(define ex-bt1 'none)
(define ex-bt10 'none)
(define ex-bt2 (make-bt 'd ex-bt1 ex-bt1))
(define ex-bt3 (make-bt 'e ex-bt1 ex-bt1))
(define ex-bt4 (make-bt 'b ex-bt2 ex-bt3))
(define ex-bt5 (make-bt 'f ex-bt1 ex-bt1))
(define ex-bt6 (make-bt 'g ex-bt1 ex-bt1))
(define ex-bt7 (make-bt 'c ex-bt5 ex-bt6))
(define ex-bt8 (make-bt 'a ex-bt4 ex-bt7))
(define ex-bt9 (make-bt 'a ex-bt7 ex-bt4))

(check-expect (tree-equiv ex-bt2 ex-bt1) #f)
(check-expect (tree-equiv ex-bt1 ex-bt2) #f)
(check-expect (tree-equiv ex-bt1 ex-bt10) #t)
(check-expect (tree-equiv ex-bt4 ex-bt7) #f)
(check-expect (tree-equiv ex-bt8 ex-bt9) #t)
(check-expect (tree-equiv ex-bt8 ex-bt1) #f)

(define (tree-equiv bt1 bt2)
  (cond
    [(and (symbol? bt1) (symbol? bt2)) #t]
    [(and (symbol? bt1) (bt? bt2)) #f]
    [(and (bt? bt1) (symbol? bt2)) #f]
    [(and (bt? bt1) (bt? bt2)) (and (symbol=? (bt-value bt1) (bt-value bt2))
                                    (or (and (tree-equiv (bt-left bt1) (bt-left bt2))
                                             (tree-equiv (bt-right bt1) (bt-right bt2)))
                                        (and (tree-equiv (bt-left bt1) (bt-right bt2))
                                             (tree-equiv (bt-right bt1) (bt-left bt2)))))]))

;; Exercise 6:
; tree-equiv.v2: BT BT -> Boolean
; given 2 Binary Trees, determines if they are equivalent where additionally, if each contains
; subtrees equivalent to one of the subtrees in the corresponding tree the binary trees are
; considered equivalent, however this one is not flippable  as opposed to the function for
; exercise 5
(check-expect (tree-equiv.v2 ex-bt2 ex-bt1) #f)
(check-expect (tree-equiv.v2 ex-bt1 ex-bt2) #f)
(check-expect (tree-equiv.v2 ex-bt1 ex-bt10) #t)
(check-expect (tree-equiv.v2 ex-bt4 ex-bt7) #f)
(check-expect (tree-equiv.v2 ex-bt8 ex-bt9) #f)
(check-expect (tree-equiv.v2 ex-bt8 ex-bt1) #f)
(define (tree-equiv.v2 bt1 bt2)
  (cond
    [(and (symbol? bt1) (symbol? bt2)) #t]
    [(and (symbol? bt1) (bt? bt2)) #f]
    [(and (bt? bt1) (symbol? bt2)) #f]
    [(and (bt? bt1) (bt? bt2)) (and (symbol=? (bt-value bt1) (bt-value bt2))
                                    (tree-equiv.v2 (bt-left bt1) (bt-left bt2))
                                    (tree-equiv.v2 (bt-right bt1) (bt-right bt2)))]))

; find-subtree: BT BT -> Boolean
; given a Binary Tree and a subtree which is also a Binary Tree, determines if there is a matching
; subtree in the Binary tree to the given subtree, in this function the Binary Tree is not flippable
(check-expect (find-subtree ex-bt1 ex-bt10) #t)
(check-expect (find-subtree ex-bt2 ex-bt10) #t)
(check-expect (find-subtree ex-bt8 ex-bt4) #t)
(check-expect (find-subtree ex-bt9 ex-bt4) #t)
(check-expect (find-subtree ex-bt4 ex-bt8) #f)
(check-expect (find-subtree ex-bt4 ex-bt7) #f)
(check-expect (find-subtree ex-bt8 ex-bt9) #f)

(define (find-subtree bt1 bt2)
  (cond
    [(symbol? bt2) #t]
    [(and (symbol? bt1) (bt? bt2)) #f]
    [(and (bt? bt1) (bt? bt2)) (or (tree-equiv.v2 bt1 bt2)
                                   (find-subtree (bt-left bt1) bt2)
                                   (find-subtree (bt-right bt1) bt2))]))

;; Exercise 7:
; max-common-tree: BT BT -> BT
; given 2 Binary Trees, compares them starting from the root of each and returning a tree that shares
; the maximum number of nodes with the 2 given Binary Trees
(define ex-bt11 (make-bt 'y ex-bt1 ex-bt1))
(define ex-bt12 (make-bt 'b ex-bt2 ex-bt10))
(define ex-bt13 (make-bt 'x ex-bt5 ex-bt1))
(define ex-bt14 (make-bt 'a ex-bt11 ex-bt12))
  
(check-expect (max-common-tree ex-bt9 ex-bt14) (make-bt
                                                'a 'none (make-bt 'b
                                                                  (make-bt 'd 'none 'none) 'none)))
(check-expect (max-common-tree ex-bt8 ex-bt4) 'none)
(check-expect (max-common-tree ex-bt8 ex-bt9) (make-bt 'a 'none 'none))
(check-expect (max-common-tree ex-bt1 ex-bt10) 'none)
(check-expect (max-common-tree ex-bt1 ex-bt4) 'none)

(define (max-common-tree bt1 bt2)
  (cond
    [(or (symbol? bt1) (symbol? bt2)) 'none]
    [(and (bt? bt1) (bt? bt2)) (if (symbol=? (bt-value bt1) (bt-value bt2))
                                   (make-bt (bt-value bt1)
                                            (max-common-tree (bt-left bt1) (bt-left bt2))
                                            (max-common-tree (bt-right bt1) (bt-right bt2)))
                                   'none)]))

;; Exercise 8:
; A Dir is one of:
; - 'left
; - 'right
(define (dir-temp d)
  (cond
    [(symbol=? 'left d) ...]
    [(symbol=? 'right d) ...]))
(define dir1 'left)
(define dir2 'right)

; A [List-of Dir] is one of:
; - empty
; - (cons Dir [List-of Dir])
(define (listofdir-temp lod)
  (cond
    [(empty? lod) ...]
    [(cons? lod) (... (dir-temp (first lod))
                      (listofdir-temp (rest lod)) ...)]))

(define ex-dir0 empty)
(define ex-dir1 (list dir1))
(define ex-dir2 (list dir2))
(define ex-dir3 (list dir1 dir2))
(define ex-dir4 (list 'right 'right 'left))

(define-struct leaf ())
(define-struct node [key info left right])
;; A BinarySearchTree (BST) is one of: 
;; -- (make-leaf)
;; -- (make-node Natural String BST BST)
(define (bst-temp bst)
  (cond
    [(leaf? bst) ...]
    [(node? bst) (... (node-key bst) ...
                      (node-info bst) ...
                      (bt-temp (bt-left bst)) ...
                      (bt-temp (bt-right bst)) ...)]))

(define LEAF (make-leaf))
(define s1 (make-node 10 "a" LEAF (make-node 14 "B" LEAF LEAF)))
(define s2
  (make-node 10 "a" 
             (make-node 8 "b" 
                        (make-node 7 "c" LEAF LEAF)
                        LEAF) 
             (make-node 11 "d" 
                        LEAF
                        (make-node 13 "e" 
                                   (make-node 12 "f" LEAF LEAF) 
                                   LEAF))))

;; valid-bst-path? : BST Num [List-of Dir] -> Boolean
;; validates every left/right decision in the given path, as well as the path leads to the correct
;; given node value in the given BST
(check-expect (valid-bst-path? LEAF 0 ex-dir0) #false)
(check-expect (valid-bst-path? LEAF 0 ex-dir1) #false)
(check-expect (valid-bst-path? s1 9 ex-dir0) #false)
(check-expect (valid-bst-path? s1 10 ex-dir0) #true)
(check-expect (valid-bst-path? s1 10 ex-dir1) #false)
(check-expect (valid-bst-path? s2 12 ex-dir4) #true)

(define (valid-bst-path? bst num lod)
  (cond [(leaf? bst) #false]
        [(and (node? bst) (empty? lod)) (= (node-key bst) num)]
        [(and (node? bst) (cons? lod))
         (cond [(symbol=? 'left (first lod))
                (valid-bst-path? (node-left bst) num (rest lod))]
               [(symbol=? 'right (first lod))
                (valid-bst-path? (node-right bst) num (rest lod))])]))

;; Exercise 9
;; merge : {X} [List-of X] [List-of X] [X X -> Boolean] -> [List-of X]
;; produces a single ordered list from 2 ordered lists containing elements of the same type
(check-expect (merge empty empty <) empty)
(check-expect (merge empty (list 1 2 3) <)
              (list 1 2 3))
(check-expect (merge (list "a" "b" "c") empty string<?)
              (list "a" "b" "c")) 
(check-expect (merge (list 2 4 6 8) (list 1 3 5 7) <)
              (list 1 2 3 4 5 6 7 8))
(check-expect (merge (list 2 3 4 8) (list 1 5 6 7) <)
              (list 1 2 3 4 5 6 7 8))
(check-expect (merge (list "a" "b" "f") (list "c" "d" "e") string<?)
              (list "a" "b" "c" "d" "e" "f"))

(define (merge lox1 lox2 lt-func)
  (cond [(and (empty? lox1) (empty? lox2)) empty]
        [(and (empty? lox1) (cons? lox2)) lox2]
        [(and (cons? lox1) (empty? lox2)) lox1]
        [(and (cons? lox1) (cons? lox2)) (if (lt-func (first lox1) (first lox2))
                                             (cons (first lox1) (merge (rest lox1) lox2 lt-func))
                                             (cons (first lox2) (merge lox1 (rest lox2) lt-func)))]))

