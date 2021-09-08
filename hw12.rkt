;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Homework12 (3)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Anna Yang & Sarah Tong
;; Homework 12

;; EXERCISE 1:
;; An ElGraph is a [List-of Node]

;; A Edge is a (make-edge Direction Name)
(define-struct edge [dir end])
;; - where dir is the Direction of the edge
;; - and end is the name of the ending node that the edge points to

;; A Node is a (make-node Name [List-of Edge])
(define-struct node [name paths])
;; - where name is the unique identifier for the given node
;; - and paths is a list of directions of edges this one is connected to

;; Edge EXAMPLES
(define ex-edge-east (make-edge "east" "B"))
(define ex-edge-north (make-edge "north" "A"))
(define ex-edge-northeast (make-edge "northeast" "B"))

;; Node EXAMPLES
(define ex-node-a (make-node "A" (list ex-edge-east)))
(define ex-node-b (make-node "B" (list empty)))
(define ex-node-c (make-node "C" (list ex-edge-north ex-edge-northeast)))
(define ex-node-x (make-node "X" (list (make-edge "east" "Z")
                                       (make-edge "southeast" "Y"))))
(define ex-node-z (make-node "Z" (list (make-edge "south" "Y")
                                       (make-edge "west" "X"))))
(define ex-node-y (make-node "Y" (list (make-edge "northwest" "Y")
                                       (make-edge "north" "Z"))))

;; ELGraph EXAMPLES
(define ex-elgraph1 (list ex-node-a ex-node-b ex-node-c))
(define ex-elgraph2 (list ex-node-x ex-node-z ex-node-y))
(define ex-elgraph3 empty)

;; A Name is String

;; A Direction is one of:
;; - "north"
;; - "south"
;; - "east"
;; - "west"
;; - "northeast"
;; - "northwest"
;; - "southeast"
;; - "southwest"
;; INTERPRETATION: a direction is any of the 8 cardinal directions

;; EXERCISE 2
(define GS+SS (make-node "Gainsborough and St Stephens"
                         (list (make-edge "west" "Gainsborough and Hemenway")
                               (make-edge "southwest" "Opera and St Stephen"))))
(define GS+HS (make-node "Gainsborough and Hemenway"
                         (list (make-edge "southwest" "Forsyth St and Hemenway")
                               (make-edge "east" "Gainsborough and St Stephens"))))
(define FS+HS (make-node "Forsyth St and Hemenway"
                         (list (make-edge "southwest" "Forsyth Way and Hemenway")
                               (make-edge "northeast" "Gainsborough and Hemenway")
                               (make-edge "southeast" "Forsyth St and Huntington"))))
(define FW+HS (make-node "Forsyth Way and Hemenway"
                         (list (make-edge "southeast" "Forsyth Way and Huntington"))))
(define FW+HA (make-node "Forsyth Way and Huntington"
                         (list (make-edge "northwest" "Forsyth Way and Hemenway")
                               (make-edge "northeast" "Forsyth St and Huntington"))))
(define FS+HA (make-node "Forsyth St and Huntington"
                         (list (make-edge "northwest" "Forsyth St and Hemenway")
                               (make-edge "southwest" "Forsyth Way and Huntington")
                               (make-edge "northeast" "Huntington and Opera"))))
(define HA+OP (make-node "Huntington and Opera"
                         (list (make-edge "southwest" "Forsyth St and Huntington")
                               (make-edge "northwest" "Opera and St Stephens"))))
(define OP+SS (make-node "Opera and St Stephens"
                         (list (make-edge "southeast" "Huntington and Opera")
                               (make-edge "northeast" "Gainsborough and St Stephens"))))
(define given-street-graph (list GS+SS GS+HS FS+HS FW+HS FW+HA FS+HA HA+OP OP+SS))

;--------------------------------------------------------------------------------------
(define CS+CA (make-node "Clarendon and Columbus"
                         (list (make-edge "southwest" "Columbus and Dartmouth")
                               (make-edge "southeast" "Clarendon and Tremont"))))
(define CA+DS (make-node "Columbus and Dartmouth"
                         (list (make-edge "southwest" "Columbus and Mass")
                               (make-edge "southeast" "Dartmouth and Tremont")
                               (make-edge "northeast" "Clarendon and Columbus"))))
(define CA+MA (make-node "Columbus and Mass"
                         (list (make-edge "southeast" "Mass and Tremont")
                               (make-edge "northeast" "Columbus and Dartmouth"))))
(define MA+TS (make-node "Mass and Tremont"
                         (list (make-edge "northeast" "Dartmouth and Tremont")
                               (make-edge "northwest" "Columbus and Mass"))))
(define DS+TS (make-node "Dartmouth and Tremont"
                         (list (make-edge "northeast" "Clarendon and Tremont") 
                               (make-edge "northwest" "Columbus and Dartmouth")
                               (make-edge "southwest" "Mass and Tremont"))))
(define CS+TS (make-node "Clarendon and Tremont"
                         (list (make-edge "northwest" "Clarendon and Columbus")
                               (make-edge "southwest" "Dartmouth and Tremont"))))
(define my-street-graph (list CS+CA CA+DS CA+MA MA+TS DS+TS CS+TS))


;; EXERCISE 3
;; A [Maybe [List-of String]] is either:
;- #f
;- [List-of String]

;; driving-directions : ELGraph String String -> [Maybe [List-of String]]
;; given a (1) graph of streets and directions, (2) the starting point which is an intersection
; and (3) the destination which is another intersection, returns the directions going from
; one intersection to another on a given map

(check-expect (driving-directions given-street-graph
                                  "Opera and St Stephens"
                                  "Gainsborough and St Stephens")
              (list "northeast to Gainsborough and St Stephens"))
(check-expect (driving-directions given-street-graph
                                  "Gainsborough and Hemenway"
                                  "Forsyth Way and Huntington")
              (list
               "southwest to Forsyth St and Hemenway"
               "southwest to Forsyth Way and Hemenway"
               "southeast to Forsyth Way and Huntington"))
(check-expect (driving-directions my-street-graph
                                  "Dartmouth and Tremont"
                                  "Clarendon and Columbus")
              (list
               "northeast to Clarendon and Tremont"
               "northwest to Clarendon and Columbus"))
(check-expect (driving-directions given-street-graph
                                  "Opera and St Stephens"
                                  "blueberries")
              #f)
                                   
(define (driving-directions graph start end)
  (local [;; driving-directions/acc :
          ;; Graph String String [List-of Edge] [List-of String] -> [List-of String]
          ;; starting from start to end, produces list of edges
          ;; ACCUMULATOR: current-node is the current node being checked
          ;; ACCUMULATOR: seen-so-far is the nodes visited so far
          ;; ACCUMULATOR: current-path is the path we are checking
          (define (driving-directions/acc graph current-node end seen-so-far current-path)
            (local [(define loedgs (produce-loeds graph current-node))]
              (cond
                [(empty? loedgs) current-path]
                [(string=? current-node end) current-path] 
                [(member? end loedgs) (cons (return-dir graph current-node end) current-path)]
                [else
                 (local [(define next (first (filter (lambda (x) (path? graph x end seen-so-far))
                                                     loedgs)))]
                   (driving-directions/acc graph next end
                                           (cons current-node seen-so-far)
                                           (cons (return-dir graph current-node next)
                                                 current-path)))])))]
    (if (path? graph start end empty)
        (map (lambda (x) (string-append (edge-dir x) " to " (edge-end x)))
             (reverse (driving-directions/acc graph start end empty empty)))
        #f)))

;; return-dir : ELGraph String String -> Edge
;; given an ELGraph and a String and a String, returns the direction that the the first node
;; points to to get to the second node
(check-expect (return-dir given-street-graph "Opera and St Stephens" "Gainsborough and St Stephens")
              (make-edge
               "northeast"
               "Gainsborough and St Stephens"))
(define (return-dir graph start end)
  (first (filter (lambda (x) (string=? end (edge-end x))) (produce-edges graph start))))

;; path? : ELGraph String String [List-of String] -> Boolean
;; returns true if there is path between the 2 given Strings in the graph
(check-expect (path? given-street-graph "Forsyth St and Hemenway" "Gainsborough and St Stephens"
                     empty) #t)
(define (path? graph start end los)
  (local [;; path?/acc: ELGraph String String [List-of String] -> Boolean
          ;; determines if a path between the 2 Strings exists in the graph
          ;; ACCUMULATOR: seen-so-far is the nodes already checked
          (define (path?/acc graph current end seen-so-far)
            (local [(define loedgs (produce-loeds graph current))]
              (cond
                [(member? current seen-so-far) #f]
                [(member? end loedgs) #t]
                [else (ormap (lambda (x) (path?/acc graph x end (cons current seen-so-far)))
                             loedgs)])))]
    (path?/acc graph start end los)))
    
;; produce-loeds: ELGraph String -> [List-of String]
;; given an ELGraph and String, produces a list of Strings of nodes that are next to
;; the given String
(check-expect (produce-loeds given-street-graph "Forsyth St and Hemenway")
              (list "Forsyth Way and Hemenway"
                    "Gainsborough and Hemenway"
                    "Forsyth St and Huntington"))
(define (produce-loeds graph street)
  (map (lambda (y) (edge-end y))
       (node-paths (first (filter (lambda (x) (string=? street (node-name x))) graph)))))

;; produce-edges: ELGraph String -> [List-of Edges]
;; given an ELGraph and a edge which is as a String,
;; produces the edges of a node as a [List-of String]
(check-expect (produce-edges given-street-graph "Forsyth St and Hemenway")
              (list (make-edge "southwest" "Forsyth Way and Hemenway")
                    (make-edge "northeast" "Gainsborough and Hemenway")
                    (make-edge "southeast" "Forsyth St and Huntington")))     
(define (produce-edges graph street)
  (node-paths (first (filter (lambda (x) (string=? street (node-name x))) graph))))
  
;; EXERCISE 4:
;; fully-connected? : ELGraph -> Boolean
;; returns whether a path exists between all pairs of points in the given graph
(check-expect (fully-connected? ex-elgraph1) #false)
(check-expect (fully-connected? ex-elgraph2) #true)
(check-expect (fully-connected? given-street-graph) #true)
(define (fully-connected? graph)
  (andmap
   (lambda (x) (andmap (lambda (y)
                         (list? (driving-directions graph (node-name x) (node-name y)))) graph))
   graph))

;----------------------------------------------------------------------------------------------------
;; EXERCISE 5:
;;NOTE: BECAUSE NODE WAS ALREADY DEFINED IN EXERCISE 1, WE WILL CALL THIS ONE NODE.V2

(define-struct node.v2 [left value right])
(define-struct leaf [value])
; A [BT X Y] is one of:
; - (make-leaf Y)
; - (make-node.v2 [BT X Y] X [BT X Y])
; Interpretation: A [BT X Y] represents a binary tree with values _X_ at each
; node and values _Y_ at each leaf.
; Note: that a [BT X Y] may not be a binary search tree.

;       0
;     /    \
;    100   -3
;         /   \
;        5    100


;        1
;     /     \
;    2       3
;   / \     / \
;  4   5   6   7
;     / \
;    8   9


(define ex-bt1 (make-node.v2 (make-leaf 100) 0
                             (make-node.v2 (make-leaf 5) -3 (make-leaf 100))))
(define ex-bt2 (make-node.v2
                (make-node.v2 (make-leaf 4) 2 (make-node.v2 (make-leaf 8) 5 (make-leaf 9)))
                1
                (make-node.v2 (make-leaf 6) 3 (make-leaf 7))))
(define ex-bt3 (make-leaf 5))

; a [Maybe [List-of Number]] is one of:
; #f
; [List-of Number]
;;INTERPRETATION: a [Maybe [List-of Number]] can either be #false or a list of numbers

;; shortest-path-to-leaf: BT -> [List-of Number]
;; Given a Binary Tree, produces a list that represents the shortest path to a leaf,
; if there are multiple shortest paths, function produces any one of them
(check-expect (shortest-path-to-leaf ex-bt1) (list 0 100))
(check-expect (shortest-path-to-leaf ex-bt2) (list 1 3 7))
(check-expect (shortest-path-to-leaf ex-bt3) (list 5)) 
(define (shortest-path-to-leaf bintree)
  (local [;;shortest-path-to-leaf/acc: BT [Maybe [List-of Num]] [List-of Num] -> [List-of Num]
          ;; produces a list that represents the shortest path to a leaf
          ;; ACCUMULATOR: shortest path so far
          ;; ACCUMULATOR: current path that is being checked
          (define (shortest-path-to-leaf/acc bt shortest current)
            (cond
              [(leaf? bt) (cons (leaf-value bt) current)]
              [(node.v2? bt) (if (or
                                  (boolean? shortest)
                                  (<= (length shortest) (length (cons (node.v2-value bt) current))))
                                 (local [(define shortest-left-path
                                           (shortest-path-to-leaf/acc
                                            (node.v2-left bt) shortest
                                            (cons (node.v2-value bt) current)))
                                         (define shortest-right-path
                                           (shortest-path-to-leaf/acc
                                            (node.v2-right bt) shortest
                                            (cons (node.v2-value bt) current)))]
                                   (if (< (length shortest-left-path) (length shortest-right-path))
                                       shortest-left-path
                                       shortest-right-path))
                                 shortest)]))]
    (reverse (shortest-path-to-leaf/acc bintree #f '()))))

;; EXERCISE 6:
; A BST is a [BT Number String] that is constrained to be a binary search tree:
; At every _(make-node l v r)_ in a BST, all node-values in _l_ are less than
;  _v_ and all node-values in _r_ are greater than _v_.

;; is-bt-a-bst? : [BT Number String] -> Boolean
;; given a [BT Number String], checks if it is a BST
;; 2 accumulators: max value and min value

;        10
;     /     \
;    5       15
;   / \     / \
;  2   7   12   18


(define ex-bt4 (make-node.v2
                (make-node.v2 (make-leaf 2) 5 (make-leaf 7))
                10
                (make-node.v2 (make-leaf 12) 15 (make-leaf 18))))
(check-expect (is-bt-a-bst? ex-bt1) #f)
(check-expect (is-bt-a-bst? ex-bt2) #f)
(check-expect (is-bt-a-bst? ex-bt3) #t)
(check-expect (is-bt-a-bst? ex-bt4) #t)

(define (is-bt-a-bst? bt)
  (local [;; is-bt-a-bst?/acc: BT -> Boolean
          ;; given a [BT Number String], checks if it's a valid BST
          ;; ACCUMULATOR: tracks maximum value so far
          ;; ACCUMULATOR: tracks minimum value so far
          (define (is-bt-a-bst/acc bt minval maxval)
            (cond
              [(leaf? bt) #t]
              [(node.v2? bt)
               (and
                (and (< node.v2-value minval)
                     (> node.v2-value maxval)) 
                (is-bt-a-bst/acc (node.v2-left bt) (node.v2-value bt) (node.v2-value bt))
                (is-bt-a-bst/acc (node.v2-right bt) (node.v2-left bt) (node.v2-value bt)))]))]
    (is-bt-a-bst/acc bt +inf.0 -inf.0)))


