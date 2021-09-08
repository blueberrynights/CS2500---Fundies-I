;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname homework7) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; Anna Yang & Sarah Tong
;; Homework 7

; A [List-of X] is one of:
; - '()
; - (cons X [List-of X])
; 
; Interpretation: A list of items, where every item is an X.
 
; list-template : [List-of X] -> ?
(define (list-template l)
  (cond
    [(empty? l) ...]
    [(cons? l) (... (first l) ...
                    (list-template (rest l)) ...)]))

;; Exercise 2:
(define-struct tweet [text likes retweets])
; INTERPRETATION: a (make-tweet String Natural Natural) represents a twitter tweet where text
; is the text of the tweet as a String with a maximum of 280 characters, likes is the number of likes
; the tweet has as a Natural, and retweets is the number of retweets the twitter tweet has as a Nat
; tweet-temp: tweet -> ???

(define (tweet-temp t)
  (... (tweet-text t) ... (tweet-likes t) ... (tweet-retweets t) ...))

(define ex-tweet1 (make-tweet "strawberries" 0 0))
(define ex-tweet2 (make-tweet "blueberries" 10 1))
(define ex-tweet3 (make-tweet
                   "My castle crumbled overnight.
I brought a knife to a gunfight.
They took the crown but it's alright.
All the liars are calling me one,
nobody's heard from me for months,
I'm doing better than I ever was.
Cuz, my baby's fit like a daydream
walking with his head down I'm the one
he" 100 100)) ; example tweet that is 280 characters


(define-struct post [text likes])
; INTERPRETATION: a (make-post String Natural) represents a facebook post where text is the text of
; of the facebook post as a String, and likes the number of likes the facebook post has as a Natural
; post-temp: post -> ???
(define (post-temp p)
  (... (post-text p) ... (post-likes p) ...))

(define ex-post1 (make-post "coffee" 0))
(define ex-post2 (make-post "tea" 5))
(define ex-post3 (make-post
                  "Fever dream high in the quiet of the
night you know that I caught it. Bad, bad, boy shiny toy
with a price you know that I bought it. Killing me slow,
out the window, always waiting for you to be waiting below.
Devils roll the dice, angels roll their eyes, what doesn't
kill me mak" 100)) ; example post that is 281 characters
  
(define-struct story [text views])
; INTERPRETATION: a (make-story String Natural) represents a medium story where text is the text of
; the medium story as a String and views is the number of page views the medium story has a Natural
; story-temp: story -> ???
(define (story-temp s)
  (... (story-text s) ... (story-views s) ...))

(define ex-story1 (make-story "chocolate" 0))
(define ex-story2 (make-story "cat" 7))
(define ex-story3 (make-story
                   "I wonder if I'm being real.
Do I speak my truth or do I feel to how I feel.
I wonder wouldn't it be nice to live inside
a world that isn't black and white. I wonder what
it's like to be my friends, hope that they don't think
I'll forget about them. I wonder... I wonder... Right be" 100)) ; example story that is 281 characters

; a social-media-item is one of:
; - tweet
; - post
; - story
; INTERPRETATION: a social-media-item is either a (make-tweet String Natural Natural), a
; (make-post String Natural), or a (make-story String Natural)
; social-media-item-temp: social-media-item -> ???
(define (social-media-item-temp smi)
  (cond
    [(tweet? smi) (... (tweet-temp smi) ...)]
    [(post? smi) (... (post-temp smi) ...)]
    [(story? smi) (... (story-temp smi) ...)]))

(define ex-social-media-item1 ex-tweet1)
(define ex-social-media-item2 ex-tweet2)
(define ex-social-media-item3 ex-post1)
(define ex-social-media-item4 ex-post2)
(define ex-social-media-item5 ex-story1)
(define ex-social-media-item6 ex-story2)
(define ex-social-media-item7 ex-tweet3)
(define ex-social-media-item8 ex-post3)
(define ex-social-media-item9 ex-story3)

; a [List-of social-media-item](losmi) is one of:
; - '()
; - (cons social-media-item [List-of social-media-item])
; INTERPRETATION: a [List-of social-media-item], where every item is a social-media-item
;  list-of social-media-item-temp: [List-of social-media-item] -> ???
(define (list-of-social-media-item-temp losmi)
  (cond
    [(empty? losmi) ...]
    [(cons? losmi) (... (first losmi) ... (list-of-social-media-item-temp (rest losmi)) ...)]))

(define ex-losmi1 '())
(define ex-losmi2 (cons ex-social-media-item1 ex-losmi1))
(define ex-losmi3 (cons ex-social-media-item2 ex-losmi2))
(define ex-losmi4 (cons ex-social-media-item3 ex-losmi3))
(define ex-losmi5 (cons ex-social-media-item4 ex-losmi4))
(define ex-losmi6 (cons ex-social-media-item5 ex-losmi5))
(define ex-losmi7 (cons ex-social-media-item6 ex-losmi6))

;; Exercise 3:
; at-least-one-retweet: [List-of social-media-item] -> [List-of social-media-item]
; given a [List-of social-media-item], produces a [List-of social-media-item] of the tweets
; that have at least one retweet
(check-expect (at-least-one-retweet ex-losmi1) '())
(check-expect (at-least-one-retweet ex-losmi2) ex-losmi1)
(check-expect (at-least-one-retweet ex-losmi3) (list ex-social-media-item2))
(check-expect (at-least-one-retweet ex-losmi4) (list ex-social-media-item2))
(check-expect (at-least-one-retweet ex-losmi6) (list ex-social-media-item2))
(define (at-least-one-retweet losmi)
  (local ((define (one-retweet? smi) ; social-media-item -> Boolean
            (cond
              [(post? smi) #f]
              [(story? smi) #f]
              [(tweet? smi) (> (tweet-retweets smi) 0)])))
    (filter one-retweet? losmi)))

;; Exercise 4:
; never: [List-of social-media-item] -> [List-of social-media-item]
; given a [List-of social-media-item], produces a [List-of social-media-item] of the items that have
; never been viewed, shared, liked, or retweeted
(check-expect (never ex-losmi1) '())
(check-expect (never ex-losmi2) (list ex-social-media-item1))
(check-expect (never ex-losmi3) (list ex-social-media-item1))
(check-expect (never ex-losmi4) (list ex-social-media-item3 ex-social-media-item1))
(check-expect (never ex-losmi5) (list ex-social-media-item3 ex-social-media-item1))
(check-expect (never ex-losmi6) (list ex-social-media-item5 ex-social-media-item3
                                      ex-social-media-item1))
(check-expect (never ex-losmi7) (list ex-social-media-item5 ex-social-media-item3
                                      ex-social-media-item1))
(define (never losmi)
  (local ((define (utter-failure? smi) ; social-media-item -> Boolean
            (cond
              [(post? smi) (= (post-likes smi) 0)]
              [(story? smi) (= (story-views smi) 0)]
              [(tweet? smi) (and (= (tweet-likes smi) 0) (= (tweet-retweets smi) 0))])))
    (filter utter-failure? losmi))) ;[X -> Boolean] losmi -> losmi

;; Exercise 5:
; sum-of-engagements: [List-of social-media-item] -> Natural
; given a [List-of social-media-item], produces a single number that is the sum of the likes,
; retweets, and page views
(check-expect (sum-of-engagements ex-losmi1) 0)
(check-expect (sum-of-engagements ex-losmi2) 0)
(check-expect (sum-of-engagements ex-losmi3) 11)
(check-expect (sum-of-engagements ex-losmi4) 11)
(check-expect (sum-of-engagements ex-losmi5) 16)
(check-expect (sum-of-engagements ex-losmi6) 16)
(check-expect (sum-of-engagements ex-losmi7) 23)
(define (sum-of-engagements losmi)
  (local ((define (engagement smi) ; social-media-item -> Nat
            (cond
              [(post? smi) (post-likes smi)]
              [(story? smi) (story-views smi)]
              [(tweet? smi) (+ (tweet-likes smi) (tweet-retweets smi))]))
          (define (engagement-list losmi) ; [social-media-item -> Nat] losmi -> [List-of Nat]
            (map engagement losmi)))
    (foldr + 0 (engagement-list losmi)))) ; [social-media-item -> Nat] losmi [List-of Nat] -> Nat

; Exercise 6:
; create-item-under-280: social-media-item -> String
; given a social-media-item, returns item if text under 280 letters, if not, returns item with text
; as first 280 letters
(check-expect (create-item-under-280 ex-social-media-item1) "strawberries")
(check-expect (create-item-under-280 ex-social-media-item3) "coffee")
(check-expect (create-item-under-280 ex-social-media-item5) "chocolate")
(check-expect (create-item-under-280 ex-social-media-item7) "My castle crumbled overnight.
I brought a knife to a gunfight.
They took the crown but it's alright.
All the liars are calling me one,
nobody's heard from me for months,
I'm doing better than I ever was.
Cuz, my baby's fit like a daydream
walking with his head down I'm the one
he")
(check-expect (create-item-under-280 ex-social-media-item8) "Fever dream high in the quiet of the
night you know that I caught it. Bad, bad, boy shiny toy
with a price you know that I bought it. Killing me slow,
out the window, always waiting for you to be waiting below.
Devils roll the dice, angels roll their eyes, what doesn't
kill me ma")
(check-expect (create-item-under-280 ex-social-media-item9)  "I wonder if I'm being real.
Do I speak my truth or do I feel to how I feel.
I wonder wouldn't it be nice to live inside
a world that isn't black and white. I wonder what
it's like to be my friends, hope that they don't think
I'll forget about them. I wonder... I wonder... Right b")

(define (create-item-under-280 smi) 
  (cond
    [(and (post? smi) (<= (string-length (post-text smi)) 280)) (post-text smi)]
    [(and (post? smi) (> (string-length (post-text smi)) 280))
     (substring (post-text smi) 0 280)]
    [(and (story? smi) (<= (string-length (story-text smi)) 280)) (story-text smi)]
    [(and (story? smi) (> (string-length (story-text smi)) 280))
     (substring (story-text smi) 0 280)]
    [(tweet? smi) (tweet-text smi)]))

; create-list: social-media-item -> [List-of social-media-item]
; given a social media item, turns item into list where post produces list of tweet and story,
; story produces list of post and tweet, and tweet produces list of post and story
(check-expect (create-list ex-social-media-item1)
              (list (make-post "strawberries" 0)
                    (make-story "strawberries" 0)))
(check-expect (create-list ex-social-media-item3)
              (list (make-tweet "coffee" 0 0)
                    (make-story "coffee" 0)))
(check-expect (create-list ex-social-media-item5)
              (list (make-tweet "chocolate" 0 0)
                    (make-post "chocolate" 0)))
(check-expect (create-list ex-social-media-item7)
              (list (make-post "My castle crumbled overnight.
I brought a knife to a gunfight.
They took the crown but it's alright.
All the liars are calling me one,
nobody's heard from me for months,
I'm doing better than I ever was.
Cuz, my baby's fit like a daydream
walking with his head down I'm the one
he" 100)
                    (make-story "My castle crumbled overnight.
I brought a knife to a gunfight.
They took the crown but it's alright.
All the liars are calling me one,
nobody's heard from me for months,
I'm doing better than I ever was.
Cuz, my baby's fit like a daydream
walking with his head down I'm the one
he" 0)))
(check-expect (create-list ex-social-media-item8)
              (list (make-tweet "Fever dream high in the quiet of the
night you know that I caught it. Bad, bad, boy shiny toy
with a price you know that I bought it. Killing me slow,
out the window, always waiting for you to be waiting below.
Devils roll the dice, angels roll their eyes, what doesn't
kill me ma" 100 0)
                    (make-story "Fever dream high in the quiet of the
night you know that I caught it. Bad, bad, boy shiny toy
with a price you know that I bought it. Killing me slow,
out the window, always waiting for you to be waiting below.
Devils roll the dice, angels roll their eyes, what doesn't
kill me mak" 0)))
(check-expect (create-list ex-social-media-item9)
              (list (make-tweet "I wonder if I'm being real.
Do I speak my truth or do I feel to how I feel.
I wonder wouldn't it be nice to live inside
a world that isn't black and white. I wonder what
it's like to be my friends, hope that they don't think
I'll forget about them. I wonder... I wonder... Right b" 0 0)
                    (make-post "I wonder if I'm being real.
Do I speak my truth or do I feel to how I feel.
I wonder wouldn't it be nice to live inside
a world that isn't black and white. I wonder what
it's like to be my friends, hope that they don't think
I'll forget about them. I wonder... I wonder... Right be" 0)))

(define (create-list smi)
  (cond
    [(post? smi) (list (make-tweet (create-item-under-280 smi) (post-likes smi) 0)
                       (make-story (post-text smi) 0))]
    [(story? smi) (list (make-tweet (create-item-under-280 smi) 0 0)
                        (make-post (story-text smi) 0))]
    [(tweet? smi) (list (make-post (tweet-text smi) (tweet-likes smi))
                        (make-story (tweet-text smi) 0))]))


; crosspost: [List-of social-media-item] -> [List-of social-media-item]
; given a [List-of social-media-item], produces a list that has 2 items for each item in the list
; where a post produces a tweet and story, a story produces a post and tweet, and a tweet produces a
; post and story where all text, views, likes, and retweets are the same
(check-expect (crosspost ex-losmi1) '())
(check-expect (crosspost ex-losmi2) (list (make-post "strawberries" 0)
                                          (make-story "strawberries" 0)))
(check-expect (crosspost ex-losmi3) (list (make-post "blueberries" 10)
                                          (make-story "blueberries" 0)
                                          (make-post "strawberries" 0)
                                          (make-story "strawberries" 0)))
(check-expect (crosspost ex-losmi4) (list (make-tweet "coffee" 0 0)
                                          (make-story "coffee" 0)
                                          (make-post "blueberries" 10)
                                          (make-story "blueberries" 0)
                                          (make-post "strawberries" 0)
                                          (make-story "strawberries" 0)))
(define (crosspost losmi)
  (cond
    [(empty? losmi) losmi]
    [(cons? losmi) (append (create-list (first losmi)) (crosspost (rest losmi)))]))

; Exercise 7:
; f: X -> [List-of X]
; given a datatype, creates a list of the given datatype
(check-expect (f 3) (list 1 2 3))
(check-expect (f 2) (list 1 2))
(check-expect (f 0) '())
(define (f element)
  (build-list element add1))

; append-apply-to-all: f -> [List-of results]
; given a function and a list, appends the lists produces from applying the function to each element
(check-expect (append-apply-to-all f (list 3 2)) (list 1 2 3 1 2))
(check-expect (append-apply-to-all f (list 1 2)) (list 1 1 2))
(define (append-apply-to-all f l)
  (cond
    [(empty? l) l]
    [(cons? l) (append (f (first l)) (append-apply-to-all f (rest l)))]))

; Exercise 8:
; crosspost/v2: [List-of social-media-item] -> [List-of social-media-item]
; given a [List-of social-media-item], produces a list that has 2 items for each item in the list
; where a post produces a tweet and story, a story produces a post and tweet, and a tweet produces a
; post and story where all text, views, likes, and retweets are the same
(check-expect (crosspost/v2 ex-losmi1) '())
(check-expect (crosspost/v2 ex-losmi2) (list  (make-post "strawberries" 0)
                                              (make-story "strawberries" 0)))
(check-expect (crosspost/v2 ex-losmi3) (list (make-post "blueberries" 10)
                                             (make-story "blueberries" 0)
                                             (make-post "strawberries" 0)
                                             (make-story "strawberries" 0)))                   
(check-expect (crosspost/v2 ex-losmi4) (list (make-tweet "coffee" 0 0)
                                             (make-story "coffee" 0)
                                             (make-post "blueberries" 10)
                                             (make-story "blueberries" 0)
                                             (make-post "strawberries" 0)
                                             (make-story "strawberries" 0)))          
(define (crosspost/v2 losmi)
  (append-apply-to-all create-list losmi)) 

; Exercise 9:
; items-since-tweet: [List-of social-media-items] String -> [List-of social-media-items]
; given a [List-of social-media-items] and a tweet text as a String, produces a
; [List-of social-media-items] of the social-media-items made after the given tweet

(check-expect (items-since-tweet ex-losmi1 "Hello") '())
(check-expect (items-since-tweet ex-losmi2 "strawberries") (list ex-social-media-item1))
(check-expect (items-since-tweet ex-losmi3 "blueberries") (list ex-social-media-item2
                                                                ex-social-media-item1))
(check-expect
 (items-since-tweet ex-losmi7 "strawberries")
 (list ex-social-media-item1))                                                            
(check-expect (items-since-tweet ex-losmi4 "strawberries")
              (list ex-social-media-item1))
(check-expect (items-since-tweet ex-losmi7 "strawberries")
              (list ex-social-media-item1))                                      
(define (items-since-tweet losmi s)
  (cond
    [(empty? losmi) losmi]
    [(cons? losmi) (if (valid-tweet? (first losmi) s) losmi (items-since-tweet (rest losmi) s))]))
; valid-tweet?: social-media-item String -> Boolean
; given a social-media-item and a String, checks if the social media item is a tweet and
; the tweet-text is equal to the String, if yes returns true, if not returns false
(check-expect (valid-tweet? ex-social-media-item1 "Hello") #f)
(check-expect (valid-tweet? ex-social-media-item2 "blueberries") #t)
(check-expect (valid-tweet? ex-social-media-item3 "Hello") #f)
(check-expect (valid-tweet? ex-social-media-item5 "Hello") #f)
(define (valid-tweet? smi s)
  (cond
    [(post? smi) #f]
    [(story? smi) #f]
    [(and (tweet? smi) (string=? (tweet-text smi) s)) #t]
    [else #f]))                            

; Exercise 10:
; items-since-10-likes: [List-of social-media-items] -> [List-of social-media-items]
; given a [List-of social-media-items], produces a [List-of social-media-items] of posts listed
; after the first facebook post that received 10 or more likes
(define ex-losmi8 (list (make-post "care bears" 25) (make-tweet "totoro" 15 15)
                        (make-story "pikachu" 30) (make-story "eevee" 9)))
(define ex-losmi9 (list (make-post "care bears" 7) (make-tweet "totoro" 15 15)
                        (make-post "pikachu" 30) (make-story "eevee" 9)))
(define ex-losmi10 (list (make-story "eevee" 9) (make-tweet "totoro" 15 15)
                         (make-post "pikachu" 30) (make-post "care bears" 7)))
(check-expect (items-since-10-likes ex-losmi1) '())
(check-expect (items-since-10-likes ex-losmi8) ex-losmi8)
(check-expect (items-since-10-likes ex-losmi9) (list (make-post "pikachu" 30)
                                                     (make-story "eevee" 9)))
(check-expect (items-since-10-likes ex-losmi10) (list (make-post "pikachu" 30)
                                                      (make-post "care bears" 7)))
(define (items-since-10-likes losmi)
  (cond
    [(empty? losmi) losmi]
    [(cons? losmi) (if (valid-post? (first losmi) 10) losmi (items-since-10-likes (rest losmi)))]))

; valid-post? : social-media-item Natural -> Boolean
; given a social-media-item and a Natural, checks if the social media item is a post and post-likes
; are equal to the given Natural, if yes returns true, otherwise returns false
(define ex-social-media-item20 (make-post "nice" 100))
(check-expect (valid-post? ex-social-media-item1 10) #f)
(check-expect (valid-post? ex-social-media-item3 10) #f)
(check-expect (valid-post? ex-social-media-item20 10) #t)
(check-expect (valid-post? ex-social-media-item5 10) #f)
(define (valid-post? smi check)
  (cond
    [(story? smi) #f]
    [(tweet? smi) #f]
    [(and (post? smi) (>= (post-likes smi) check)) #t]
    [else #f]))

; Exercise 11:
; suffix-from-2500: [List-of Numbers] -> [List-of Numbers]
; given a [List-of Numbers], produces suffix of that list that begins
; from the first 2500 that occurs in the list
(define lon1 (list 1 2 3 4))
(define lon2 (list 2500 1 2 3 4))
(define lon3 (list 1 2 2500 3 4))
(check-expect (suffix-from-2500 lon1) '())
(check-expect (suffix-from-2500 lon2) lon2)
(check-expect (suffix-from-2500 lon3) (list 2500 3 4))
(define (suffix-from-2500 lon)
  (cond
    [(empty? lon) lon]
    [(cons? lon) (if (valid-num? (first lon) 2500) lon (suffix-from-2500 (rest lon)))]))

; valid-num? : Number Number -> Boolean
; given a number, checks if it is equal to a given value, if yes returns true
; otherwise returns false
(check-expect (valid-num? 10 5) #f)
(check-expect (valid-num? 10 10) #t)
(define (valid-num? x check)
  (if (= x check) #t #f))

; Exercise 12:
; produce-after-condition: {X, Y}[X Y -> Y] [List-of X] -> Y
; given a list and a condition, produces the suffix of the list from the element that satisfies the
; condition where the element that satifies the condition is also in the produced list
(define (produce-after-condition operation list check)
  (cond
    [(empty? list) list]
    [(cons? list) (if (operation (first list) check) list
                      (produce-after-condition operation (rest list) check))]))    
; Exercise 13:
; items-since-tweet/v2: [List-of social-media-items] String -> [List-of social-media-items]
; given a [List-of social-media-items] and a tweet text as a String, produces a
; [List-of social-media-items] of the social-media-items made after the given tweet
(check-expect (items-since-tweet/v2 ex-losmi1 "Hello") '())
(check-expect (items-since-tweet/v2 ex-losmi2 "strawberries")
              (list ex-social-media-item1))
(check-expect (items-since-tweet/v2 ex-losmi3 "blueberries")
              (list ex-social-media-item2 ex-social-media-item1))
(check-expect (items-since-tweet/v2 ex-losmi7 "strawberries") (list ex-social-media-item1))
(check-expect (items-since-tweet/v2 ex-losmi4 "strawberries") (list ex-social-media-item1))
(check-expect (items-since-tweet/v2 ex-losmi7 "strawberries") (list ex-social-media-item1))

(define (items-since-tweet/v2 losmi s)
  (produce-after-condition valid-tweet? losmi s))

; items-since-10-likes/v2: [List-of social-media-items] -> [List-of social-media-items]
; given a [List-of social-media-items], produces a [List-of social-media-items] of posts listed
; after the first facebook post that received 10 or more likes
(check-expect (items-since-10-likes/v2 ex-losmi1) '())
(check-expect (items-since-10-likes/v2 ex-losmi8) ex-losmi8)
(check-expect (items-since-10-likes/v2 ex-losmi9) (list (make-post "pikachu" 30)
                                                        (make-story "eevee" 9)))
(check-expect (items-since-10-likes ex-losmi10) (list (make-post "pikachu" 30)
                                                      (make-post "care bears" 7)))
(define (items-since-10-likes/v2 losmi)
  (produce-after-condition valid-post? losmi 10))

; suffix-from-2500/v2: [List-of Numbers] -> [List-of Numbers]
; given a [List-of Numbers], produces suffix of that list that begins
; from the first 2500 that occurs in the list
(check-expect (suffix-from-2500/v2 lon1) '())
(check-expect (suffix-from-2500/v2 lon2) lon2)
(check-expect (suffix-from-2500/v2 lon3) (list 2500 3 4))

(define (suffix-from-2500/v2 lon)
  (produce-after-condition valid-num? lon 2500))
  







