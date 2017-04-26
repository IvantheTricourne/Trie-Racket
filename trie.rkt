#lang racket

(require rackunit)

(struct node (dict end?)
  #:mutable)

;; dict :: Hash Table
;; end? :: Boolean

(define (make-empty-node)
  (node (make-hash) #f))

(define (set-dict! dict char next-node)
  (hash-set! dict char next-node))

;; helper function to get node from a char
(define (get-child char curr)
  (define ref
    (hash-ref (node-dict curr) char #f))
  (if ref ref #f))

;; inserts a string into the trie
(define (insert str start-node)
  (define str-len (string-length str))
  (let loop ([i 0] [curr start-node])
    (cond
      [(= i str-len) (node (make-hash) #t)]
      [else
       (define dict (node-dict curr))
       (define char (string-ref str i))
       (define next-node
         (let ([next (get-child char curr)])
           (if next
               (loop (add1 i) next)
               (loop (add1 i) (make-empty-node)))))
       (set-dict! dict char next-node)
       (node dict (node-end? curr))])))

;; determines whether the given trie contains a word
(define (has-word? str start-node)
  (define str-len (string-length str))
  (let loop ([i 0] [curr start-node])
    (cond
      [(= i str-len) (node-end? curr)]
      [else
       (define char (string-ref str i))
       (define next-node (get-child char curr))
       (and next-node (loop (add1 i) next-node))])))

;; returns the sub-trie after a given prefix string
(define (get-prefix str start-node)
  (define str-len (string-length str))
  (let loop ([i 0] [curr start-node])
    (cond
      [(= i str-len) curr]
      [else
       (define char (string-ref str i))
       (define next-node (get-child char curr))
       (and next-node (loop (add1 i) next-node))])))

;; get all words contained within a trie
(define (get-words start-node)
  (define (add-word word words curr)
    (if (node-end? curr)
        (cons (list->string word) words)
        words))
  (let loop ([word '()] [words '()] [curr start-node])
    (define dict (node-dict curr))
    (cond
      ;; empty-dict
      [(zero? (hash-count dict))
       (add-word word words curr)]
      [else
       (define keys (hash-keys dict))
       (define (loopify c)
         (loop (append word (list c)) words (hash-ref dict c)))
       (define new-words (append-map loopify keys))
       (add-word word new-words curr)])))


;;; main
(define (nice-test)
  (define root-node (make-empty-node))
  (insert "cat" root-node)
  (insert "cata" root-node)
  (insert "cats" root-node)
  (insert "nice" root-node)
  (insert "niceuuu" root-node)
  (insert "niceruuu" root-node)
  (insert "niceuuuruuu" root-node))

(define root-node (nice-test))
(define cat-branch (get-prefix "cat" root-node))
(define nice-branch (get-prefix "nice" root-node))

(define (main)
  ;; tests for has-word?
  (check-true
   (andmap (λ (word) (has-word? word root-node))
           '("cat" "cata" "cats"))
   "missing a word in cat branch")
  ;; check for all words in nice branch
  (check-true
   (andmap (λ (word) (has-word? word root-node))
           '("niceuuu" "niceruuu" "niceuuuruuu"))
   "missing a word in nice branch")
  ;; test for get-words
  (define all-words (get-words root-node))
  ;; (check-true
  ;;  (andmap (λ (word)
  ;;            (memv word all-words))
  ;;          '("cat" "cata" "cats" "nice"
  ;;            "niceuuu" "niceruuu" "niceuuuruuu")))
  )


