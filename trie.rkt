#lang racket

(require rackunit)

(struct node (dict end?)
  #:mutable)

;; dict :: Hash Table
;; end? :: Boolean

(define (make-empty-node)
  (node (make-hash) #f))

(define (get-child char curr)
  (define ref
    (hash-ref (node-dict curr) char #f))
  (if ref ref #f))

(define (set-dict! dict char next-node)
  (hash-set! dict char next-node))

(define (insert str start-node)
  (define str-len (string-length str))
  (let loop ([i 0] [curr start-node])
    (cond
      [(< i str-len)
       (define dict (node-dict curr))
       (define char (string-ref str i))
       (define next-node
         (let ([next (get-child char curr)])
           (if next
               (loop (add1 i) next)
               (loop (add1 i) (make-empty-node)))))
       (set-dict! dict char next-node)
       (node dict (node-end? curr))]
      [else (node (make-hash) #t)])))

(define (has-word? str start-node)
  (define str-len (string-length str))
  (let loop ([i 0] [curr start-node])
    (cond
      [(< i str-len)
       (define char (string-ref str i))
       (define next-node (get-child char curr))
       (and next-node (loop (add1 i) next-node))]
      [else (node-end? curr)])))

(define (get-prefix str start-node)
  (define str-len (string-length str))
  (let loop ([i 0] [curr start-node])
    (cond
      [(< i str-len)
       (define char (string-ref str i))
       (define next-node (get-child char curr))
       (and next-node (loop (add1 i) next-node))]
      [else curr])))


(define (get-words start-node)
  (let loop ([word '()] [words '()] [curr start-node])
    (cond
      ;; empty-dict
      [(zero? (hash-count (node-dict curr)))
       words]
      [else
       (define dict (node-dict curr))
       (define new-words
         (if (node-end? curr)
             (cons (list->string word) words)
             words))
       (define keys (hash-keys dict))
       (define (loopify c)
         (define next (hash-ref dict c))
         (display c) (display (node-dict next)) (newline)
         (loop (append word (list c)) new-words next))
       (append-map loopify keys)])))


;;; main
(define (nice-test)
  (define root-node (make-empty-node))
  (insert "cat" root-node)
  (insert "cata" root-node)
  (insert "cats" root-node)
  (insert "niceuuu" root-node)
  (insert "niceruuu" root-node)
  ;; when you get rid of this... niceuuu is gone in get-words
  (insert "niceuuuruuu" root-node))

(define root-node (nice-test))
(define cat-branch (get-prefix "cat" root-node))
(define nice-branch (get-prefix "nice" root-node))

(define (main)
  ;; check for all words in cat branch
  (check-true
   (andmap (λ (word) (has-word? word root-node))
           '("cat" "cata" "cats"))
   "missing a word in cat branch")
  ;; check for all words in nice branch
  (check-true
   (andmap (λ (word) (has-word? word root-node))
           '("niceuuu" "niceruuu" "niceuuuruuu"))
   "missing a word in nice branch")
  )


