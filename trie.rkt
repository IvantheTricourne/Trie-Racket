#lang racket

(require rackunit)

(struct node (dict end?)
  #:mutable)

;; dict :: Hash Table
;; end? :: Boolean

(define (make-empty-node)
  (node (make-hash) #f))

(define (get-child curr char)
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
         (let ([next (get-child curr char)])
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
       (define next-node (get-child curr char))
       (and next-node (loop (add1 i) next-node))]
      [else (node-end? curr)])))

(define (get-prefix str start-node)
  (define str-len (string-length str))
  (let loop ([i 0] [curr start-node])
    (cond
      [(< i str-len)
       (define char (string-ref str i))
       (define next-node (get-child curr char))
       (and next-node (loop (add1 i) next-node))]
      [else curr])))


(define (trie->strings start-node)
  (let loop ([word '()] [words '()] [curr start-node])
    (cond
      ;; empty-dict
      [(zero? (hash-count (node-dict curr))) words]
      ;; node-end?
      [else
       (define dict (node-dict curr))
       (define new-words
         (if (node-end? curr)
             (cons (list->string word) words)
             words))
       (define-values (keys vals)
         (values (hash-keys dict) (hash-values dict)))
       (define (loopify c next)
         (loop (append word (list c)) new-words next))
       (append-map loopify keys (reverse vals))]
      ;; non-empty-dict
      ;; [else
      ;;  (define-values (keys vals)
      ;;    (values (hash-keys curr) (hash-values curr)))
      ;;  (define (loopify c next)
      ;;    (loop (append word (list c)) new-words next))
      ;;  (map loopify keys vals)]
      )))


;;; main
(define (nice-test)
  (define root-node (make-empty-node))
  (insert "cat" root-node)
  (insert "cata" root-node)
  (insert "cats" root-node)
  (insert "niceuuu" root-node)
  (insert "niceruuu" root-node)
  (insert "niceuuuruuu" root-node))

(define (main)
  (define root-node (make-empty-node))

  (check-true (zero? (hash-count (node-dict root-node)))
              "dictionary starts empty")
  
  ;; insert "cat"
  (insert "cat" root-node)

  (check-false (has-word? "niceuuu" root-node)
               "not niceuuuu")
  
  ;; insert "cats"
  (insert "cats" root-node)

  (check-true (has-word? "cat" root-node)
              "what happened to cat??")
  (check-true (has-word? "cats" root-node)
              "trie should have cats")
  (check-true (node? (get-prefix "c" root-node))
              "get-prefix")
  (check-true (node? (get-prefix "ca" root-node))
              "get-prefix")
  )


