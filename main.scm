(use srfi-69
     (only miscmacros push! pop! inc! dec!)
     (prefix utf8 utf8:))
;; string-length, string->list, read-char, display
(define *tape-length* 30000)
(define *tape* (make-vector *tape-length* 0))
(define *pointer* 0)
(define *while-stack* '())
(define *while-count* 0)
(define (do-ops c)
  (case c
    [(#\+) (inc! (vector-ref *tape* *pointer*))]
    [(#\-) (dec! (vector-ref *tape* *pointer*))]
    [(#\>) (inc! *pointer*)]
    [(#\<) (dec! *pointer*)]
    [(#\,) (vector-set! *tape* *pointer* (char->integer (read-char)))]
    [(#\.) (display (integer->char (vector-ref *tape* *pointer*)))]
    [else (for-each do-ops c)]))
;;; state machine として

(define (divide lst x)
  (let loop ([lst lst]
             [acc '()])
    (cond [(null? lst) #f]
          [(eq? (car lst) x) (values acc (cdr lst))]
          [else (loop (cdr lst) (cons (car lst) acc))])))

(define (make-brainfucker tape-length)
  (let ([tape (make-vector tape-length 0)]
        [ptr   0]
        [while-count 0]
        [code '()])
    (define (do-ops c)
      (case c
        [(#\+) (inc! (vector-ref tape ptr))]
        [(#\-) (dec! (vector-ref tape ptr))]
        [(#\>) (inc! ptr)]
        [(#\<) (dec! ptr)]
        [(#\,) (vector-set! tape ptr (char->integer (read-char)))]
        [(#\.) (display (integer->char (vector-ref tape ptr)))]
        [else (for-each do-ops c)]))
    (define (process-char c)
      (cond
       [(zero? while-count)             ; whileの外にいるとき
        (case c
          [(#\+ #\- #\> #\< #\, #\.) (do-ops c)]
          [(#\[)
           (inc! while-count)
           (push! c code)]
          [(#\])
           (void)]
          [else (void)])]
       [(zero? (vector-ref tape ptr))
        (set! code '())
        (case c
          [(#\]) (dec! while-count)]
          [(#\[) (inc! while-count)]
          [else (void)])]
       [else
        (case c
          [(#\+ #\- #\> #\< #\, #\.) (push! c (car code))]
          [(#\[)
           (inc! while-count)
           (push! c code)]
          [(#\])
           (dec! while-count)
           (if (zero? while-count)               ; whileから抜けた?
               (if (zero? (vector-ref tape ptr)) ; 0であれば 
                   (set! code '())               ; 終了
                   (begin (set! code (reverse! code))
                          (let loop ()
                            (unless (zero? (vector-ref tape ptr))
                              (do-ops code)
                              (loop)))
                          (set! code '())))
               (receive (while-code rest) (divide code #\[)
                 (set! code (cons while-code rest))))]
          [else (void)])]))
    process-char))
