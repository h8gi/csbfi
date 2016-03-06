(use srfi-69
     (only miscmacros push! inc! dec!)
     (only utils read-all)
     (only srfi-1 reverse!)
     (prefix utf8 utf8:))
;; string-length, string->list, read-char, display
(define *tape-length* 30000)
(define *tape* (make-vector *tape-length* 0))
(define *pointer* 0)
(define (stop?)
  (zero? (vector-ref *tape* *pointer*)))
(define *while-stack* '())
(define *while-count* 0)
(define (initialize)
  (set! *while-stack* '())
  (set! *while-count* 0)
  (set! *pointer 0)
  (vector-fill! *tape* 0))
;;; cとして渡されるのは char か *while-stack*
(define (do-ops c)
  (case c
    [(#\+) (inc! (vector-ref *tape* *pointer*))]
    [(#\-) (dec! (vector-ref *tape* *pointer*))]
    [(#\>) (inc! *pointer*)]
    [(#\<) (dec! *pointer*)]
    [(#\,) (vector-set! *tape* *pointer* (char->integer (read-char)))]
    [(#\.) (display (integer->char (vector-ref *tape* *pointer*)))]
    [else (when (list? c)
            (do-while c))]))

(define (do-while w-stk)
  (unless (or (stop?) (null? w-stk))
    (for-each do-ops w-stk)
    (do-while w-stk)))

(define (process-char c)
  (cond
   [(zero? *while-count*)                 ; whileの外にいるとき
    (case c
      [(#\[)                            ; 最外のwhileスタート 
       (inc! *while-count*)]            ; このときは push しない
      [(#\])
       (void)]
      [(#\+ #\- #\> #\< #\, #\.)
       (do-ops c)])]
   [(stop?) ; whileしない
    (set! *while-stack* '())
    (case c
      [(#\]) (dec! *while-count*)]
      [(#\[) (inc! *while-count*)])]
   [else                                ; whileの中にあって、(stop?)しない状態
    (case c
      [(#\[)
       (inc! *while-count*)
       (push! c *while-stack*)]
      [(#\])
       (dec! *while-count*)
       (cond [(zero? *while-count*)   ; whileから抜けた?
              (set! *while-stack* (reverse! *while-stack*))
              (do-while *while-stack*)
              (set! *while-stack* '())]
             [(> *while-count* 0)
              (set! *while-stack* (pack-while *while-stack*))]
             [else (set! *while-stack* '())] ; while ] の 打ちすぎ(このエッジケースはあり得るか?)
             )]
      [(#\+ #\- #\> #\< #\, #\.)
       (push! c *while-stack*)])]))

;;; state machine として
(define (pack-while while-stack)
  (let loop ([lst while-stack]
             [acc '()])
    (cond [(null? lst) #f]
          [(eq? (car lst) #\[) (cons acc (cdr lst))]
          [else (loop (cdr lst) (cons (car lst) acc))])))
(define (process-string str)
  (for-each process-char (string->list str)))
(define (bf-read-file file)
  (initialize)
  (process-string
   (with-input-from-file file
     read-all)))
