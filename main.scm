 (use (only utils read-all)
     (only srfi-1 reverse!)
     (only srfi-13 string-for-each)
     irregex)
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

(define *op-table*
  `([#\+ . ,(lambda () (vector-set! *tape* *pointer* (fx+ (vector-ref *tape* *pointer*) 1)))]
    [#\- . ,(lambda () (vector-set! *tape* *pointer* (fx- (vector-ref *tape* *pointer*) 1)))]
    [#\> . ,(lambda () (set! *pointer* (fx+ *pointer* 1)))]
    [#\< . ,(lambda () (set! *pointer* (fx- *pointer* 1)))]
    [#\, . ,(lambda () (vector-set! *tape* *pointer* (char->integer (read-char))))]
    [#\. . ,(lambda () (display (integer->char (vector-ref *tape* *pointer*))))]))

;;; cとして渡されるのは char か *while-stack*
(define (do-ops c)
  (case c
    [(#\+) (vector-set! *tape* *pointer* (fx+ (vector-ref *tape* *pointer*) 1))]
    [(#\-) (vector-set! *tape* *pointer* (fx- (vector-ref *tape* *pointer*) 1))]
    [(#\>) (set! *pointer* (fx+ *pointer* 1))]
    [(#\<) (set! *pointer* (fx- *pointer* 1))]
    [(#\,) (vector-set! *tape* *pointer* (char->integer (read-char)))]
    [(#\.) (display (integer->char (vector-ref *tape* *pointer*)))]))

(define (do-while w-stk)
  (unless (null? w-stk)
    (let loop ()
      (unless (stop?)
        (for-each (cut <>) w-stk)
        (loop)))))

(define (process-char c)
  (cond
   [(zero? *while-count*)                 ; whileの外にいるとき
    (case c
      [(#\[)                            ; 最外のwhileスタート 
       (set! *while-count* (fx+ *while-count* 1))] ; このときは push しない
      [(#\])
       (void)]
      [else
       (do-ops c)])]
   [(stop?) ; whileしない
    (set! *while-stack* '())
    (case c
      [(#\]) (set! *while-count* (fx- *while-count* 1))]
      [(#\[) (set! *while-count* (fx+ *while-count* 1))])]
   [else                                ; whileの中にあって、(stop?)しない状態
    (case c
      [(#\[)
       (set! *while-count* (fx+ *while-count* 1))
       (set! *while-stack* (cons c *while-stack*))]
      [(#\])
       (set! *while-count* (fx- *while-count* 1))
       (cond [(zero? *while-count*)   ; whileから抜けた?
              (set! *while-stack* (reverse! *while-stack*))
              (do-while *while-stack*)
              (set! *while-stack* '())]
             [(> *while-count* 0)
              (set! *while-stack* (pack-while *while-stack*))]
             [else (set! *while-stack* '())] ; while ] の 打ちすぎ(このエッジケースはあり得るか?)
             )]
      [else
       (set! *while-stack* (cons (cdr (assoc c *op-table*))
                                 *while-stack*))])]))
(define (pack-while while-stack)
  (let loop ([lst while-stack]
             [acc '()])
    (cond [(null? lst) #f]
          [(eq? (car lst) #\[)  (cons (lambda () (do-while acc))
                                      (cdr lst))]
          [else (loop (cdr lst) (cons (car lst) acc))])))

(define (bf-process-string str)
  (string-for-each process-char (irregex-replace/all "[^+-><,.\\[\\]]" str ""))
  (flush-output))

(define (dump-tape start end)
  (when (<= 0 start end *tape-length*)
    (do ([i start (add1 i)]
         [c 1 (add1 c)])
        ((>= i end))
      (display (vector-ref *tape* i))
      (display " ")
      (when (zero? (modulo c 50))
        (newline))))
  (newline)
  (flush-output))
