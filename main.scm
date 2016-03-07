 (use (only utils read-all)
     (only srfi-1 reverse!)
     (only srfi-13 string-for-each)
     irregex)
;; string-length, string->list, read-char, display
(define *tape-length* 30000)
(define *tape* (make-vector *tape-length* 0))
(define *pointer* 0)
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

(define (parse-string str)
  (fold-right (lambda (c acc)
                (cond  [(char=? c #\[) (cons 'open acc)]
                       [(char=? c #\]) (cons 'close acc)]
                       [(assoc c *op-table*) => (lambda (x) (cons (cdr x) acc))]                       
                       [else acc]))
              '()
              (string->list str)))

;;; tkn は 関数か シンボル
(define (process-token tkn)
  (cond
   [(fx= 0 *while-count*)               ; whileの外にいるとき
    (case tkn
      [(open)                            ; 最外のwhileスタート 
       (set! *while-count* (fx+ *while-count* 1))] ; このときは push しない
      [(close)
       (void)]
      [else
       (tkn)])]
   [(fx= 0 (vector-ref *tape* *pointer*)) ;while しない
    (set! *while-stack* '())
    (case tkn
      [(close) (set! *while-count* (fx- *while-count* 1))]
      [(open)  (set! *while-count* (fx+ *while-count* 1))])]
   [else                        ; whileの中にあって、(stop?)しない状態
    (case tkn
      [(open)
       (set! *while-count* (fx+ *while-count* 1))
       (set! *while-stack* (cons tkn *while-stack*))]
      [(close)
       (set! *while-count* (fx- *while-count* 1))
       (cond [(fx= 0 *while-count*)     ; whileから抜けた?
              (set! *while-stack* (reverse! *while-stack*))
              (do-while *while-stack*)
              (set! *while-stack* '())]
             [(> *while-count* 0)
              (set! *while-stack* (pack-while *while-stack*))]
             [else (set! *while-stack* '())] ; while ] の 打ちすぎ(このエッジケースはあり得るか?)
             )]
      [else
       (set! *while-stack* (cons tkn *while-stack*))])]))

(define (do-while w-stk)
  (unless (null? w-stk)
    (let loop ()
      (unless (fx= 0 (vector-ref *tape* *pointer*))
        (for-each (cut <>) w-stk)
        (loop)))))

(define (pack-while while-stack)
  (let loop ([lst while-stack]
             [acc '()])
    (cond [(null? lst) #f]
          [(eq? (car lst) 'open) (cons (lambda () (do-while acc))
                                       (cdr lst))]
          [else (loop (cdr lst) (cons (car lst) acc))])))

(define (bf-process-string str)
  (for-each process-token (parse-string str))
  (flush-output))

(define (bf-read-file file)
  (initialize)
  (bf-process-string
   (with-input-from-file file
     read-all)))

;;; for debug
(define (dump-tape start end)
  (when (<= 0 start end *tape-length*)
    (do ([i start (add1 i)]
         [c 1 (add1 c)])
        ((>= i end))
      (display (vector-ref *tape* i))
      (display " ")
      (when (fx= 0 (modulo c 50))
        (newline))))
  (newline)
  (flush-output))

;; (bf-read-file "test/bench.bf")

