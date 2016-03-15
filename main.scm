 (use (only utils read-all)
     (only srfi-1 reverse!)
     (only srfi-13 string-for-each)
     (only vector-lib vector-for-each)
     srfi-69
     irregex)
;; string-length, string->list, read-char, display
(: *pointer* fixnum)
(define *tape-length* 30000)
(define *tape* (make-vector *tape-length* 0))
(define *pointer* 0)
(define *while-stack* '())
(define *while-count* 0)
(define pack-while #f)
(define (initialize)
  (set! *while-stack* '())
  (set! *while-count* 0)
  (set! *pointer* 0)
  (vector-fill! *tape* 0)
  (set! *counter* 0)
  (if (bf-opt)
      (set! pack-while pack-while-opt)
      (set! pack-while pack-while-no-opt)))
(define bf-debug (make-parameter #t))
(define bf-opt (make-parameter #f))
(define *counter* 0)
(define (count-op op)
  (set! *counter* (add1 *counter*)))
(define *op-table*
  `([#\+ +   . ,(lambda ()    (vector-set! *tape* *pointer* (fx+ (vector-ref *tape* *pointer*) 1)))]
    [#\- -   . ,(lambda ()    (vector-set! *tape* *pointer* (fx- (vector-ref *tape* *pointer*) 1)))]
    [#\> >   . ,(lambda ()    (set! *pointer* (fx+ *pointer* 1)))]
    [#\< <   . ,(lambda ()    (set! *pointer* (fx- *pointer* 1)))]
    [#\, in  . ,(lambda ()   (vector-set! *tape* *pointer* (char->integer (read-char))))]
    [#\. out . ,(lambda ()  (display (integer->char (vector-ref *tape* *pointer*))))]
    [#\[ open ]
    [#\] close]))

(define (parse-string str)
  (fold-right (lambda (c acc)
                (cond [(assoc c *op-table*) => (lambda (x) (cons (cdr x) acc))]                       
                      [else acc]))
              '()
              (string->list str)))

;;; tkn は 関数か シンボル
(define (process-token-pair tkn-pair)
  (let ([tkn  (car tkn-pair)]
        [proc (cdr tkn-pair)])
    (cond
     [(fx= 0 *while-count*)             ; whileの外にいるとき
      (cond  [(eq? tkn 'open)           ; 最外のwhileスタート 
              (set! *while-count* (fx+ *while-count* 1))] ; このときは push しない
             [(eq? tkn 'close)
              (void)]
             [else
              (proc)])]
     ;; ここからはwhileの内側
     [(fx= 0 (vector-ref *tape* *pointer*)) ;while しない
      (set! *while-stack* '())
      (cond [(eq? tkn 'close) (set! *while-count* (fx- *while-count* 1))]
            [(eq? tkn 'open)  (set! *while-count* (fx+ *while-count* 1))])]
     ;; whileの中にあって、stop しない状態
     [(eq? tkn 'open)
      (set! *while-count* (fx+ *while-count* 1))
      (set! *while-stack* (cons tkn-pair *while-stack*))]
     [(eq? tkn 'close)
      (set! *while-count* (fx- *while-count* 1))
      (cond [(fx= 0 *while-count*)      ; whileから抜けた?
             (pack-while *while-stack*)
             (set! *while-stack* '())]
            [(> *while-count* 0)        ; whileの中
             (set! *while-stack* (pack-while *while-stack*))]
            [else (set! *while-stack* '())] ; while ] の 打ちすぎ(このエッジケースはあり得るか?)
            )]
     [else                              ; とりあえず
      (set! *while-stack* (cons tkn-pair *while-stack*))])))

(define (exec-while w-stk)
  (unless (null? w-stk)
    (let loop ()
      (unless (fx= 0 (vector-ref *tape* *pointer*))
        (for-each (cut <>) w-stk)
        (loop)))))
;;;  TODO: FIXME
;;; あとは最適化をかけるだけ…
(define (pack-while-no-opt while-stack)
  (let loop ([lst while-stack]
             [acc '()])
    (if (null? lst)
        (exec-while acc)
        (let ([tkn (caar lst)]
              [proc (cdar lst)])
          (if (eq? tkn 'open)
              (cons `(while . ,(lambda () (exec-while acc))) ; dummy 'while
                    (cdr lst))
              (loop (cdr lst) (cons proc acc)))))))

;;; これ動くか?
(define (pack-while-opt while-stack)
  (let ([pos *pointer*]
        [pos-table (make-pos-table (length while-stack))]
        [len (length while-stack)])
    (let loop ([lst while-stack]
               [acc '()])
      (if (null? lst)                   ; 最外のwhile おわり
          (if (and (fx= *pointer* pos) (fx= -1 (pos-ref pos-table pos len)))
              (pos-for-each pos-table len)
              (exec-while acc))
          (let ([tkn  (caar lst)]
                [proc (cdar lst)])
            (cond [(eq? tkn 'open)                   
                   (cons `(while .
                            ,(lambda ()
                               (if (and (fx= *pointer* pos) (fx= -1 (pos-ref pos-table pos len)))
                                   (pos-for-each pos-table len)                       
                                   (exec-while acc))))
                         (cdr lst))]
                  [(eq? tkn '+)
                   (pos-inc! pos-table pos len)
                   (loop (cdr lst) (cons proc acc))]
                  [(eq? tkn '-)
                   (pos-dec! pos-table pos len)
                   (loop (cdr lst) (cons proc acc))]
                  ;; うそだろ
                  [(eq? tkn '>) (set! pos (fx- pos 1)) (loop (cdr lst) (cons proc acc))] ;逆をやる必要がある
                  [(eq? tkn '<) (set! pos (fx+ pos 1)) (loop (cdr lst) (cons proc acc))]
                  [(eq? tkn 'while)     ; whileコード…
                   (loop (cdr lst) (cons proc acc))]
                  [else (pack-while-no-opt while-stack)]))))))

(define (make-pos-table len)
  (make-vector (fx+ 1 (fx* 2 len)) 0))

(define (pos-ref tbl pos len)
  (vector-ref tbl (fx- (fx+ pos len) *pointer*)))

(define (pos-inc! tbl pos len)
  (let ([i (fx- (fx+ pos len) *pointer*)])
    (vector-set! tbl i
                 (fx+ (vector-ref tbl i) 1))))

(define (pos-dec! tbl pos len)
  (let ([i (fx- (fx+ pos len) *pointer*)])
    (vector-set! tbl i
                 (fx- (vector-ref tbl i) 1))))
;;; i = pos + len -pointer
;;; pos = i -len + pointer
(define (pos-for-each tbl len)
  (let ([val (vector-ref *tape* *pointer*)])    
    (vector-for-each (lambda (i x)
                       (unless (fx= x 0)
                         (let ([pos (fx+ (fx- i len) *pointer*)])
                           (vector-set! *tape*
                                        pos
                                        (fx+ (vector-ref *tape* pos)
                                             (fx* val x))))))
                     tbl)
    (vector-set! *tape* *pointer* 0)))

(define (bf-process-string str)
  (for-each process-token-pair (parse-string str))
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
