(use srfi-69
     (only miscmacros push! pop! inc! dec!)
     (prefix utf8 utf8:))
;; string-length, string->list, read-char, display
(define *array-length* 30000)
(define *array* (make-vector *array-length* 0))
(define *pointer* 0)
(define *read-table* #f)
(define bf+ (make-parameter #\+))
(define bf- (make-parameter #\-))
(define bf> (make-parameter #\>))
(define bf< (make-parameter #\<))
(define bf-in (make-parameter #\,))
(define bf-out (make-parameter #\.))
(define bf-lb (make-parameter #\[))
(define bf-rb (make-parameter #\]))
(define *default-str* "+-><,.[]")
(define (set-tokens! str)
  (assert (= 8 (string-length str)) "string length != 8" str)
  (map (lambda (p x) (p x))
       (list bf+ bf- bf> bf< bf-in bf-out bf-lb bf-rb)
       (string->list str)))


(define (initialize str)
  (set! *pointer* 0)
  (set! *array* (make-vector *array-length* 0))
  (set-tokens! str)
  (set! *read-table*
        (alist->hash-table `((,(bf+) . ,(lambda ()
                                          (inc! (vector-ref *array* *pointer*))))
                             (,(bf-) . ,(lambda ()
                                          (dec! (vector-ref *array* *pointer*))))
                             (,(bf>) . ,(lambda ()
                                          (inc! *pointer*)))
                             (,(bf<) . ,(lambda ()
                                          (dec! *pointer*)))
                             (,(bf-in) . ,(lambda ()
                                            (vector-set! *array* *pointer*
                                                         (char->integer (read-char)))))
                             (,(bf-out) . ,(lambda ()
                                             (display
                                              (integer->char (vector-ref *array* *pointer*)))))
                             (,(bf-lb) . ,(lambda ()
                                            ((read-while)) ; 2重
                                            ))))))

(define (bf-read #!optional (str *default-str*))
  (initialize str)
  (let loop ([ch (read-char)])
    (unless (eof-object? ch)
      (cond [(hash-table-ref/default *read-table* ch #f)
             => (lambda (proc)
                  (proc)
                  (loop (read-char)))]
            [else (loop (read-char))]))))

(define (read-while)
  (let read-loop ([ch (read-char)]
                  [proc-list '()])
    (unless (eof-object? ch)
      (cond [(char=? ch (bf-lb))       ; 開き
             (let ([proc (read-while)])
               (read-loop (read-char) (cons proc proc-list)))]            
            [(char=? ch (bf-rb))       ; とじ
             (set! proc-list (reverse! proc-list))
             (lambda ()                     ; 返り値として関数
               (let do-loop ()
                 (unless (zero? (vector-ref *array* *pointer*))                 
                   (for-each (cut <>) proc-list)
                   (do-loop))))]

            [(hash-table-ref/default *read-table* ch #f) ; ループ以外のコード
             => (lambda (proc)                  
                  (read-loop (read-char) (cons proc proc-list)))]
            [else
             (read-loop (read-char) proc-list)]))))
