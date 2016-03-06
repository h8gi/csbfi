(use csbfi matchable linenoise
     (only data-structures conc))

(define (bf-read-file file)
  (initialize)
  (bf-process-string
   (with-input-from-file file
     read-all)))
(define (repl-help)
  (display #<<END

?       Show this help
q       quit repl
>       Increment the data pointer
<       Decrement the data pointer
+       Increment the byte at the data pointer
-       Decrement the byte at the data pointer
.       Out put the byte at the data pointer
,       Input from stdin and store it in the byte at the data pointer
[       Start while loop
]       End while loop

For more information, see https://en.wikipedia.org/wiki/Brainfuck

END

))

(define (repl-hello)
  (display #<<END
CHICKEN SCHEME BRAINFUCK INTERPRETER
    type ? to see help message
    type q to quit


END

))

(define (bf-repl)
  (initialize)
  (set-history-length! 300)
  (load-history-from-file ".brainfuck_history")
  (repl-hello)
  (let loop ([l (linenoise "> ")])
    (cond
     [(equal? l "q")
      (save-history-to-file ".brainfuck_history") (display "BYE!\n")]
     [(equal? l "?")
      (repl-help)
      (newline)
      (loop (linenoise (conc (make-string *while-count* #\[) "> ")))]
     [else (bf-process-string l)
           (history-add l)
           (newline)
           (loop (linenoise (conc (make-string *while-count* #\[) "> ")))])))

(define (usage) (display #<<END
csbfi - chicken scheme brainfuck interpreter
Usage: csbfi [options] [file]
    -h              show this message        
    -e <string>     eval string

END

))

(define (main args)
  (match args
    [() (bf-repl)]
    [("-h" . rest)
     (usage)]
    [("-e" str . rest) (bf-process-string str)]
    [(filename) (bf-read-file filename)]
    [else (usage)]))

(main (command-line-arguments))
