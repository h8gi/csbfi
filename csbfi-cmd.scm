(use csbfi matchable srfi-1 srfi-69)


(define (usage) (display #<<END
csbfi - chicken scheme brainfuck interpreter
Usage: csbfi [options] [file]
    -h              show this message        
    -e <string>     eval string

END

))

(match (command-line-arguments)
  [() (bf-repl)]
  [("-h" . rest)
   (usage)]
  [("-e" str . rest) (bf-process-string str)]
  [(filename) (bf-read-file filename)]
  [else (usage)])

