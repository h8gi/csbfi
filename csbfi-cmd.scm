(use csbfi matchable srfi-1 srfi-69)

(define usage #<<END
csbfi - chicken scheme brainfuck interpreter
Usage: csbfi [options] [file]
    -h              show this message    
    -s string       set tokens (default:"+-><,.[]", length must be 8)

END

)

(match (command-line-arguments)
  [() (bf-read)]
  [("-h" . rest) (display usage)]
  [("-s" str filename)
   (with-input-from-file filename
     (lambda () (bf-read str)))]
  [("-s" str)
   (bf-read str)]
  [(filename) (with-input-from-file filename
                bf-read)]
  [else (display usage)])

