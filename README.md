# csbfi

a brainfuck interpreter

## usage

`chicken-install -s`で実行ファイルcsbfiが多分/usr/local/bin以下につくられます。

ファイル名をわたすとファイルを実行します。無いとreplが起動します。
`csbfi -h`でヘルプが出ます。オプションのパースが雑です。

~~~~~
csbfi - chicken scheme brainfuck interpreter
Usage: csbfi [options] [file]
    -h              show this message
    -s string       set tokens (default:"+-><,.[]", length must be 8)
~~~~~

sオプションに文字列を渡すことでトークンを指定できます。デフォルトは"+-><,.[]"です。
各トークンはアスキーで1文字にしてください。

~~~~~{.sh}
 [~/Documents/brainfuck]
 $ cat hello.bf
++++++++[>++++[>++>++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++
 [~/Documents/brainfuck]
 $ csbfi hello.bf
HELLO WORLD!
 [~/Documents/brainfuck]
 $ cat hello_nm.bf
00000000620000620020020002033331720202122063731722521115000000055000522531535000511111151111111152205200
 [~/Documents/brainfuck]
 $ csbfi -s "01234567" hello_nm.bf
HELLO WORLD!
~~~~~
