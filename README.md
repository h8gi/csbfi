# csbfi

a brainfuck interpreter

## usage

`chicken-install -s`で実行ファイルcsbfiが多分/usr/local/bin以下につくられます。

ファイル名をわたすとファイルを実行します。無いとreplが起動します。
`csbfi -h`でヘルプが出ます。

~~~~~

csbfi - chicken scheme brainfuck interpreter
Usage: csbfi [options] [file]
    -h              show this message
    -s string       set tokens (string length must be 8)

~~~~~

sオプションに文字列を渡すことでトークンを指定できます。各トークンは1文字のaschiiにしてください。

~~~~~

~~~~~