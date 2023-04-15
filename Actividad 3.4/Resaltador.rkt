;; Jose Ricardo Rosales Castañeda
;; Dante David Perez Perez
#lang racket

(require 2htdp/batch-io)

;; keyword from c#
(define keyword
  '("abstract" "as" "base" "bool" "break" "byte" "case" "catch" "char" "checked"
    "class" "const" "continue" "decimal" "default" "delegate" "do" "double" "else"
    "enum" "event" "explicit" "extern" "false" "finally" "fixed" "float" "for"
    "foreach" "goto" "if" "implicit" "in" "int" "interface" "internal" "is" "lock"
    "long" "namespace" "new" "null" "object" "operator" "out" "override" "params"
    "private" "protected" "public" "readonly" "ref" "return" "sbyte" "sealed" "short"
    "sizeof" "stackalloc" "static" "string" "struct" "switch" "this" "throw" "true"
    "try" "typeof" "uint" "ulong" "unchecked" "unsafe" "ushort" "using" "virtual"
    "void" "volatile" "while"))

;; System
(define System
  '("System" "Console" "Program"))

;; operator from c#
(define operator
  '("&=" "|=" "^=" "<<" ">>" ">>>" "<<=" ">>=" ">>>=" "==" "!=" "<=" ">=" "&&" "||"
    "+" "-" "*" "/" "%" "&" "|" "^" "!" "~" "=" "<" ">" "+=" "-=" "*=" 
    "/=" "%=" "++" "--" "?" "??"))

;; separator from c#
(define separator
  '(";" "," "." "(" ")" "[" "]" "{" "}" "<" ">" ":" "::" "..." "=>" "??"))

  ;; numbers using regexp from c#
(define number
  (regexp "(\\b[0-9]+(\\.[0-9]*)?\\b)"))

;; strings using regexp from c#
(define string
  (regexp "(\"(\\\\.|[^\\\\\"])*\")"))

;; all types of comments using regexp from c#
(define comment
  (regexp "(//.*|/\\*.*\\*/|/\\*.*|.*\\*/)"))

;; identifier using regexp from c#
(define identifier
  (regexp "[a-zA-Z_][a-zA-Z0-9_]*$"))

;; function to replace comments
(define (replace-comment s)
  (string-append "<span class=\"comment\">" s "</span>"))

;; function to categorize tokens
(define (categorize-token s)
    (cond
        [(member s keyword) (string-append "<span class=\"keyword\">" s "</span>")]
        [(member s operator) (string-append "<span class=\"operator\">" s "</span>")]
        [(member s separator) (string-append "<span class=\"separator\">" s "</span>")]
        [(member s System) (string-append "<span class=\"System\">" s "</span>")]
        [(regexp-match number s) (string-append "<span class=\"number\">" s "</span>")]
        [(regexp-match string s) (string-append "<span class=\"string\">" s "</span>")]
        [(regexp-match comment s) (string-append "<span class=\"comment\">" s "</span>")]
        [(regexp-match identifier s) (string-append "<span class=\"identifier\">" s "</span>")]
        [else s]
    )
)

;;Replace all tokens
(define (replace-all-tokens s multiline-comment)
    (define string '())
    (define string-list '())
    (define string-identifier #f)
    (define string-comment #f)
    (define open-string-comment #f)
    (define ch (regexp-split #px"" s))

     (for/last ([char ch])
      (when (and (eq? char (last ch)) (or open-string-comment multiline-comment)) (set! string-list (append string-list (list string))))
        (cond 
          [multiline-comment (set! string (append string (list char)))]
          [(regexp-match #rx"#" char) (set! string (append string (list char)))]
          [(regexp-match? #rx"[a-zA-Z0-9_]" char)
          (set! string (append string (list char)))]
          [(regexp-match #px"/" char) 
            (cond 
              [string-comment 
                ((lambda () 
                  (set! string-comment #f)
                  (set! open-string-comment #t)
                  (set! string (append string (list char)))))]
              [else 
                ((lambda () 
                  (set! string-comment #t)
                  (set! string (append string (list char)))))])]
          [open-string-comment (set! string (append string (list char)))]
          [(regexp-match? #px"\"" char)
          ((lambda ()
              (set! string-identifier (not string-identifier))
              (set! string (append string (list char)))))]
          [string-identifier (set! string (append string (list char)))]
          [(member char operator)
          ((lambda ()
              (set! string-list (append string-list (list string)))
              (set! string '())
              (set! string (append string (list char)))
              (set! string-list (append string-list (list string)))
              (set! string '())))]
          [(member char separator)
          ((lambda ()
              (set! string-list (append string-list (list string)))
              (set! string '())
              (set! string (append string (list char)))
              (set! string-list (append string-list (list string)))
              (set! string '())))]
          [else
          ((lambda ()
              (set! string-list (append string-list (list string)))
              (set! string '())))]
        )
    )

    (define tokens (map (lambda (x) (string-join x "")) string-list))
    (define (categorize-tokens tokens)
      (define (loop tokens multiline-comment)
        (cond
          [(null? tokens) '()]
          [(and (string=? (car tokens) "/*") (not multiline-comment))
            (append (list (categorize-token (car tokens))) (loop (cdr tokens) #t))]
          [(and (string=? (car tokens) "*/") multiline-comment)
            (append (list (categorize-token (car tokens))) (loop (cdr tokens) #f))]
          [multiline-comment
            (append (list (replace-comment (car tokens))) (loop (cdr tokens) #t))]
          [(string=? (car tokens) "//")
            (append (list (replace-comment (car tokens))) (loop (cdr tokens) #f))]
          [else
            (append (list (categorize-token (car tokens))) (loop (cdr tokens) #f))]))
      (loop tokens multiline-comment))

    (categorize-tokens tokens)
)

;; html header
(define html-header
  "<!DOCTYPE html>
  <html>
  <head>
    <meta chet=\"utf-8\">
    <title>Resaltador de sintaxis</title>
    <link rel=\"stylesheet\" href=\"style.css\">
  </head>
  <body>
  <pre>"
)

;; html footer
(define html-footer
  "</pre>
  </body>
  </html>"
)

;; css file
(define css
  "body {
    background-color: #2b2b2b;
    color: #f8f8f2;
    font-family: Consolas, monospace;
    font-size: 14px;
    margin-left: 30px;
  }

  pre {
    margin: 0;
  }

  span {
    display: inline-block;
  }

  span.keyword {
    color: #ff0000;
  }

  span.operator {
    color: #d10000;
    margin-right: -7px;
    margin-left: -7px;
  }

  span.separator {
    color: #ffff00;
    margin-right: -7px;
    margin-left: -7px;
  }

  span.identifier {
    color: #cccccc;
  }

  span.number {
    color: #bf00ff;
  }

  span.string {
    color: #ffee58;
  }

  span.comment {
    color: #75715e;
  }

  span.System {
    color: #8453ff;
  }"
)

;; function to print lines in html
(define (print input-strings)
  (define multiline-comment #f)

  (define (process-string s)
    (when (not multiline-comment)
      (set! multiline-comment (regexp-match? #px"/\\*" s)))

    (define tokens (replace-all-tokens s multiline-comment))
    (define formatted-string (string-join tokens " "))

    (when multiline-comment
      (set! multiline-comment (not (regexp-match? #px"\\*/" s))))

    (string-append "<pre>" formatted-string "</pre>"))

  (define processed-strings (map process-string input-strings))
  (string-join processed-strings "")
)

;; function to generate html file
(define (generate-html input-strings)
  (write-file "output.html" 
    (string-append html-header (print input-strings) html-footer)
  )
)

;; function to generate css file
(define (generate-css)
  (write-file "style.css" 
    (string-append css)
  )
)

;; input file
(define input-file "input.cs")

;; output file
(define output-file "output.html")

;; css file
(define css-file "style.css")

;; Run
(define (Run)
  (define input-strings (file->lines input-file))
  (generate-html input-strings)
  (generate-css)
)

(time (Run))