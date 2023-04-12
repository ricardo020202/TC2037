;; Jose Ricardo Rosales Castañeda
;; Dante David Perez Perez

;Resaltador de sinrtaxis para C#

#lang racket

(require 2htdp/batch-io)

;; keywords from c#
(define keyword
  (list 
    "abstract" "as" "base" "bool" "break" "byte" "case" "catch" "char" 
    "checked" "class" "const" "continue" "decimal" "default" "delegate" 
    "do" "double" "else" "endif" "enum" "event" "explicit" "extern" 
    "false" "finally" "fixed" "float" "for" "foreach" "goto" "if" 
    "implicit" "in" "int" "interface" "internal" "is" "lock" "long" 
    "namespace" "new" "null" "object" "operator" "out" "override" 
    "params" "private" "protected" "public" "readonly" "ref" "return"
    "sbyte" "sealed" "short" "sizeof" "stackalloc" "static" "string"
    "struct" "switch" "this" "throw" "true" "try" "typeof" "uint"
    "ulong" "unchecked" "unsafe" "ushort" "using" "virtual" "void"
    "volatile" "while" "#if" "#endif" "#else" "Console"
   )
)

;; operators from c#
(define operator
    (list
        "+" "-" "*" "/" "%" "^" "&" "|" "~" "!" "=" "<" ">" "?" 
        ":" ";" "," "." "++" "--" "&&" "||" "==" "!=" "<=" ">="
        "+=" "-=" "*=" "/=" "%=" "^=" "&=" "|=" "<<=" ">>=" "=>"
        "??"
    )
)

;; separators from c#
(define separator
  '(";" "," "." "(" ")" "[" "]" "{" "}" "<" ">" ":" "::" "..." "=>" "??"))

;; input file
(define input-file "input.cs")

;; output file
(define output-file "output.html")

;; html header
(define html-header
  "<!DOCTYPE html>
<html>
<head>
  <meta charset=\"utf-8\">
  <title>Resaltador de sintaxis</title>
  <link rel=\"stylesheet\" href=\"style.css\">
</head>
<body>
  <pre>")
  
;; html footer
(define html-footer
  "</pre>
</body>
</html>")

;; function to replace keywords
(define (replace-keyword s)
  (string-append "<span class=\"keyword\">" s "</span>"))

;; function to replace operators
(define (replace-operator s)
  (string-append "<span class=\"operator\">" s "</span>"))

;; function to replace separators
(define (replace-separator s)
  (string-append "<span class=\"separator\">" s "</span>"))

;; function to replace numbers
(define (replace-number s)
  (string-append "<span class=\"number\">" s "</span>"))

;; function to replace strings
(define (replace-string s)
  (string-append "<span class=\"string\">" s "</span>"))

;; function to replace comments
(define (replace-comment s)
  (string-append "<span class=\"comment\">" s "</span>"))

;; function to replace identifiers
(define (replace-identifier s)
  (string-append "<span class=\"identifier\">" s "</span>"))

;; function to categorize tokens
(define (categorize-tokens s)
  (cond
    [(member s keyword) (replace-keyword s)]
    [(member s operator) (replace-operator s)]
    [(member s separator) (replace-separator s)]
    [(regexp-match #rx"^[0-9x]+$"  s) (replace-number s)]
    [(regexp-match #rx"^\".*\"$" s) (replace-string s)]
    [(regexp-match #rx"//." s) (replace-comment s)]
    [(regexp-match #rx"/*.*/" s) (replace-comment s)]
    [(regexp-match #rx"^[a-zA-Z_][a-zA-Z0-9_]*$" s) (replace-identifier s)]
    [else s]))

;; function to tokenize a given string
(define (tokenize s)
  (regexp-split #rx"[ \t \n .]+" s))

;; function to replace all tokens
(define (replace-all-tokens s)
  (string-join (map categorize-tokens (tokenize s)) " "))

;; function to read input file
(define (read-input-file)
  (read-file input-file))

;; function to write output file
(define (write-output-file s)
  (write-file output-file s))

;; function to generate html file
(define (generate-html-file)
  (write-output-file
    (string-append html-header
      (replace-all-tokens (read-input-file))
      html-footer)))

;; function to generate css file
(define (generate-css-file)
  (write-file "style.css"
    "body {
  background-color: #2b2b2b;
  color: #f8f8f2;
  font-family: Consolas, monospace;
  font-size: 14px;
  line-height: 1.5;
}

pre {
  margin: 0;
}

span {
  display: inline-block;
}

span.keyword {
  color: #f92672;
}

span.operator {
  color: #66d9ef;
}

span.separator {
  color: yellow;
}

span.identifier {
  color: #a6e22e;
}

span.number {
  color: #ae81ff;
}

span.string {
  color: #e6db74;
}

span.comment {
  color: #75715e;
}"))

;; function to generate output file
(define (generate-output-file)
  (generate-html-file)
  (generate-css-file))

;; function to run program
(define (run)
  (generate-output-file))

(run)
