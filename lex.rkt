#lang racket

(require parser-tools/lex
         (prefix-in sre- parser-tools/lex-sre))

;(define-tokens t (Punctuator Name IntValue FloatValue StringValue))
;(define-empty-tokens et (UnicodeBOM WhiteSpace LineTerminator Comment Comma))

(define-lex-abbrev lower (char-range #\a #\z))
(define-lex-abbrev upper (char-range #\A #\Z))
(define-lex-abbrev digit (char-range #\0 #\9))
(define-lex-abbrev nonzero-digit (char-range #\1 #\9))

(define gql-lexer
  (lexer
    [(eof)
     (cons (list 'EOF lexeme) '())]
    [(sre-or #\! #\$ #\( #\) "..." #\: #\= #\@ #\[ #\] #\{ #\| #\})
     (cons (list 'Punctuator lexeme) (gql-lexer input-port))]
    [(sre-: (sre-or "_" upper lower) (sre-* (sre-or "_" digit upper lower)))
     (cons (list 'Name lexeme) (gql-lexer input-port))]
    [(sre-or (sre-or "-0" #\0) (sre-: (sre-? #\-) nonzero-digit (sre-+ digit)))
     (cons (list 'IntValue lexeme) (gql-lexer input-port))]
    [digit
     (cons (list 'Digit lexeme) (gql-lexer input-port))]
    [nonzero-digit
     (cons (list 'NonZeroDigit lexeme) (gql-lexer input-port))]
    ;[#rx"/[0-9A-Fa-f]{4}/"
    ; (cons (list 'EscapedUnicode lexeme) (gql-lexer input-port))]
    [(sre-or #\" #\\ #\/ #\b #\f #\n #\r #\t)
     (cons (list 'EscapedCharacter lexeme) (gql-lexer input-port))]
    [whitespace 
     (cons (list 'WhiteSpace lexeme) (gql-lexer input-port))]
    [#\,
     (cons (list 'Comma lexeme) (gql-lexer input-port))]))

