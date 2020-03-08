#lang racket

(require parser-tools/lex
         (prefix-in sre- parser-tools/lex-sre))

(define-lex-abbrevs
  (source-character (sre-+ (sre-: #\u0009 #\u000A #\u000D (sre-/ #\u0020 #\uFFFF))))
  (unicode-bom #\uFEFF)
  (whitespace (sre-or #\u0009 #\u0020))
  (line-terminator (sre-or #\u000A (sre-: #\u000D (sre-- #\u000A)) (sre-: #\u000D #\u000A)))
  (comment-char (sre-- source-character line-terminator))
  (comment (sre-: #\# (sre-? (sre-+ comment-char))))
  (comma #\,)
  (ignored (sre-or unicode-bom whitespace line-terminator comment comma))

  (lower (sre-/ #\a #\z))
  (upper (sre-/ #\A #\Z))
  (digit (sre-/ #\0 #\9))
  (nonzero-digit (sre-/ #\1 #\9))
  (sign (sre-or #\+ #\-))
  (negative-sign #\-)
  (punctuator (sre-or #\! #\$ #\( #\) "..." #\: #\= #\@ #\[ #\] #\{ #\| #\}))
  (name (sre-: (sre-or #\_ upper lower) (sre-* (sre-or #\_ digit upper lower))))

  (integer-part (sre-or (sre-: (sre-? negative-sign) #\0) (sre-: (sre-? negative-sign) nonzero-digit (sre-? (sre-+ digit)))))
  (int-value integer-part)

  (fractional-part (sre-: #\. (sre-+ digit)))
  (exponent-indicator (sre-or #\e #\E))
  (exponent-part (sre-: exponent-indicator (sre-? sign) (sre-+ digit)))
  (float-value
    (sre-or (sre-: integer-part fractional-part)
            (sre-: integer-part exponent-part)
            (sre-: integer-part fractional-part exponent-part)))

  (escaped-unicode (sre-= 4 (sre-or digit (sre-/ #\A #\F) (sre-/ #\a #\f))))
  (escaped-character (sre-or #\" #\\ #\/ #\b #\f #\n #\r #\t)) 
  (string-character
    (sre-or (sre-- source-character (sre-or #\" #\\ line-terminator))
            (sre-: #\u escaped-unicode)
            (sre-: #\\ escaped-character)))
  (block-quotation (sre-: #\" #\" #\"))
  (escaped-block-quotation (sre-: #\\ block-quotation))
  (block-string-character
    (sre-or (sre-- source-character (sre-or block-quotation escaped-block-quotation))
            escaped-block-quotation))
  (string-value
    (sre-or (sre-: #\" (sre-? (sre-+ string-character)) #\")
            (sre-: block-quotation (sre-? (sre-+ block-string-character)) block-quotation))))

(define gql-lexer
  (lexer
    [(eof)
     (cons (list 'EOF lexeme) '())]
    [punctuator
     (cons (list 'Punctuator lexeme) (gql-lexer input-port))]
    [name
     (cons (list 'Name lexeme) (gql-lexer input-port))]
    [int-value
     (cons (list 'IntValue lexeme) (gql-lexer input-port))]
    [int-value
     (cons (list 'IntValue lexeme) (gql-lexer input-port))]
    [float-value
     (cons (list 'FloatValue lexeme) (gql-lexer input-port))]
    [string-value
     (cons (list 'StringValue lexeme) (gql-lexer input-port))]
    [ignored
     (gql-lexer input-port)]))

