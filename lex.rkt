#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-lex-abbrevs
  (source-character (:or #\u0009 #\u000A #\u000D (:/ #\u0020 #\uFFFF)))
  (unicode-bom #\uFEFF)
  (whitespace (:or #\u0009 #\u0020))
  (line-terminator (:or #\u000A (:: #\u000D (:- source-character #\u000A)) (:: #\u000D #\u000A)))
  (comment-char (:- source-character line-terminator))
  (comment (:: #\# (:* comment-char)))
  (comma #\,)
  (ignored (:or unicode-bom whitespace line-terminator comment comma))

  (lower (:/ #\a #\z))
  (upper (:/ #\A #\Z))
  (digit (:/ #\0 #\9))
  (nonzero-digit (:/ #\1 #\9))
  (sign (:or #\+ #\-))
  (negative-sign #\-)
  (punctuator (:or #\! #\$ #\( #\) "..." #\: #\= #\@ #\[ #\] #\{ #\| #\}))
  (name (:: (:or #\_ upper lower) (:* (:or #\_ digit upper lower))))

  (integer-part (:or (:: (:? negative-sign) #\0) (:: (:? negative-sign) nonzero-digit (:? (:+ digit)))))
  (int-value integer-part)

  (fractional-part (:: #\. (:+ digit)))
  (exponent-indicator (:or #\e #\E))
  (exponent-part (:: exponent-indicator (:? sign) (:+ digit)))
  (float-value
    (:or (:: integer-part fractional-part)
         (:: integer-part exponent-part)
         (:: integer-part fractional-part exponent-part)))

  (escaped-unicode (:= 4 (:or digit (:/ #\A #\F) (:/ #\a #\f))))
  (escaped-character (:or #\" #\\ #\/ #\b #\f #\n #\r #\t)) 
  (string-character
    (:or (:- source-character (:or #\" #\\ line-terminator))
         (:: #\u escaped-unicode)
         (:: #\\ escaped-character)))
  (block-quotation (:: #\" #\" #\"))
  (escaped-block-quotation (:: #\\ block-quotation))
  (block-string-character
    (:or (:- source-character (:or block-quotation escaped-block-quotation))
         escaped-block-quotation))
  (string-value
    (:or (:: #\" (:? (:+ string-character)) #\")
         (:: block-quotation (:? (:+ block-string-character)) block-quotation))))

(define gql-lexer
  (lexer
    [ignored
     (gql-lexer input-port)]
    [(eof)
     (cons (list 'EOF lexeme) '())]
    [punctuator
     (cons (list 'Punctuator lexeme) (gql-lexer input-port))]
    [name
     (cons (list 'Name lexeme) (gql-lexer input-port))]
    [int-value
     (cons (list 'IntValue lexeme) (gql-lexer input-port))]
    [float-value
     (cons (list 'FloatValue lexeme) (gql-lexer input-port))]
    [string-value
     (cons (list 'StringValue lexeme) (gql-lexer input-port))]))

