#lang racket

(require json 
         "spin.rkt"
         )

(define h (make-hash))

(define (getvalue req)
  (define tag (params req 'tag))
  ; (printf "~nGET TAG: ~a~n" tag)
  (jsexpr->string (list "VALUE" tag (hash-ref h tag "'not found'")))
  )

(define (extended-handler? tag)
  (member tag (list "delete")))
  
(define (handle-extended-tag tag value)
  (match tag
    ["delete"
     ; (printf "~nDELETE: ~a (has-key? ~a) ~n" value (string-length value))
     ; (printf "string? ~a symbol? ~a~n" (string? value) (symbol? value))
     (when (hash-has-key? h value)
       (hash-remove! h value)
       )])
  (jsexpr->string (list "VALUE" tag value)))

(define (storeavalue req)
  (define tag (params req 'tag))
  (define value (string->jsexpr (params req 'value)))
  (cond
    [(extended-handler? tag)
     (handle-extended-tag tag value)]
    [else
     ; (printf "~nSTORE TAG: ~a (string? ~a) VALUE: ~a~n" tag (string? tag) value)
     (hash-set! h tag value)
     ; (printf "HASH: ~a~n" h)
     (jsexpr->string (list "VALUE" tag value))]))

;; ROUTES
(post "/getvalue" getvalue)
(post "/storeavalue" storeavalue)

(run #:listen-ip "0.0.0.0")
