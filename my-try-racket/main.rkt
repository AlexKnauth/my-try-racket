#lang racket/base

(require web-server/servlet
         web-server/servlet-env
         racket/sandbox
         racket/format
         racket/string
         racket/list
         )

(define ev
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-propagate-exceptions #f])
    (make-evaluator 'racket)))

(define already-text '())
(define (extend-already-text! . stuff)
  (set! already-text (append already-text stuff)))

(define (myresponse request)
  (define bindings (request-bindings request))
  (when (exists-binding? 'expr bindings)
    (define expr (extract-binding/single 'expr bindings))
    (extend-already-text! expr '(br))
    (eval-and-handle-expr! expr)
    )
  (extend-already-text! "> ")
  (response/xexpr
   `(html (head (title "My Try-Racket Repl, in progress")
                (style "body {font-family: monospace;}"
                       ".output {color: purple;}"
                       ".error-output {color: red;}"
                       ".value {color: blue;}"
                       ))
          (body
           (h1 "My Try-Racket Repl, in progress")
           ,@already-text
           (form (input ((name "expr") (autofocus "true"))))
           ))))

(define (eval-and-handle-expr! expr)
  (define lst (call-with-values (λ () (ev expr)) list))
  (define output (get-output ev))
  (define err-output (get-error-output ev))
  (define (string->lsthtml str)
    (define strs (string-split str "\n"))
    (append-map (λ (s) `(,s (br))) strs))
  (extend-already-text!
   `(span ((class "output")) ,@(string->lsthtml output))
   `(span ((class "error-output")) ,@(string->lsthtml err-output))
   `(span () ,@(for/list ([val (in-list lst)] #:when (not (void? val)))
                 `(span ((class "value")) ,@(string->lsthtml (~v val)))))))

(serve/servlet myresponse)
