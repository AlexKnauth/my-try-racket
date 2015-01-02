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

(define in-progress-text "")
(define (extend-in-progress-text! . strs)
  (set! in-progress-text (string-append* in-progress-text strs)))

(define (myresponse request)
  (define bindings (request-bindings request))
  (when (exists-binding? 'expr bindings)
    (define expr (extract-binding/single 'expr bindings))
    (extend-already-text! expr '(br))
    (eval-and-handle-expr! expr)
    )
  (when (string=? in-progress-text "")
    (extend-already-text! "> "))
  (response/xexpr
   `(html (head (title "My Try-Racket Repl, in progress")
                (style "body {font-family: monospace;}"
                       ".output {color: purple;}"
                       ".error-output {color: red;}"
                       ".value {color: blue;}"
                       ".input {font-family: monospace; width: 100%;}"
                       ))
          (body
           (h1 "My Try-Racket Repl, in progress")
           ,@already-text
           (form (input ((name "expr") (autofocus "true") (class "input"))))
           ))))

(define (eval-and-handle-expr! expr)
  (extend-in-progress-text! expr "\n")
  (define (read/exn in)
    (with-handlers ([exn:fail:read? (λ (x) x)])
      (read-syntax 'my-try-racket-repl in)))
  (define exprs
    (for/list ([expr (in-port read/exn (open-input-string in-progress-text))])
      expr))
  (cond [(ormap exn:fail:read? exprs) (void)]
        [else
         (set! in-progress-text "")
         (for ([expr (in-list exprs)])
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
                          `(span ((class "value")) ,@(string->lsthtml (~v val))))))
           )]))

(serve/servlet myresponse)
