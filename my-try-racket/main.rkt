#lang racket/base

(require web-server/servlet
         web-server/servlet-env
         racket/sandbox
         racket/format
         racket/string
         racket/list
         racket/local
         racket/match
         )

(define (start request)
  (my-response #:ev (make-ev) #:request request #:already-text '() #:in-progress-text ""))

(define (make-ev)
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-propagate-exceptions #f])
    (make-evaluator 'racket)))

;; make-page/already-text+prompt+action-url : lsthtml lsthtml action-url -> html
(define (make-page/already-text+prompt+action-url already-text prompt action-url)
  `(html (head (title "My Try-Racket Repl, in progress")
               (style "body {font-family: monospace; font-size: 12pt}"
                      ".output {color: purple;}"
                      ".error-output {color: red;}"
                      ".value {color: blue;}"
                      ".input {font-family: monospace; width: 80%; font-size: 12pt;}"
                      ))
         (body
          (h1 "My Try-Racket Repl, in progress")
          ,@already-text
          (form ((action ,action-url))
                ,@prompt
                (input ((name "expr") (autofocus "true") (class "input"))))
          )))

(define (string->lsthtml str)
  (define strs (string-split str "\n"))
  (append-map (λ (s) `(,s (br))) strs))

(define (my-response #:request request
                     #:ev ev
                     #:already-text already-text ; lsthtml
                     #:in-progress-text in-progress-text) ; string
  (define bindings (request-bindings request))
  (define-values (new-already-text new-in-progress-text new-prompt)
    (cond [(exists-binding? 'expr bindings)
           (define expr (string-append in-progress-text (extract-binding/single 'expr bindings) "\n"))
           (define stx-exprs (str-expr->stx-exprs? expr))
           (cond [stx-exprs
                  (values (append already-text
                                  (string->lsthtml expr)
                                  (eval-stx-exprs->lsthtml stx-exprs ev))
                          ""
                          '("> "))]
                 [else (values already-text expr '())])]
          [else (values already-text "" '("> "))]))
  (define (response-generator embed/url)
    (response/xexpr
     (make-page/already-text+prompt+action-url
      (append new-already-text (string->lsthtml new-in-progress-text))
      new-prompt
      (embed/url
       (λ (request) (my-response #:request request #:ev ev
                                 #:already-text (append new-already-text new-prompt)
                                 #:in-progress-text new-in-progress-text))))))
  (send/suspend/dispatch response-generator))

(define (str-expr->stx-exprs? str-expr)
  (define (read/exn in)
    (with-handlers ([exn:fail:read? (λ (x) x)])
      (read-syntax 'my-try-racket-repl in)))
  (define exprs
    (for/list ([expr (in-port read/exn (open-input-string str-expr))])
      expr))
  (cond [(ormap exn:fail:read? exprs) #f]
        [else exprs]))
  

(define (eval-stx-exprs->lsthtml stx-exprs ev)
  (append*
   (for/list ([expr (in-list stx-exprs)])
     (define lst (call-with-values (λ () (ev expr)) list))
     (define output (get-output ev))
     (define err-output (get-error-output ev))
     (list
      `(span ((class "output")) ,@(string->lsthtml output))
      `(span ((class "error-output")) ,@(string->lsthtml err-output))
      `(span () ,@(for/list ([val (in-list lst)] #:when (not (void? val)))
                    `(span ((class "value")) ,@(string->lsthtml (~v val)))))))))

(serve/servlet start)
