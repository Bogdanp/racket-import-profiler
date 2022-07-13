#lang racket/base

(require racket/contract/base
         racket/list
         racket/match
         racket/place
         racket/port
         racket/runtime-path
         racket/system
         syntax/modresolve)

(provide
 complete-module-path/c
 (contract-out
  [module-path->dependency-tree
   (->* (module-path?)
        (#:max-depth exact-positive-integer?
         #:status-printf (-> string? any/c ... void?))
        (values
         (hash/c complete-module-path/c (listof complete-module-path/c))
         complete-module-path/c))]
  [measure
   (->* (complete-module-path/c)
        (#:mode (or/c 'place 'process)
         #:time-per-mod exact-positive-integer?
         #:status-printf (-> string? any/c ... void?))
        real?)]))

(define complete-module-path/c
  (or/c symbol?
        (and/c path? complete-path?)
        (list/c 'quote symbol?)
        (list/c 'submod (or/c symbol? complete-path?) symbol?)))

(define (module-path->dependency-tree start-mod-path
                                      #:max-depth [max-depth 2]
                                      #:status-printf [status eprintf])
  (define tree
    (make-hash))
  (define start (resolve-module-path start-mod-path))
  (let loop ([mod-path start]
             [depth 0])
    (let/ec esc
      (unless (or (> depth max-depth) (hash-has-key? tree mod-path))
        (with-handlers* ([exn:fail?
                          (λ (e)
                            (status "module-path->dependency-tree: failed to load module ~s~n error: ~a~n" mod-path (exn-message e))
                            (esc))])
          (dynamic-require mod-path #f))
        (define path
          (match mod-path
            [`(quote ,_) #f]
            [`(submod ,path ,_) path]
            [_ mod-path]))
        (for ([phase-list (in-list (module->imports mod-path))])
          (define dependencies
            (parameterize ([current-load-relative-directory (if path
                                                                (simplify-path (build-path path 'up))
                                                                (current-load-relative-directory))])
              (for/list ([dep (in-list (cdr phase-list))])
                (define resolved-dep
                  (resolved-module-path-name (module-path-index-resolve dep)))
                (cond
                  [(symbol? resolved-dep) `(quote ,resolved-dep)]
                  [(list? resolved-dep) `(submod ,@resolved-dep)]
                  [else resolved-dep]))))
          (hash-update! tree mod-path (λ (es) (remove-duplicates (append dependencies es))) null)
          (for-each (λ (dep) (loop dep (add1 depth))) dependencies)))))
  (define res-tree
    (for/hash ([(mod-path dependencies) (in-hash tree)])
      (values mod-path dependencies)))
  (values res-tree start))

(define-runtime-path worker.rkt "worker.rkt")
(define (measure-once/place mod-path [_status eprintf])
  (define ch (dynamic-place worker.rkt 'main))
  (define duration-or-err
    (place-channel-put/get ch mod-path))
  (when (string? duration-or-err)
    (raise duration-or-err))
  duration-or-err)

(define this-racket (find-executable-path (find-system-path 'exec-file)))
(define (measure-once/process mod-path [status eprintf])
  (let/ec esc
    (define expr
      (match mod-path
        [`(quote ,name)
         `(require ,name)]
        [`(submod ,(? (and path? complete-path?) path) ,(? symbol? submod-name))
         `(require (submod ,(path->string path) ,submod-name))]
        [(? (and path? complete-path?) path)
         `(require (file ,(path->string path)))]
        [_
         (status "measure-once/process: cannot measure ~s via process~n" mod-path)
         (esc 0)]))
    (define expr-str
      (call-with-output-string
       (lambda (out)
         (write expr out))))
    (define start-time (current-inexact-monotonic-milliseconds))
    (system* this-racket "-l" "racket/base" "-e" expr-str)
    (- (current-inexact-monotonic-milliseconds) start-time)))

(define (measure mod-path
                 #:mode [mode 'place]
                 #:time-per-mod [time-per-mod 2000]
                 #:status-printf [status-printf eprintf])
  (define start-time
    (current-inexact-monotonic-milliseconds))
  (define timings
    (for/list ([_ (in-naturals)])
      #:final (> (- (current-inexact-monotonic-milliseconds) start-time) time-per-mod)
      (case mode
        [(place) (measure-once/place mod-path status-printf)]
        [(process) (measure-once/process mod-path status-printf)]
        [else (raise-argument-error 'measure "(or/c 'place 'process)" mode)])))
  (/ (apply + timings) (length timings)))
