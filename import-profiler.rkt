#lang racket/base

(require racket/format
         racket/list
         racket/match)

(define (~duration d)
  (if (> d 1000)
      (~a (~r #:precision '(= 2) (/ d 1000)) "s")
      (~a (~r #:precision '(= 2) d) "ms")))

(define (~mod-path mod-path [max-len 60])
  (match mod-path
    [`(quote ,_) mod-path]
    [`(submod ,mod-path ,submod-name)
     `(submod ,(~mod-path mod-path) ,submod-name)]
    [_
     (define p-str (path->string mod-path))
     (define p-len (string-length p-str))
     (if (> p-len max-len)
         (~a "..." (substring p-str (- p-len max-len)))
         p-str)]))

(define (display-timings tree timings start [min-duration 5])
  (let loop ([start start] [depth 0])
    (define indent (make-string (* 2 depth) #\space))
    (define (display-timing fmt . args)
      (displayln (~a indent (apply format fmt args))))
    (display-timing "* ~a (~a)" (~mod-path start) (~duration (hash-ref timings start)))
    (define deps-with-timings
      (filter
       (λ (dep) (hash-has-key? timings dep))
       (hash-ref tree start null)))
    (define sorted-deps
      (sort deps-with-timings > #:key (λ (dep) (hash-ref timings dep))))
    (for* ([dep (in-list sorted-deps)]
           [duration (in-value (hash-ref timings dep))]
           #:when (> duration min-duration))
      (loop dep (add1 depth)))))

(define (display-leaf-timings tree timings [min-duration 5])
  (define leaves
    (for*/fold ([tree (hash)]
                #:result (for/hash ([(mod-path dependents) (in-hash tree)])
                           (values mod-path (remove-duplicates dependents))))
               ([(mod-path dependencies) (in-hash tree)]
                [dep (in-list dependencies)]
                #:when (null? (hash-ref tree dep null)))
      (hash-update tree dep (λ (dependents) (cons mod-path dependents)) null)))
  (define weights
    (for/hash ([(mod-path dependents) (in-hash leaves)])
      (define weight (/ (hash-ref timings mod-path 0) (length dependents)))
      (values mod-path weight)))
  (define total-weight
    (apply + (hash-values weights)))
  (define scaled-weights
    (for/hash ([(mod-path weight) (in-hash weights)])
      (values mod-path (/ weight total-weight))))
  (define sorted-mods
    (sort (hash-keys weights) > #:key (λ (mod-path) (hash-ref scaled-weights mod-path 0))))
  (for ([mod-path (in-list sorted-mods)])
    (define duration (hash-ref timings mod-path 0))
    (when (> duration min-duration)
      (printf "* ~a (~a, weight ~a)~n"
              (~mod-path mod-path)
              (~duration duration)
              (~r (hash-ref scaled-weights mod-path 0))))))

(module+ main
  (require racket/cmdline
           "import-profiler/profiler.rkt")

  (define leaves-only? #f)
  (define max-depth 2)
  (define min-duration 5)
  (define mode 'place)
  (define time-per-mod 2000)
  (define verbose? #f)
  (define module-path
    (command-line
     #:once-each
     [("-l" "--leaves") "only print timings for leaf nodes" (set! leaves-only? #t)]
     [("-D" "--max-depth") MAX-DEPTH "the max dependency tree recursion depth"
                           (define max-depth-n (string->number MAX-DEPTH))
                           (unless (and max-depth-n (> max-depth-n 0))
                             (eprintf "error: --max-depth must be a positive integer~n")
                             (exit 1))
                           (set! max-depth max-depth-n)]
     [("-d" "--min-duration") MIN-DURATION ""
                              (define min-duration-n (string->number MIN-DURATION))
                              (unless (and min-duration-n (> min-duration-n 0))
                                (eprintf "error: --min-duration must be a positive integer~n")
                                (exit 1))
                              (set! min-duration min-duration-n)]
     [("-m" "--mode") MODE "the measurement mode ('place' or 'process')"
                      (define mode-sym (string->symbol MODE))
                      (unless (memq mode-sym '(place process))
                        (eprintf "error: --mode must be 'place' or 'process'~n")
                        (exit 1))
                      (set! mode mode-sym)]
     [("-t" "--time-per-mod") TIME-PER-MOD "the amount of time (in milliseconds) to spend profiling each module"
                              (define time-per-mod-n (string->number TIME-PER-MOD))
                              (unless (and time-per-mod-n (> time-per-mod-n 0))
                                (eprintf "error: --time-per-mod must be a positive integer")
                                (exit 1))
                              (set! time-per-mod time-per-mod-n)]
     [("-v" "--verbose") "turn on verbose output" (set! verbose? #t)]
     #:args [MODULE-PATH]
     (string->symbol MODULE-PATH)))
  (define (status fmt . args)
    (when verbose?
      (apply printf fmt args)))
  (define-values (tree start-path)
    (module-path->dependency-tree
     #:max-depth max-depth
     #:status-printf status
     module-path))
  (unless (hash-has-key? tree start-path)
    (eprintf "error: module not found~n")
    (exit 1))

  (printf "Profiling ~a discovered modules. This will take approx ~a...~n"
          (hash-count tree)
          (~duration (* (hash-count tree) time-per-mod)))
  (define timings
    (for/hash ([mod-path (in-hash-keys tree)])
      (status " * Timing ~a~n" (~mod-path mod-path))
      (values mod-path (measure
                        #:mode mode
                        #:time-per-mod time-per-mod
                        #:status-printf status
                        mod-path))))

  (printf "Timings:~n")
  (if leaves-only?
      (display-leaf-timings tree timings min-duration)
      (display-timings tree timings start-path min-duration)))
