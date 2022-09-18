#lang racket/base

(require racket/place)
(provide main)

(define (main ch)
  (define mod-path (place-channel-get ch))
  (with-handlers ([exn:fail? (λ (e) (place-channel-put ch (exn-message e)))])
    (define start-time (current-inexact-monotonic-milliseconds))
    (dynamic-require mod-path #f)
    (place-channel-put ch (- (current-inexact-monotonic-milliseconds) start-time))))
