#lang info

(define license 'BSD-3-Clause)
(define version "0.1")
(define collection "pkg")
(define deps '("base"))
(define raco-commands
  '(("profile-imports" (submod pkg/import-profiler main) "profile module dependency import times" #f)))
(define license 'BSD-3-Clause)
