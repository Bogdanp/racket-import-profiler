#lang info

(define version "0.1")
(define collection "pkg")
(define deps '("base"))
(define raco-commands
  '(("profile-imports" (submod pkg/import-profiler main) "profile module dependency import times" #f)))
