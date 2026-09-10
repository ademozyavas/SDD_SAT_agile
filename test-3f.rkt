#lang racket

(require "circuit.rkt"
         "universe.rkt"
         "sat-vars.rkt")

(build-sat-universe
 sample-c17
 3
 '(0 1))

(dump-sat-vars)