#!/usr/bin/env racket

#lang racket/base

(require "interpreter.rkt"
         rackunit/text-ui)

(for ([test-suite (list instruction-creation-tests
                        program-creation-tests
                        simple-execution-tests
                        add-arith-tests
                        sub-arith-tests
                        and-arith-tests
                        or-arith-tests
                        xor-arith-tests)])
  (run-tests test-suite 'verbose))
