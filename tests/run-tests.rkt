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
                        xor-arith-tests
                        je-jump-tests
                        jne-jump-tests
                        jl-jump-tests
                        jle-jump-tests
                        jg-jump-tests
                        jge-jump-tests
                        jo-jump-tests
                        jno-jump-tests
                        jc-jump-tests
                        jnc-jump-tests)])
  (run-tests test-suite 'verbose))
