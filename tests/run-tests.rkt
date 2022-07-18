#!/usr/bin/env racket

#lang racket/base

(require "interpreter.rkt"
         rackunit/text-ui)

(run-tests instruction-creation-tests)
(run-tests program-creation-tests)
(run-tests simple-execution-tests)
(run-tests addition-flag-setting-tests)
(run-tests subtraction-flag-setting-tests)
