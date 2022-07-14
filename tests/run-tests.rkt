#!/usr/bin/env racket

#lang racket/base

(require "interpreter.rkt"
         rackunit/text-ui)

(run-tests instruction-creation-tests)
(run-tests program-creation-tests)
(run-tests simple-execution-tests)
(run-tests flag-setting-tests)
