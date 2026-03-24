#lang info

(define collection "mred-format-viewer")
(define version "0.1")
(define deps '("base" "wxme-lib" "kettle-lib"))
(define build-deps '("rackunit-lib" "kettle-test-lib"))

(define pkg-desc "Terminal viewer for DrRacket's WXME binary file format")
(define pkg-authors '(samth))
(define license 'MIT)
