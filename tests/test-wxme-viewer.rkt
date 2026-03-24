#lang racket/base

(require rackunit
         racket/set
         racket/match
         racket/path
         racket/runtime-path
         kettle/test
         (except-in kettle/program init)
         "../main.rkt")

(define-runtime-path examples-dir "examples")

;; ============================================================
;; Test helpers — synthetic snip-info structs (no real WXME objects needed)

(define (make-fake-snip-info [kind 'unknown] [desc "test snip"])
  (snip-info kind desc #f))

(define (make-test-elements)
  ;; Simulates: "line one\nline two\n" [image] "line three\n" [editor] "line four"
  (list "line one\nline two\n"
        (make-fake-snip-info 'image "image (100x50)")
        "line three\n"
        (make-fake-snip-info 'editor "nested editor")
        "line four"))

(define (make-test-viewer #:width [w 80] #:height [h 24])
  (make-wxme-viewer-from-elements
   (make-test-elements)
   "test.wxme" 1024
   '("wxtext" "wximage" "wxmedia") '("wxloc")
   #:width w #:height h))

;; ============================================================
;; Unit tests: elements->display

(test-case "elements->display produces correct lines from mixed elements"
  (define-values (lines anns snips)
    (elements->display (make-test-elements) (set)))
  ;; str1 "line one\nline two\n" -> "line one", "line two", ""  (3 lines)
  ;; snip1 -> "[image (100x50)]" (1 line)
  ;; str2 "line three\n" -> "line three", "" (2 lines)
  ;; snip2 -> "[nested editor]" (1 line)
  ;; str3 "line four" -> "line four" (1 line)
  ;; Total: 8 lines
  (check-equal? (vector-length lines) 8)
  (check-equal? (vector-ref lines 0) "line one")
  (check-equal? (vector-ref lines 1) "line two")
  (check-equal? (vector-ref lines 2) "")
  (check-equal? (vector-ref lines 3) "[image (100x50)]")
  (check-equal? (vector-ref lines 4) "line three")
  (check-equal? (vector-ref lines 5) "")
  (check-equal? (vector-ref lines 6) "[nested editor]")
  (check-equal? (vector-ref lines 7) "line four")
  (check-equal? (vector-length snips) 2))

;; Let me write a more careful test
(test-case "elements->display line count and content"
  (define elems (list "hello\nworld" (make-fake-snip-info 'image "img") "end"))
  (define-values (lines anns snips) (elements->display elems (set)))
  ;; "hello\nworld" -> "hello", "world"
  ;; snip -> "[img]"
  ;; "end" -> "end"
  (check-equal? (vector-length lines) 4)
  (check-equal? (vector-ref lines 0) "hello")
  (check-equal? (vector-ref lines 1) "world")
  (check-equal? (vector-ref lines 2) "[img]")
  (check-equal? (vector-ref lines 3) "end")
  ;; Annotations
  (check-equal? (line-ann-type (vector-ref anns 0)) 'text)
  (check-equal? (line-ann-type (vector-ref anns 1)) 'text)
  (check-equal? (line-ann-type (vector-ref anns 2)) 'snip-placeholder)
  (check-equal? (line-ann-snip-idx (vector-ref anns 2)) 0)
  (check-equal? (line-ann-type (vector-ref anns 3)) 'text)
  ;; One snip
  (check-equal? (vector-length snips) 1))

(test-case "elements->display with expanded snip inserts detail lines"
  (define si (make-fake-snip-info 'unknown "thing"))
  (define elems (list "before" si "after"))
  (define-values (lines anns snips) (elements->display elems (set 0)))
  ;; "before", "[thing]", detail lines..., "after"
  (check-true (> (vector-length lines) 3))
  (check-equal? (vector-ref lines 0) "before")
  (check-equal? (vector-ref lines 1) "[thing]")
  (check-equal? (line-ann-type (vector-ref anns 2)) 'snip-detail)
  ;; Last line is "after"
  (check-equal? (vector-ref lines (sub1 (vector-length lines))) "after"))

(test-case "elements->display with no elements"
  (define-values (lines anns snips) (elements->display '() (set)))
  (check-equal? (vector-length lines) 0)
  (check-equal? (vector-length snips) 0))

(test-case "elements->display with only text"
  (define-values (lines anns snips) (elements->display (list "a\nb\nc") (set)))
  (check-equal? (vector-length lines) 3)
  (check-equal? (vector-length snips) 0)
  (for ([ann (in-vector anns)])
    (check-equal? (line-ann-type ann) 'text)))

;; ============================================================
;; Unit tests: cursor movement

(test-case "move-cursor down increments cursor"
  (define wv (make-test-viewer))
  (define wv2 (update wv (key-msg #\j #f #f)))
  (check-equal? (wxme-viewer-cursor-line wv2) 1))

(test-case "move-cursor up at top stays at 0"
  (define wv (make-test-viewer))
  (define wv2 (update wv (key-msg #\k #f #f)))
  (check-equal? (wxme-viewer-cursor-line wv2) 0))

(test-case "move-cursor down clamps at last line"
  (define wv (make-test-viewer #:height 100))
  ;; Move down many times past end
  (define wv2
    (for/fold ([wv wv]) ([_ (in-range 100)])
      (update wv (key-msg #\j #f #f))))
  (check-equal? (wxme-viewer-cursor-line wv2)
                (sub1 (wxme-viewer-line-count wv2))))

(test-case "goto-top and goto-bottom"
  (define wv (make-test-viewer))
  ;; Move down a few
  (define wv2 (update (update (update wv (key-msg #\j #f #f))
                               (key-msg #\j #f #f))
                       (key-msg #\j #f #f)))
  (check-equal? (wxme-viewer-cursor-line wv2) 3)
  ;; goto top
  (define wv3 (update wv2 (key-msg #\g #f #f)))
  (check-equal? (wxme-viewer-cursor-line wv3) 0)
  ;; goto bottom
  (define wv4 (update wv3 (key-msg #\G #f #f)))
  (check-equal? (wxme-viewer-cursor-line wv4)
                (sub1 (wxme-viewer-line-count wv4))))

;; ============================================================
;; Unit tests: scrolling

(test-case "cursor scrolls view when going past visible area"
  (define wv (make-test-viewer #:height 5)) ; view-height = 3
  (check-equal? (wxme-viewer-y-offset wv) 0)
  ;; Move down past visible area
  (define wv2
    (for/fold ([wv wv]) ([_ (in-range 4)])
      (update wv (key-msg #\j #f #f))))
  ;; y-offset should have adjusted
  (check-true (> (wxme-viewer-y-offset wv2) 0)))

(test-case "horizontal scroll with h/l keys"
  (define wv (make-test-viewer))
  (define wv2 (update wv (key-msg #\l #f #f)))
  (check-true (>= (wxme-viewer-x-offset wv2) 0))
  (define wv3 (update wv2 (key-msg #\h #f #f)))
  (check-equal? (wxme-viewer-x-offset wv3) 0))

;; ============================================================
;; Unit tests: snip toggle

(test-case "toggle-snip-detail expands and collapses"
  (define wv (make-test-viewer))
  (define lc-before (wxme-viewer-line-count wv))
  ;; Move to first snip placeholder
  (define wv2 (update wv (key-msg 'tab #f #f)))
  (check-equal? (line-ann-type
                 (vector-ref (wxme-viewer-annotations wv2)
                             (wxme-viewer-cursor-line wv2)))
                'snip-placeholder)
  ;; Toggle expand
  (define wv3 (update wv2 (key-msg 'enter #f #f)))
  (check-true (> (wxme-viewer-line-count wv3) lc-before))
  ;; Toggle collapse
  (define wv4 (update wv3 (key-msg 'enter #f #f)))
  (check-equal? (wxme-viewer-line-count wv4) lc-before))

(test-case "enter on text line does nothing"
  (define wv (make-test-viewer))
  ;; cursor at 0, which is text
  (define wv2 (update wv (key-msg 'enter #f #f)))
  (check-equal? (wxme-viewer-line-count wv2)
                (wxme-viewer-line-count wv)))

;; ============================================================
;; Unit tests: snip navigation

(test-case "tab jumps to snip placeholders"
  (define wv (make-test-viewer))
  ;; First tab
  (define wv2 (update wv (key-msg 'tab #f #f)))
  (check-equal? (line-ann-type
                 (vector-ref (wxme-viewer-annotations wv2)
                             (wxme-viewer-cursor-line wv2)))
                'snip-placeholder)
  ;; Second tab
  (define wv3 (update wv2 (key-msg 'tab #f #f)))
  (check-not-equal? (wxme-viewer-cursor-line wv3)
                     (wxme-viewer-cursor-line wv2))
  (check-equal? (line-ann-type
                 (vector-ref (wxme-viewer-annotations wv3)
                             (wxme-viewer-cursor-line wv3)))
                'snip-placeholder))

(test-case "backtab jumps to previous snip"
  (define wv (make-test-viewer))
  ;; backtab from beginning wraps to last snip
  (define wv2 (update wv (key-msg 'backtab #f #f)))
  (check-equal? (line-ann-type
                 (vector-ref (wxme-viewer-annotations wv2)
                             (wxme-viewer-cursor-line wv2)))
                'snip-placeholder))

;; ============================================================
;; Unit tests: window resize

(test-case "window resize updates dimensions"
  (define wv (make-test-viewer #:width 80 #:height 24))
  (define wv2 (update wv (window-size-msg 120 40)))
  (check-equal? (wxme-viewer-width wv2) 120)
  (check-equal? (wxme-viewer-height wv2) 40))

;; ============================================================
;; Unit tests: quit

(test-case "q produces quit cmd"
  (define wv (make-test-viewer))
  (define result (update wv (key-msg #\q #f #f)))
  (check-true (cmd? result)))

;; ============================================================
;; E2E tests using kettle/test harness

(test-case "e2e: viewer displays text and snip placeholders"
  (define wv (make-test-viewer))
  (define tp (make-test-program wv))
  (check-test-program-running tp)
  (check-test-program-contains tp "line one")
  (check-test-program-contains tp "[image (100x50)]"))

(test-case "e2e: scrolling changes view"
  (define wv (make-test-viewer #:height 6))
  (define tp (make-test-program wv #:height 6))
  ;; Page down
  (test-program-press tp #\space)
  (check-test-program-running tp))

(test-case "e2e: snip toggle shows detail lines"
  (define wv (make-test-viewer))
  (define tp (make-test-program wv))
  ;; Navigate to first snip
  (test-program-press tp 'tab)
  ;; Toggle expand
  (test-program-press tp 'enter)
  (check-test-program-contains tp "Kind:")
  ;; Toggle collapse
  (test-program-press tp 'enter)
  ;; Detail lines should be gone — check line count went back
  (check-test-program-running tp))

(test-case "e2e: tab navigation between snips"
  (define wv (make-test-viewer))
  (define tp (make-test-program wv))
  (test-program-press tp 'tab)
  (define m1 (test-program-model tp))
  (test-program-press tp 'tab)
  (define m2 (test-program-model tp))
  (check-not-equal? (wxme-viewer-cursor-line m1)
                     (wxme-viewer-cursor-line m2)))

(test-case "e2e: q quits"
  (define wv (make-test-viewer))
  (define tp (make-test-program wv))
  (test-program-press tp #\q)
  (check-test-program-done tp))

(test-case "e2e: resize updates view"
  (define wv (make-test-viewer))
  (define tp (make-test-program wv))
  (test-program-resize tp 120 40)
  (define m (test-program-model tp))
  (check-equal? (wxme-viewer-width m) 120)
  (check-equal? (wxme-viewer-height m) 40)
  (check-test-program-running tp))

(test-case "e2e: header shows filename and metadata"
  (define wv (make-test-viewer))
  (define tp (make-test-program wv))
  (check-test-program-contains tp "test.wxme"))

;; ============================================================
;; E2E tests with real WXME files

(define (example-path name)
  (path->string (build-path examples-dir name)))

;; Helper: load a real WXME file, create viewer, run through basic interactions
(define (test-real-wxme-file name
                             #:expected-min-lines [min-lines 1]
                             #:expected-min-snips [min-snips 0]
                             #:expect-text [expect-text #f])
  (test-case (format "e2e real file: ~a" name)
    (define wv (make-wxme-viewer (example-path name)))

    ;; Basic model sanity
    (check-true (>= (wxme-viewer-line-count wv) min-lines)
                (format "expected at least ~a lines, got ~a"
                        min-lines (wxme-viewer-line-count wv)))
    (check-true (>= (vector-length (wxme-viewer-snip-infos wv)) min-snips)
                (format "expected at least ~a snips, got ~a"
                        min-snips (vector-length (wxme-viewer-snip-infos wv))))

    ;; E2E: create test program, verify it renders
    (define tp (make-test-program wv))
    (check-test-program-running tp)

    ;; Verify expected text appears in rendered view
    (when expect-text
      (check-test-program-contains tp expect-text))

    ;; Header shows filename
    (check-test-program-contains tp name)

    ;; Scroll down and back up
    (test-program-press tp #\j)
    (check-test-program-running tp)
    (test-program-press tp #\k)
    (check-test-program-running tp)

    ;; Page down and up
    (test-program-press tp #\space)
    (check-test-program-running tp)
    (test-program-press tp #\b)
    (check-test-program-running tp)

    ;; Goto bottom and top
    (test-program-press tp #\G)
    (check-test-program-running tp)
    (test-program-press tp #\g)
    (check-test-program-running tp)

    ;; If there are snips, test Tab navigation and toggle
    (when (> (vector-length (wxme-viewer-snip-infos wv)) 0)
      ;; Tab to first snip
      (test-program-press tp 'tab)
      (define m-at-snip (test-program-model tp))
      (check-equal? (line-ann-type
                     (vector-ref (wxme-viewer-annotations m-at-snip)
                                 (wxme-viewer-cursor-line m-at-snip)))
                    'snip-placeholder)

      ;; Expand snip detail
      (define lc-before (wxme-viewer-line-count m-at-snip))
      (test-program-press tp 'enter)
      (define m-expanded (test-program-model tp))
      (check-true (> (wxme-viewer-line-count m-expanded) lc-before)
                  "expanding snip should add detail lines")

      ;; Scroll down to make detail lines visible, then check
      (test-program-press tp #\j)
      (check-test-program-contains tp "Kind:")

      ;; Scroll back up to the snip-placeholder and collapse
      (test-program-press tp #\k)
      (test-program-press tp 'enter)
      (define m-collapsed (test-program-model tp))
      (check-equal? (wxme-viewer-line-count m-collapsed) lc-before
                    "collapsing should restore original line count"))

    ;; Resize
    (test-program-resize tp 100 30)
    (check-test-program-running tp)

    ;; Quit
    (test-program-press tp #\q)
    (check-test-program-done tp)))

;; --- Test each example file ---

(test-real-wxme-file "image.wxme"
                     #:expected-min-lines 2
                     #:expected-min-snips 2)

(test-real-wxme-file "commentbox.rkt"
                     #:expected-min-lines 5
                     #:expected-min-snips 1
                     #:expect-text "#lang racket/base")

(test-real-wxme-file "image-and-comment-box.rkt"
                     #:expected-min-lines 5
                     #:expected-min-snips 1
                     #:expect-text "define x")

(test-real-wxme-file "collapsed.rkt"
                     #:expected-min-lines 5
                     #:expect-text "#lang racket/base")

(test-real-wxme-file "color-red.rkt"
                     #:expected-min-lines 3
                     #:expected-min-snips 1
                     #:expect-text "require 2htdp/image")

(test-real-wxme-file "image-snip.rkt"
                     #:expected-min-lines 2
                     #:expected-min-snips 1
                     #:expect-text "#lang racket/base")

(test-real-wxme-file "number-snip.rkt"
                     #:expected-min-lines 2
                     #:expected-min-snips 1
                     #:expect-text "#lang racket")

(test-real-wxme-file "perform-whack.rkt"
                     #:expected-min-lines 20
                     #:expected-min-snips 1
                     #:expect-text "require")

(test-real-wxme-file "pict-snip.rkt"
                     #:expected-min-lines 3
                     #:expected-min-snips 1
                     #:expect-text "#lang racket/base")

(test-real-wxme-file "xml-snip-bug.rkt"
                     #:expected-min-lines 3
                     #:expected-min-snips 1
                     #:expect-text "#lang racket/gui")
