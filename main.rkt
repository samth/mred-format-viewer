#lang racket/base

(require racket/class
         racket/format
         racket/list
         racket/match
         racket/port
         racket/set
         racket/string
         racket/file
         wxme
         wxme/image
         wxme/editor
         (except-in kettle/program init)
         kettle/image
         kettle/style)

;; ============================================================
;; Data structures

(provide (struct-out snip-info)
         (struct-out line-ann)
         (struct-out wxme-viewer)
         make-wxme-viewer
         make-wxme-viewer-from-elements
         elements->display
         snip-detail-lines)

;; Metadata about a non-text snip
(struct snip-info (kind description original) #:transparent)

;; Annotation for a single display line
(struct line-ann (type snip-idx) #:transparent)

;; Main model
(struct wxme-viewer
  (elements        ; (listof (or/c string? snip-info?))
   lines           ; vector of strings
   annotations     ; vector of line-ann
   snip-infos      ; vector of snip-info
   line-count      ; exact-nonneg-integer?
   max-line-w      ; exact-nonneg-integer?
   filename        ; string
   file-size       ; exact-nonneg-integer?
   snip-classes    ; (listof string)
   data-classes    ; (listof string)
   y-offset        ; scroll position
   x-offset        ; horizontal scroll
   width           ; terminal width
   height          ; terminal height
   gutter-w        ; line-number gutter width
   expanded-snips  ; (setof exact-nonneg-integer?)
   cursor-line)    ; current cursor position
  #:transparent
  #:methods gen:kettle-model
  [(define (init wv) wv)
   (define (update wv msg) (wxme-viewer-update wv msg))
   (define (view wv) (wxme-viewer-view wv))])

;; ============================================================
;; WXME reading

(define (classify-snip obj)
  (cond
    [(is-a? obj image%) 'image]
    [(is-a? obj editor%) 'editor]
    [else 'unknown]))

(define (describe-snip obj kind)
  (match kind
    ['image
     (define w (send obj get-w))
     (define h (send obj get-h))
     (define data (send obj get-data))
     (cond
       [(and (> w 0) (> h 0))
        (format "image (~ax~a)" (inexact->exact (round w)) (inexact->exact (round h)))]
       [data
        (format "image (~a bytes)" (bytes-length data))]
       [else "image"])]
    ['editor "nested editor"]
    ['unknown (format "snip: ~a" (~a obj))]))

(define (capture-snip obj)
  (define kind (classify-snip obj))
  (snip-info kind (describe-snip obj kind) obj))

(define (snip-detail-lines si)
  (define obj (snip-info-original si))
  (cond
    ;; No original object (e.g., synthetic test data)
    [(not obj)
     (list (format "  Kind: ~a" (snip-info-kind si))
           (format "  Description: ~a" (snip-info-description si)))]
    [else
     (match (snip-info-kind si)
       ['image
        (define data (send obj get-data))
        (define w (send obj get-w))
        (define h (send obj get-h))
        (filter
         values
         (list "  Kind: image"
               (if (and (> w 0) (> h 0))
                   (format "  Dimensions: ~ax~a"
                           (inexact->exact (round w))
                           (inexact->exact (round h)))
                   #f)
               (format "  Offset: (~a, ~a)" (send obj get-dx) (send obj get-dy))
               (if data
                   (format "  Data: ~a bytes (inline)" (bytes-length data))
                   "  Data: file reference")))]
       ['editor
        (define p (send obj get-content-port))
        (define preview
          (with-handlers ([exn:fail? (lambda (_) "(unreadable)")])
            (define s (read-string 80 p))
            (if (eof-object? s)
                "(empty)"
                (string-append (string-replace s "\n" "\\n") "..."))))
        (list "  Kind: nested editor"
              (format "  Content preview: ~a" preview))]
       [_
        (list (format "  Kind: ~a" (snip-info-kind si))
              (format "  Description: ~a" (snip-info-description si)))])]))

(define (read-decoded-port port)
  (let loop ([accum '()] [current-bytes #f])
    (define v (read-byte-or-special port))
    (cond
      [(eof-object? v)
       (reverse (if current-bytes
                    (cons (finish-text current-bytes) accum)
                    accum))]
      [(byte? v)
       (define out (or current-bytes (open-output-bytes)))
       (write-byte v out)
       (loop accum out)]
      [else
       (define accum2 (if current-bytes
                          (cons (finish-text current-bytes) accum)
                          accum))
       (cond
         [(snip-info? v)
          (loop (cons v accum2) #f)]
         [else
          ;; Wrap unexpected specials
          (loop (cons (snip-info 'unknown (~a v) v) accum2) #f)])])))

(define (finish-text out)
  (bytes->string/utf-8 (get-output-bytes out) #\?))

;; ============================================================
;; Elements -> display lines

(define (elements->display elements expanded-snips)
  ;; Returns (values lines-vec annotations-vec snip-infos-vec)
  (define snip-infos '())
  (define snip-idx 0)
  (define lines '())
  (define anns '())

  (for ([elem (in-list elements)])
    (cond
      [(string? elem)
       (define parts (string-split elem "\n" #:trim? #f))
       (for ([part (in-list parts)])
         (set! lines (cons part lines))
         (set! anns (cons (line-ann 'text #f) anns)))]
      [(snip-info? elem)
       (define idx snip-idx)
       (set! snip-infos (cons elem snip-infos))
       (set! snip-idx (add1 snip-idx))
       ;; Placeholder line
       (set! lines (cons (format "[~a]" (snip-info-description elem)) lines))
       (set! anns (cons (line-ann 'snip-placeholder idx) anns))
       ;; Detail lines if expanded
       (when (set-member? expanded-snips idx)
         (for ([detail (in-list (snip-detail-lines elem))])
           (set! lines (cons detail lines))
           (set! anns (cons (line-ann 'snip-detail idx) anns))))]))

  (values (list->vector (reverse lines))
          (list->vector (reverse anns))
          (list->vector (reverse snip-infos))))

;; ============================================================
;; Constructor

(define (compute-gutter-w line-count)
  (+ 1 (string-length (number->string (max 1 line-count)))))

(define (compute-max-line-w lines-vec)
  (for/fold ([maxw 0]) ([line (in-vector lines-vec)])
    (max maxw (string-length line))))

(define (make-wxme-viewer-from-elements elements filename file-size
                                        snip-classes data-classes
                                        #:width [w 80] #:height [h 24])
  (define-values (lines-vec anns-vec snip-infos-vec)
    (elements->display elements (set)))
  (define lc (vector-length lines-vec))
  (define maxw (compute-max-line-w lines-vec))
  (define gw (compute-gutter-w lc))
  (wxme-viewer elements lines-vec anns-vec snip-infos-vec
               lc maxw filename file-size
               snip-classes data-classes
               0 0 w h gw
               (set) 0))

(define (make-wxme-viewer filename
                          #:width [w 80] #:height [h 24])
  (define raw-port (open-input-file filename))
  (unless (is-wxme-stream? raw-port)
    (close-input-port raw-port)
    (error 'make-wxme-viewer "not a WXME file: ~a" filename))

  ;; Extract class info
  (file-position raw-port 0)
  (define-values (snip-classes data-classes)
    (parameterize ([unknown-extensions-skip-enabled #t])
      (extract-used-classes raw-port)))

  ;; Decode content
  (file-position raw-port 0)
  (define decoded-port
    (parameterize ([unknown-extensions-skip-enabled #t])
      (wxme-port->port raw-port #t capture-snip)))

  (define elements (read-decoded-port decoded-port))
  (define fsize (file-size filename))

  (make-wxme-viewer-from-elements elements filename fsize
                                  snip-classes data-classes
                                  #:width w #:height h))

;; ============================================================
;; Scrolling helpers

(define (view-height wv)
  (- (wxme-viewer-height wv) 2))

(define (max-y-offset wv)
  (max 0 (- (wxme-viewer-line-count wv) (view-height wv))))

(define (clamp-y wv offset)
  (max 0 (min offset (max-y-offset wv))))

(define (max-x-offset wv)
  (define content-w (- (wxme-viewer-width wv) (wxme-viewer-gutter-w wv) 3))
  (max 0 (- (wxme-viewer-max-line-w wv) content-w)))

(define (clamp-x wv offset)
  (max 0 (min offset (max-x-offset wv))))

(define (scroll-percent wv)
  (define maxoff (max-y-offset wv))
  (if (<= maxoff 0)
      100
      (quotient (* 100 (wxme-viewer-y-offset wv)) maxoff)))

;; ============================================================
;; Cursor movement

(define (ensure-cursor-visible wv)
  (define cursor (wxme-viewer-cursor-line wv))
  (define y-off (wxme-viewer-y-offset wv))
  (define vh (view-height wv))
  (cond
    [(< cursor y-off)
     (struct-copy wxme-viewer wv [y-offset cursor])]
    [(>= cursor (+ y-off vh))
     (struct-copy wxme-viewer wv [y-offset (- cursor vh -1)])]
    [else wv]))

(define (move-cursor wv delta)
  (define new-cursor (max 0 (min (sub1 (wxme-viewer-line-count wv))
                                 (+ (wxme-viewer-cursor-line wv) delta))))
  (ensure-cursor-visible
   (struct-copy wxme-viewer wv [cursor-line new-cursor])))

(define (page-down wv)
  (define vh (view-height wv))
  (move-cursor wv vh))

(define (page-up wv)
  (define vh (view-height wv))
  (move-cursor wv (- vh)))

(define (goto-top wv)
  (ensure-cursor-visible
   (struct-copy wxme-viewer wv [cursor-line 0])))

(define (goto-bottom wv)
  (ensure-cursor-visible
   (struct-copy wxme-viewer wv
                [cursor-line (sub1 (wxme-viewer-line-count wv))])))

;; ============================================================
;; Snip navigation and toggle

(define (jump-to-next-snip wv)
  (define lc (wxme-viewer-line-count wv))
  (define anns (wxme-viewer-annotations wv))
  (define start (add1 (wxme-viewer-cursor-line wv)))
  ;; Search forward from cursor, wrapping around
  (let loop ([i start] [wrapped? #f])
    (cond
      [(and wrapped? (>= i start)) wv] ; no snips found
      [(>= i lc) (loop 0 #t)]
      [(eq? 'snip-placeholder (line-ann-type (vector-ref anns i)))
       (ensure-cursor-visible
        (struct-copy wxme-viewer wv [cursor-line i]))]
      [else (loop (add1 i) wrapped?)])))

(define (jump-to-prev-snip wv)
  (define lc (wxme-viewer-line-count wv))
  (define anns (wxme-viewer-annotations wv))
  (define start (sub1 (wxme-viewer-cursor-line wv)))
  (let loop ([i start] [wrapped? #f])
    (cond
      [(and wrapped? (<= i start)) wv]
      [(< i 0) (loop (sub1 lc) #t)]
      [(eq? 'snip-placeholder (line-ann-type (vector-ref anns i)))
       (ensure-cursor-visible
        (struct-copy wxme-viewer wv [cursor-line i]))]
      [else (loop (sub1 i) wrapped?)])))

(define (rebuild-lines wv)
  (define expanded (wxme-viewer-expanded-snips wv))
  (define-values (lines-vec anns-vec snip-infos-vec)
    (elements->display (wxme-viewer-elements wv) expanded))
  (define lc (vector-length lines-vec))
  (define maxw (compute-max-line-w lines-vec))
  (define gw (compute-gutter-w lc))
  (define cursor (min (wxme-viewer-cursor-line wv) (sub1 (max 1 lc))))
  (define wv2
    (struct-copy wxme-viewer wv
                 [lines lines-vec]
                 [annotations anns-vec]
                 [snip-infos snip-infos-vec]
                 [line-count lc]
                 [max-line-w maxw]
                 [gutter-w gw]
                 [cursor-line cursor]))
  (ensure-cursor-visible
   (struct-copy wxme-viewer wv2
                [y-offset (clamp-y wv2 (wxme-viewer-y-offset wv2))]
                [x-offset (clamp-x wv2 (wxme-viewer-x-offset wv2))])))

(define (toggle-snip-detail wv)
  (define cursor (wxme-viewer-cursor-line wv))
  (define ann (vector-ref (wxme-viewer-annotations wv) cursor))
  (cond
    [(eq? 'snip-placeholder (line-ann-type ann))
     (define idx (line-ann-snip-idx ann))
     (define expanded (wxme-viewer-expanded-snips wv))
     (define new-expanded
       (if (set-member? expanded idx)
           (set-remove expanded idx)
           (set-add expanded idx)))
     (rebuild-lines
      (struct-copy wxme-viewer wv [expanded-snips new-expanded]))]
    [else wv]))

;; ============================================================
;; Styles

(define header-style (make-style #:bold #t #:reverse #t))
(define footer-style (make-style #:faint #t))
(define gutter-style (make-style #:faint #t))
(define snip-style (make-style #:foreground fg-cyan #:bold #t))
(define selected-snip-style (make-style #:foreground fg-cyan #:reverse #t #:bold #t))
(define detail-style (make-style #:foreground fg-yellow #:faint #t))
(define cursor-style (make-style #:foreground fg-green #:bold #t))

;; ============================================================
;; Update

(define (wxme-viewer-update wv msg)
  (match msg
    [(key-msg key alt? ctrl?)
     (match key
       [#\q (cmd wv (quit-cmd))]

       [(or 'down #\j) (move-cursor wv +1)]
       [(or 'up #\k) (move-cursor wv -1)]

       [(or #\space 'page-down) (page-down wv)]
       [(or #\b 'page-up) (page-up wv)]

       [#\g (goto-top wv)]
       [#\G (goto-bottom wv)]

       [(or 'left #\h)
        (struct-copy wxme-viewer wv
                     [x-offset (clamp-x wv (- (wxme-viewer-x-offset wv) 4))])]
       [(or 'right #\l)
        (struct-copy wxme-viewer wv
                     [x-offset (clamp-x wv (+ (wxme-viewer-x-offset wv) 4))])]

       ['enter (toggle-snip-detail wv)]

       ['tab (jump-to-next-snip wv)]
       ['backtab (jump-to-prev-snip wv)]

       [_ wv])]

    [(window-size-msg w h)
     (define wv2 (struct-copy wxme-viewer wv [width w] [height h]))
     (struct-copy wxme-viewer wv2
                  [y-offset (clamp-y wv2 (wxme-viewer-y-offset wv2))]
                  [x-offset (clamp-x wv2 (wxme-viewer-x-offset wv2))])]

    [_ wv]))

;; ============================================================
;; View

(define (pad-line-number n gutter-w)
  (~a (add1 n) #:min-width (sub1 gutter-w) #:align 'right))

(define (visible-portion line x-offset content-w)
  (define len (string-length line))
  (define start (min x-offset len))
  (define end (min (+ start content-w) len))
  (if (>= start len) "" (substring line start end)))

(define (truncate-to-width str w)
  (if (<= (string-length str) w) str (substring str 0 w)))

(define (wxme-viewer-view wv)
  (define w (wxme-viewer-width wv))
  (define h (wxme-viewer-height wv))
  (define vh (view-height wv))
  (define lines (wxme-viewer-lines wv))
  (define anns (wxme-viewer-annotations wv))
  (define lc (wxme-viewer-line-count wv))
  (define y-off (wxme-viewer-y-offset wv))
  (define x-off (wxme-viewer-x-offset wv))
  (define gw (wxme-viewer-gutter-w wv))
  (define content-w (max 1 (- w gw 3)))
  (define cursor (wxme-viewer-cursor-line wv))
  (define snip-count (vector-length (wxme-viewer-snip-infos wv)))

  (define visible-start y-off)
  (define visible-end (min (+ visible-start vh) lc))
  (define actual-visible (- visible-end visible-start))

  ;; Header
  (define header-text
    (truncate-to-width
     (format " ~a  ~a lines  ~a snips  ~a bytes "
             (wxme-viewer-filename wv) lc snip-count
             (wxme-viewer-file-size wv))
     w))

  ;; Content lines
  (define line-images
    (for/list ([i (in-range visible-start visible-end)])
      (define line (vector-ref lines i))
      (define ann (vector-ref anns i))
      (define at-cursor? (= i cursor))
      (define num-str (pad-line-number i gw))
      (define content (visible-portion line x-off content-w))

      (define gutter-img (styled gutter-style (text num-str)))
      (define indicator
        (if at-cursor?
            (styled cursor-style (text ">"))
            (text " ")))
      (define content-img
        (match (line-ann-type ann)
          ['text (text content)]
          ['snip-placeholder
           (styled (if at-cursor? selected-snip-style snip-style)
                   (text content))]
          ['snip-detail
           (styled detail-style (text content))]))

      (hcat 'top gutter-img (text " ") indicator (text " ") content-img)))

  ;; Padding for short files
  (define pad-images (make-list (max 0 (- vh actual-visible)) (text "")))

  ;; Footer
  (define pct (scroll-percent wv))
  (define footer-text
    (truncate-to-width
     (format " ~a%  j/k:scroll  Enter:toggle  Tab:next snip  g/G:top/bot  q:quit" pct)
     w))

  (apply vcat 'left
         (styled header-style (text header-text))
         (append line-images pad-images
                 (list (styled footer-style (text footer-text))))))

;; ============================================================
;; Main

(module+ main
  (require racket/cmdline)
  (define filename
    (command-line
     #:program "mred-format-viewer"
     #:args (file)
     file))
  (define wv (make-wxme-viewer filename))
  (define p (make-program wv #:alt-screen #t))
  (program-run p))
