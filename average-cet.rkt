#lang racket

;;; File from http://hadobs.metoffice.com/hadcet/cetml1659on.dat
;;; via https://www.trevorharley.com/weather.html
;;;

(require racket/string
         plot)

(define-syntax-rule (with-open-input-file (in file) form ...)
  (call-with-input-file file
    (λ (in) form ...)))

(define (tokenize-file f)
  ;; You need to prune the header by hand
  (define this-year (pregexp (format "^[[:space:]]*~A"
                               (date-year (seconds->date (current-seconds))))))
  (with-open-input-file (in f)
    (for/list ([l (in-lines in)]
               #:when (and (regexp-match?
                            #px"^[[:space:]]*[[:digit:]]{4}[[:space:]]"
                            l)
                           (not (regexp-match this-year l))))
      (for/list ([e (in-list (string-split l))])
        (string->number e)))))

(define (summer-averages d #:since (since #f))
  (for/list ([yl (in-list d)]
             #:when (or (not since) (>= (first yl) since)))
    (match-let ([(list y _ _ _ _ _ june july august _ _ _ _ _) yl])
      (list y (/ (+ (* june 30) (* july 31) (* august 31)) 92)))))

(define (year-averages d #:since (since #f))
  (for/list ([yl (in-list d)]
             #:when (or (not since) (>= (first yl) since)))
    (list (first yl) (last yl))))

(plot-font-family 'modern)

(define (plot-summer-averages f #:since (since #f))
  (plot (lines (summer-averages (tokenize-file f) #:since since))
        #:title "Summer (JJA) CE average temperature"
        #:x-label "year"
        #:y-label "temperature"))

(define (plot-year-averages f #:since (since #f))
  (plot (lines (year-averages (tokenize-file f) #:since since))
        #:title "CE average temperature over year"
        #:x-label "year"
        #:y-label "temperature"))

(define (at-below-hottest f
                           #:since (since #f)
                           #:averages (averager summer-averages))
  (let* ([averages (averager (tokenize-file f) #:since since)]
         [hottest (second (argmax second averages))])
    (for/list ([a (in-list averages)])
      (list (first a) (- hottest (second a))))))

(define (years-within-hottest ab within)
  (define hottest (second (argmin second ab)))
  (filter (λ (yd)
            (<= (second yd) within))
          ab))

(define (counts-within-hottest ab within)
  (define hy (first (argmin second ab)))
  (let loop ([yt ab]
             [cb 0] [yb 0]
             [ca 0] [ya 0])
    (match yt
      ['() (values cb yb ca ya)]
      [(list-rest (list y b) ytt)
       (cond [(< y hy)
              (loop ytt
                    (if (<= b within) (+ cb 1) cb) (+ yb 1)
                    ca ya)]
             [(> y hy)
              (loop ytt
                    cb yb
                    (if (<= b within) (+ ca 1) ca) (+ ya 1))]
             [else
              (loop ytt cb yb ca ya)])])))

(define (chances-within-hottest ab within)
  (let-values ([(cb yb ca ya) (counts-within-hottest ab within)])
    (values (/ cb yb) (/ ca ya))))

(define (%-chance-within-hottest ab within)
  (let-values ([(bc ac) (chances-within-hottest ab within)])
    (values (* bc 100.0) (* ac 100.0) (* 1.0 (/ ac bc)))))
