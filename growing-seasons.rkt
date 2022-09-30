#lang racket

;;;; An attempt at growing season lengths
;;; CET daily data from
;;; https://www.metoffice.gov.uk/hadobs/hadcet/data/meantemp_daily_totals.txt
;;; replacing legacy file (old format) from
;;; https://www.metoffice.gov.uk/hadobs/hadcet/legacy/data/cetdl1772on.dat
;;;
;;; Both via https://www.metoffice.gov.uk/hadobs/hadcet/index.html
;;;
;;; start of growing season is 5 days with temperature above 5, end
;;; is 5 days with temperature below 5, starting from July
;;;

(require plot
         srfi/17
         racket/generator
         "util.rkt")

(set! (setter vector-ref) vector-set!)

(module+ test
  (require rackunit))

(define data-file (make-parameter "meantemp_daily_totals.txt"))

;;;; Turning the file into a reasonable format
;;; Lines in the file are YYYY-MM-DD temp
;;;

(define (tokenize-file (f (data-file)))
  ;; Read the file and return the obvious structure as a list of
  ;; (yyyy mm dd temperatyre) lists.
  (unless (or (file-exists? f)
              (system* (find-executable-path "make") f #:set-pwd? #t))
    (error 'tokenize-file "no file ~A and could not make it" f))
  (define good-line
    #px"^[[:space:]]*([[:digit:]]{4})-([[:digit:]]{2})-([[:digit:]]{2})\
[[:space:]]+([-+]?[[:digit:]]+(?:[.][[:digit:]]+))[[:space:]]*$")
  ;; a good line is a yyyy-mm-dd x.y
  (with-open-input-file (in f)
    (for/fold ([matched '()] #:result (reverse matched))
              ([line (in-lines in)])
      (match line
        [(pregexp good-line (list* _ vals))
         (cons (map string->number vals) matched)]
        [_
         matched]))))

(define (yearify entries)
  ;; Turn a flat list of (year month day temp) into a list of entries
  ;; of the form (year . entries) where entries is a vector od days
  ;; and each day is (day-number month day temp) & day-number is the
  ;; day in the year.
  (for/fold ([years '()]
             [entries-in-current-year '()]
             [current-year (first (first entries))]
             [current-year-day 1]
             #:result (reverse (if (< current-year-day 365)
                                   ;; the current year is short or
                                   ;; hasn't started: ignore it
                                   years
                                   (cons
                                    (cons current-year
                                          (list->vector
                                           (reverse entries-in-current-year)))
                                    years))))
            ([entry (in-list entries)])
    (match-let ([(list year month day temperature) entry])
      (if (eqv? year current-year)
          (values years
                  (cons (list current-year-day month day temperature)
                        entries-in-current-year)
                  year
                  (+ current-year-day 1))
          (values (cons (cons current-year
                              (list->vector
                               (reverse entries-in-current-year)))
                        years)
                  (list (list 1 month day temperature))
                  year
                  2)))))

;;;; Finding the start and end of seasons
;;;

(define (find-first-n-matching pred? n v i (l (vector-length v)) (s 1))
  ;; Return the first index of v from i where pred becomes true for
  ;; the following n entries
  (if (= i l)
      #f
      (let search-here? ([j i]
                         [m n])
        (cond
          [(zero? m)
           i]
          [(or (>= j l) (< j 0))
           #f]
          [(pred? (vector-ref v j))
           (search-here? (+ j s) (- m 1))]
          [else
           (find-first-n-matching pred? n v (+ i s) l s)]))))

(module+ test
  (let ([v #(1 2 1 2 2 3 2 2)])
    (check-eqv?
     (find-first-n-matching (λ (e) (= e 2)) 2
                            v 0)
     3)
    (check-false
     (find-first-n-matching (λ (e) (= e 2)) 3
                            v 0))
    (check-eqv?
     (find-first-n-matching (λ (e) (= e 2)) 2
                            v 3)
     3)
    (check-eqv?
     (find-first-n-matching (λ (e) (= e 2)) 2
                            v 4)
     6)))

(define (growing-seasons yeared
                         #:start-temp (start-temp 5)
                         #:n-start (n-start 5)
                         #:end-temp (end-temp 5)
                         #:n-end (n-end 5)
                         #:end-from-fraction (end-from-fraction 7/12))
  ;; Return list of (yyyy start end)
  (define day-temp fourth)
  (for/list ([year yeared])
    (match-let ([(cons yyyy entries) year])
      (let* ([l (vector-length entries)]
             [l/2 (round (* l end-from-fraction))]
             [start (find-first-n-matching
                     (λ (day)
                       (>= (day-temp day) start-temp))
                     n-start entries 0 l)]
             [end (and start
                       (find-first-n-matching
                        (λ (day)
                          (< (day-temp day) end-temp))
                        n-end entries (max start l/2) l))])
        (list yyyy start (or end l))))))

(define (growing-season-lengths file)
  ;; Compose all the above to produce a vector of (yyyy l)
  (for/vector ([gs (in-list (--> file
                               tokenize-file
                               yearify
                               growing-seasons))]
               #:when (second gs))
    (match-let ([(list yyyy start end) gs])
      (list yyyy (- end start)))))

;;;; Smoothing seasons
;;;

(define (moving-average v over
                        #:iterations (iterations 1)
                        #:import (import identity)
                        #:export (export identity)
                        #:exact (exact #f)
                        #:start (start (min (max (- over 1) 1)
                                            (vector-length v)))
                        #:end (end (vector-length v)))
  ;; Compute the trailing moving average of a vector, trying to be
  ;; clever about the leading elements: by default they're left as is
  ;; but you can ask to average over fewer elements at the start of
  ;; the vector.
  (unless (> over 0)
    (error 'moving-average "over must be positive"))
  (unless (<= start end)
    (error 'moving-average "start > end"))
  (unless (> start 0)
    (error 'moving-average
           "can't average zeroth element (or start is negative)"))
  (cond
    [(or (zero? iterations) (= start end))
     (export (import v))]
    [(> iterations 0)
     (define l (vector-length v))
     (define o (- over 1))
     (define w (if exact over (exact->inexact over)))
     (export
      (for/last ([i (in-range iterations)]
                 [(vs vt) (in-generator
                           (let swap ([va (import v)]
                                      [vb (make-vector l)])
                             (yield va vb)
                             (swap vb va)))])
        ;; Copy the unsmoothed parts of the source into the target
        (for ([j (in-range 0 start)])
          (set! (vector-ref vt j) (vector-ref vs j)))
        (when (< end l)
          (for ([j (in-range (+ end 1) l)])
            (set! (vector-ref vt j) (vector-ref vs j))))
        (when (< start o)
          ;; Deal with the special case at the start of the vector
          (for ([j (in-range start o)])
            (set! (vector-ref vt j)
                  (/ (for/sum ([k (in-inclusive-range (max 0 (- j o)) j)])
                       (vector-ref vs k))
                     (if exact (+ j 1) (exact->inexact (+ j 1)))))))
        ;; Deal with the boring case of the rest of the vector
        (for ([j (in-range o end)])
          (set! (vector-ref vt j)
                (/ (for/sum ([k (in-inclusive-range (- j o) j)])
                     (vector-ref vs k))
                   w)))
        vt))]
    [else
     (error 'moving-average "negative creep")]))

(module+ test
  (check-equal?
   ;; leading element is not touched
   (moving-average #(12) 1 #:exact #t)
   #(12))
  (check-equal?
   ;; averaging over 1 does nothing
   (moving-average #(12 2 3 4) 1 #:exact #t)
   #(12 2 3 4))
  (check-equal?
   ;; second element is average of first two
   (moving-average #(12 0) 2 #:exact #t)
   #(12 6))
  (check-equal?
   ;; general case for average over 2
   (moving-average #(12 0 2 0 0) 2 #:exact #t)
   (vector 12 (/ (+ 12 0) 2) (/ (+ 0 2) 2) (/ (+ 2 0) 2) (/ (+ 0 0) 2)))
  (check-equal?
   ;; average over 3
   (moving-average #(12 0 2 3) 3 #:exact #t)
   (vector 12 0 (/ (+ 12 0 2) 3) (/ (+ 0 2 3) 3)))
  (check-equal?
   ;; average over 3, starting from 1
   (moving-average #(12 0 2 3) 3 #:exact #t #:start 1)
   (vector 12 6 (/ (+ 12 0 2) 3) (/ (+ 0 2 3) 3))))

(define (smooth-growing-season-lengths gsv
                                       #:over (over 5)
                                       #:iterations (iterations 1))
  ;; From a vector (yyyy l) return a vector of (yyy sl)
  (moving-average gsv over
                  #:iterations iterations
                  #:start 1
                  #:import (λ (v)
                              (for/vector ([e (in-vector v)])
                                (second e)))
                  #:export (λ (v)
                             (for/vector ([s (in-vector v)]
                                          [e (in-vector gsv)])
                               (list (first e) s)))))


(plot-font-family 'modern)

(define (plot-growing-seasons (f (data-file))
                              #:since (since #f)
                              #:before (before #f)
                              #:average-over (average-over 5)
                              #:iterations (iterations 1)
                              #:to (to #f))
  (define raw (growing-season-lengths f))
  ((if to (curryr plot-file to) plot)
   (list
    (lines (for/list ([s (in-vector raw)]
                      #:when (and (or (not since) (>= (first s) since))
                                  (or (not before) (< (first s) before))))
             s)
           #:alpha 0.3
           #:label "raw")
    (lines (for/list ([s (in-vector
                         (smooth-growing-season-lengths
                          raw
                          #:over average-over
                          #:iterations iterations))]
                     #:when (and (or (not since) (>= (first s) since))
                                 (or (not before) (< (first s) before))))
             s)
           #:label (format "moving average over ~S, ~S iterations"
                     average-over iterations)))
   #:title (format "CET growing season lengths from ~S to ~S"
             (or since (first (vector-ref raw 0)))
             (if before
                 (- before 1)
                 (first (vector-ref raw (- (vector-length raw) 1)))))
   #:x-label "year"
   #:y-label "length in days"))
