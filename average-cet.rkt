#lang racket

;;; File from
;;; https://www.metoffice.gov.uk/hadobs/hadcet/data/meantemp_monthly_totals.txt
;;; replacing the legacy file from
;;; https://www.metoffice.gov.uk/hadobs/hadcet/legacy/data/cetml1659on.dat
;;; Both via https://www.metoffice.gov.uk/hadobs/hadcet/index.html
;;; originally via https://www.trevorharley.com/weather.html
;;; (See Makefile which will fetch them)
;;;

(require racket/string
         plot
         "util.rkt")

(module+ test
  (require rackunit))

(define data-file (make-parameter "meantemp_monthly_totals.txt"))

(define (tokenize-file (f (data-file)))
  (unless (or (file-exists? f)
              (system* (find-executable-path "make") f #:set-pwd? #t))
    (error 'tokenize-file "no file ~A and could not make it" f))
  (define this-year (pregexp (format "^[[:space:]]*~A"
                               (date-year (seconds->date (current-seconds))))))
  (define missing-data (pregexp (regexp-quote "-99.9")))
  (define good-line
    ;; a good line starts with a date and then has 13 floats
    #px"^[[:space:]]*[[:digit:]]{4}\
([[:space:]]+[-+]?[[:digit:]]+\\.[[:digit:]]+){13}")
  (let ([sequence
          (with-open-input-file (in f)
            (for/list ([l (in-lines in)]
                       #:when (and (regexp-match?
                                    good-line
                                    l)
                                   (not (regexp-match missing-data l))
                                   (not (regexp-match this-year l))))
              (for/list ([e (in-list (string-split l))])
                (string->number e))))])
    (let-values ([(valid bad-year) (validate-tf-year-sequence sequence)])
      (unless valid
        (error 'tokenize-file "sequence break at ~A" bad-year)))
    sequence))

(define (validate-tf-year-sequence tf)
  ;; Validate the year sequence
  (let loop ([last-year (first (first tf))]
             [tft (rest tf)])
    (match tft
      ['() (values #t #f)]
      [(list* (list* this-year _) ntft)
       (if (= this-year (+ last-year 1))
           (loop this-year ntft)
           (values #f last-year))]
      [_
       (error 'validate-tf-year-sequence "what?")])))

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
(plot-width (round (* (plot-width) 4/3))) ;?

(define (plot-summer-averages (f (data-file)) #:since (since #f)
                              #:to (to #f))
  ((if to (curryr plot-file to) plot)
   (lines (summer-averages (tokenize-file f) #:since since))
   #:title "Summer (JJA) CE average temperature"
   #:x-label "year"
   #:y-label "temperature"))

(define (plot-year-averages (f (data-file)) #:since (since #f)
                            #:to (to #f))
  ((if to (curryr plot-file to) plot)
   (lines (year-averages (tokenize-file f) #:since since))
   #:title "CE average temperature over year"
   #:x-label "year"
   #:y-label "temperature"))

(define (at-below-hottest (f (data-file))
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


;;; Decadal averages (or averages over a period)
;;;

(struct ya
  ;; a yearly average
  (year average months)
  #:transparent)

(define (vectorify-years years #:since (since #f)
                         #:summer (summer #f))
  ;; Turn a list of years into a vector of ya objects.
  ;; since is the start year if given.
  ;; summer means average only june july august.  This is replicating code from
  ;; above, which sucks.
  (for/vector ([yl (in-list years)]
               #:when (or (not since) (>= (first yl) since)))
    (match-let ([(list year january february march april may june
                       july august september
                       october november december average) yl])
      (if summer
          (ya year (/ (+ (* june 30) (* july 31) (* august 31)) 92)
              (vector june july august))
          (ya year average
              (vector january february march april may june
                      july august september
                      october november december))))))

(define (average-vector v n (vf identity))
  ;; Return a vector which averages the last n elements of v,
  ;; extracting values with vf
  (define l (vector-length v))
  (define (sum-stream index current)
    (cond
      [(< index l)
       (stream-cons current (sum-stream
                             (+ index 1)
                             (+ (- current (vf (vector-ref v (- index n))))
                                (vf (vector-ref v index)))))]
      [(= index l)
       (stream-cons current empty-stream)]
      [else
       (error 'sum-stream "bad index")]))
  (for/vector ([s (in-stream (sum-stream n
                                         (for/sum ([i (in-range n)])
                                           (vf (vector-ref v i)))))])
    (/ s n)))

(module+ test
  (test-case
   "average vector"
   (let ([v #(1 2 3 4)])
     (check-equal? (average-vector v 1) v)
     (check-equal? (average-vector v 2) #(3/2 5/2 7/2)))
   (let* ([vl #(1 2 3 7 7 7 7)]
          [vla (average-vector vl 3)])
     (check-eqv? (vector-length vla) (- (vector-length vl) 2))
     (check-eqv? (vector-ref vla (- (vector-length vla) 1))
                 (vector-ref vl (- (vector-length vl) 1))))))

(define (plot-decadal-averages (f (data-file))
                               #:since (since #f)
                               #:decade (decade 10)
                               #:to (to #f)
                               #:t-min (t-min #f)
                               #:t-max (t-max #f)
                               #:annual (annual #t)
                               #:summer (summer #f))
  ;; Plot decadal averages where a 'decade' can be any length of time
  (define yvs (vectorify-years (tokenize-file f)
                               #:since (if since (- since (- decade 1)) #f)))
  (define svs (vectorify-years (tokenize-file f)
                               #:since (if since (- since (- decade 1)) #f)
                               #:summer #t))
  (define start (ya-year (vector-ref yvs (- decade 1))))
  (unless (or (not since)
            (= start since))
    (error 'plot-decadal-averages "~A is too early: start=~A" since start))
  ((if to (curryr plot-file to) plot)
   (append
    (if annual
        (list (lines (for/vector ([a (in-vector (average-vector yvs decade
                                                                ya-average))]
                                  [y (in-naturals start)])
                       (list y a))
                     #:color "green"
                     #:label "annual average"))
        '())
    (if summer
        (list (lines (for/vector ([a (in-vector (average-vector svs decade
                                                                ya-average))]
                                  [y (in-naturals start)])
                       (list y a))
                     #:color "gold"
                     #:label "summer average"))
        '()))
   #:title (case decade
             ((1) (format "CE temperatures since ~A" start))
             ((10) (format "CE decadal averages from ~A)" start))
             (else (format "CE averages from ~A, averaged over ~A years"
                     start decade)))
   #:x-label "year"
   #:y-label (case decade
               ((1) (format "average temperature"))
               (else (format "average temperature, last ~A years" decade)))
   #:y-min t-min
   #:y-max t-max))


;;; Decaying averages
;;;

(define (decaying-average-vector v d (vf identity))
  ;; What should this do about zero-length vectors?
  (define l (vector-length v))
  (define d-1/d (/ (- d 1) d))
  (define (da-stream index current)
    (cond
      [(< index l)
       (stream-cons current
                    (da-stream (+ index 1)
                               (+ (/ (vf (vector-ref v index))
                                     d)
                                  (* current d-1/d))))]
      [(= index l)
       (stream-cons current empty-stream)]
      [else
       (error 'da-stream "bad index")]))
  (for/vector ([s (in-stream (da-stream 1 (vf (vector-ref v 0))))])
    s))

(module+ test
  (test-case
   "decaying average vector"
   (let ([v #(1)])
     (check-equal? (decaying-average-vector v 10) v))
   (let ([v #(1 2 3)])
     (check-equal? (decaying-average-vector v 1) v)
     (check-equal? (decaying-average-vector v 2)
                   #(1 3/2 9/4)))
   (let* ([vl #(1 2 3 4 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)]
          [vla (decaying-average-vector vl 2)])
     (check-not-eqv? (vector-ref vl (- (vector-length vl) 1))
                     (vector-ref vla (- (vector-length vla) 1))))))

(define (plot-decaying-averages (f (data-file))
                                #:decay (decay 10)
                                #:since (since #f)
                                #:decade (decade 10)
                                #:to (to #f)
                                #:t-min (t-min #f)
                                #:t-max (t-max #f))
  ;; Plot decaying averages where the decay is how long to remember
  (define yvs (vectorify-years (tokenize-file f) #:since since))
  (define start (or since (ya-year (vector-ref yvs 0))))
  ((if to (curryr plot-file to) plot)
   (lines (for/vector ([a (in-vector (decaying-average-vector yvs decay
                                                              ya-average))]
                       [y (in-naturals start)])
            (list y a)))
   #:title (format "CE decaying averages since ~A (decay=~A)" start decay)
   #:x-label "year"
   #:y-label (format "average temperature, decayed over ~A years" decade)
   #:y-min t-min
   #:y-max t-max))


;;; Monthly averages
;;;

(define (average-monthly-temperatures yvs i0 i1)
  ;; Compute average monthly temperatures between i0 (inclusive)
  ;; and i1 (exclusive)
  (for/fold ([sums (make-vector 12 0.0)]
             #:result (let ([years (- i1 i0)])
                        (for ([m (in-range 12)])
                          (vector-set! sums m
                                       (/ (vector-ref sums m) years)))
                        sums))
            ([yv (in-vector yvs i0 i1)])
    (for ([m (in-naturals)]
          [t (in-vector (ya-months yv))])
      (vector-set! sums m (+ (vector-ref sums m) t)))
    sums))

(struct mat
  ;; monthly averaged temperatures over a range of years
  ;; The end year is exclusive
  (start-year end-year averages))

(define (monthly-averages yvs
                          #:start-year (start-year #f)
                          #:end-year (end-year #f)
                          #:average-over (average-over 1))
  ;; Compute a list of mat objects
  (define yvl (vector-length yvs))
  (define i0 (if (not start-year)
                 0
                 (- start-year (ya-year (vector-ref yvs 0)))))
  (define i1 (if (not end-year)
                 yvl
                 (+ i0 1
                    (- end-year (ya-year (vector-ref yvs i0))))))
  (unless (and (>= i0 0)
               (< i0 i1)
               (<= i1 (vector-length yvs)))
    (error 'monthly-averages
           "insane indices ~A, ~A (data has ~A years from ~A)"
           i0 i1 yvl (ya-year (vector-ref yvs 0))))
  (unless (> average-over 0)
    (error 'monthly-averages "average over ~A is insane" average-over))
  (for/list ([start (in-range i0 i1 average-over)])
    (let ([end (min (+ start average-over) yvl)]
          [start-year (ya-year (vector-ref yvs start))])
      (mat start-year
           (+ start-year (- end start))
           (average-monthly-temperatures yvs start end)))))

(define month-lengths
  ;; No leap years!
  #(31 28 31 30 31 30 31 31 30 31 30 31))

(define month-starts
  (for/vector ([m (in-range 12)])
    (for/sum ([n (in-range m)])
      (vector-ref month-lengths n))))

(define (month-bounds month start-year end-year value)
  ;; The bounds for a given month in a range of years with a value
  (define start-day (vector-ref month-starts month))
  (define end-day (+ start-day (vector-ref month-lengths month)))
  ;; xmin xmax, ymin ymax zmin zmax
  (list (ivl start-day end-day)
        (ivl start-year end-year)
        (ivl 0 value)))

(define (monthly-averages->bounds averages)
  ;; compute the bounds for a list of monthly averages
  (for*/list ([avg (in-list averages)]
              [month (in-range 12)])
    (month-bounds month (mat-start-year avg) (mat-end-year avg)
                  (vector-ref (mat-averages avg) month))))

(define (plot-monthly-averages (f (data-file))
                               #:start-year (start-year #f)
                               #:end-year (end-year #f)
                               #:average-over (average-over 50)
                               #:to (to #f)
                               #:alpha (alpha 0.8)
                               #:t-min (t-min #f)
                               #:t-max (t-max #f))
  ;; Plot monthly averages
  (define averages (monthly-averages
                    (vectorify-years (tokenize-file f))
                    #:start-year start-year #:end-year end-year
                    #:average-over average-over))
  ((if to (curryr plot3d-file to) plot3d)
   (rectangles3d (monthly-averages->bounds averages)
                 #:alpha alpha)
   #:title (format "CE monthly average temperature ~A-~A, ~Ayr average"
             (mat-start-year (first averages))
             (- (mat-end-year (last averages)) 1)
             (- (mat-end-year (first averages))
                (mat-start-year (first averages))))
   #:x-label "day of year"
   #:y-label "year"
   #:z-label "temperature"
   #:z-min t-min
   #:z-max t-max))
