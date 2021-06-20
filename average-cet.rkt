#lang racket

;;; File from http://hadobs.metoffice.com/hadcet/cetml1659on.dat
;;; via https://www.trevorharley.com/weather.html
;;; (See Makefile which will fetch it)
;;;

(require racket/string
         plot)

(define-syntax-rule (with-open-input-file (in file) form ...)
  (call-with-input-file file
    (λ (in) form ...)))

(define data-file (make-parameter "cetml1659on.dat"))

(define (tokenize-file (f (data-file)))
  (unless (or (file-exists? f)
              (system* (find-executable-path "make") f #:set-pwd? #t))
    (error 'tokenize-file "no file ~A and could not make it" f))
  (define this-year (pregexp (format "^[[:space:]]*~A"
                               (date-year (seconds->date (current-seconds))))))
  (define good-line
    ;; a good line starts with a date and then has 13 floats
    #px"^[[:space:]]*[[:digit:]]{4}\
([[:space:]]+[-+]?[[:digit:]]+\\.[[:digit:]]+){13}")
  (with-open-input-file (in f)
    (for/list ([l (in-lines in)]
               #:when (and (regexp-match?
                            good-line
                            l)
                           (not (regexp-match this-year l))))
      (for/list ([e (in-list (string-split l))])
        (string->number e)))))

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
       (error 'tf-year-sequence-continuous? "what?")])))

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

(struct ya (year average months))

(define (vectorify-years years #:since (since #f))
  ;; Turn a list of years into a vector of ya objects
  (for/vector ([yl (in-list years)]
               #:when (or (not since) (>= (first yl) since)))
    (match-let ([(list year january february march april may june
                       july august september
                       october november december average) yl])
      (ya year average
          (vector january february march april may june
                  july august september
                  october november december)))))

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

(define (plot-decadal-averages (f (data-file))
                               #:since (since #f)
                               #:decade (decade 10)
                               #:to (to #f))
  ;; Plot decadal averages where a 'decade' can be any length of time
  (define yvs (vectorify-years (tokenize-file f)
                               #:since (if since (- since (- decade 1)) #f)))
  (define start (ya-year (vector-ref yvs (- decade 1))))
  (unless (or (not since)
            (= start since))
    (error 'plot-decadal-averages "~A is too early: start=~A" since start))
  ((if to (curryr plot-file to) plot)
   (lines (for/vector ([a (in-vector (average-vector yvs decade
                                                      ya-average))]
                        [y (in-naturals start)])
             (list y a)))
   #:title (format "CE decadal averages from ~A (decade=~A)" start decade)
   #:x-label "year"
   #:y-label (format "average temperature, last ~A years" decade)))

(define (decaying-average-vector v d (vf identity))
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
  (for/vector ([s (in-stream (da-stream 0 (vf (vector-ref v 0))))])
    s))

(define (plot-decaying-averages (f (data-file))
                                #:decay (decay 10)
                                #:since (since #f)
                                #:decade (decade 10)
                                #:to (to #f))
  ;; Plot decaying averages where the decay is how long to remember
  (define yvs (vectorify-years (tokenize-file f) #:since since))
  (define start (or since (ya-year (vector-ref yvs 0))))
  ((if to (curryr plot-file to) plot)
   (lines (for/vector ([a (in-vector (decaying-average-vector yvs decay
                                                              ya-average))]
                       [y (in-naturals start)])
            (list y a)))
   #:title (format "CE decaying averages from ~A (decay=~A)" since decay)
   #:x-label "year"
   #:y-label (format "average temperature, decayed over ~A years" decade)))