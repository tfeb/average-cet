#lang racket

;;;; An attempt at growing season lengths
;;; CET daily data from https://www.metoffice.gov.uk/hadobs/hadcet/cetdl1772on.dat
;;; via https://www.metoffice.gov.uk/hadobs/hadcet/data/download.html
;;;
;;; start of growing season is 5 days with temperature above 5, end
;;; is 5 days with temperature below 5, starting from June
;;;

(require racket/string
         plot
         "util.rkt")

(module+ test
  (require rackunit))

(define data-file (make-parameter "cetdl1772on.dat"))

(define missing-data -999)

(define temperature-scale 10)

;;; Lines in the file are of the form
;;; year day-of-month entry-for-jan ... entry-for-december
;;;

(define (tokenize-file (f (data-file)))
  (unless (or (file-exists? f)
              (system* (find-executable-path "make") f #:set-pwd? #t))
    (error 'tokenize-file "no file ~A and could not make it" f))
  (define good-line
    ;; a good line is a year, a day of the month and then 12 integers
    #px"^[[:space:]]*[[:digit:]]{4}\
([[:space:]]+[-+]?[[:digit:]]+){13}")
  (with-open-input-file (in f)
    (for/list ([l (in-lines in)]
               #:when (regexp-match?
                       good-line
                       l))
      (for/list ([e (in-list (string-split l))])
        (string->number e)))))

(define (maybe-beyond-month-end? day month)
  (case month
    ((2) (> day 28))
    (else (> day 30))))

(define (prune-and-tag-lines lines #:report-missing (report-missing #f))
  ;; return a (backwards) list of tagged entries which are (y m d t)
  (for/fold ([tagged-lines '()])
            ([line lines])
    (match-let ([(list* year day entries) line])
      (for/fold ([tagged-entries tagged-lines])
                ([month (in-naturals 1)]
                 [entry (in-list entries)])
        (cond
          [(and (= entry missing-data)
                (maybe-beyond-month-end? day month))
           ;; if the entry is missing data and plausibly beyond the end of
           ;; the month just ignore it
           tagged-entries]
          [(= entry missing-data)
           ;; some other missing data
           (when report-missing
             (printf "missing data ~A-~A-~A~%" year month day))
           (cons (list year month day #f) tagged-entries)]
          [else (cons (list year month day entry) tagged-entries)])))))

(define (sort-tagged-entries entries)
  (sort entries <
        #:key (λ (entry)
                (match-let ([(list y m d _) entry])
                  ;; if every month has 31 days this counts days
                  (+ (* y 372) (* m 31) d)))
        #:cache-keys? #t))


(define (prune-deaders entries)
  ;; entry is (day month day-in-month temp)
  (if (fourth (first entries))
      entries
      (prune-deaders (rest entries))))

(define (yearify entries)
  ;; Turn a flat list of (year month day temp) into a list of entries
  ;; of the form (year day ...) where each day is (day-number month day temp),
  ;; and day-number is the day in the year.
  (for/fold ([years '()]
             [entries-in-current-year '()]
             [current-year (first (first entries))]
             [current-year-day 1]
             #:result (reverse (if (null? entries-in-current-year)
                                   years
                                   (cons
                                    (cons current-year
                                          (list->vector
                                           (reverse (prune-deaders
                                                     entries-in-current-year))))
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
                               (reverse (prune-deaders
                                         entries-in-current-year))))
                        years)
                  (list (list 1 month day temperature))
                  year
                  2)))))

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

(define (growing-seasons yeared
                         #:start-temp (start-temp 5)
                         #:n-start (n-start 5)
                         #:end-temp (end-temp 5)
                         #:n-end (n-end 5)
                         #:open-end (open-end #t))
  (define scaled-day-temp fourth)
  (for/list ([year yeared])
    (match-let ([(cons yyyy entries) year])
      (let* ([l (vector-length entries)]
             [l/2 (round (/ l 2))]
             [start (find-first-n-matching
                     (λ (day)
                       (>= (scaled-day-temp day)
                           (* start-temp temperature-scale)))
                     n-start entries 0 l)]
             [end (and start
                       (find-first-n-matching
                        (λ (day)
                          (< (scaled-day-temp day)
                             (* end-temp temperature-scale)))
                        n-end entries (max start l/2) l))])
        (if (>= l 365)
            ;; reject short years (this year)
            (list yyyy start (or end l))
            (list yyyy #f #f))))))


(plot-font-family 'modern)

(define (growing-season-lengths file #:since (since #f))
  (for/list ([gs (in-list (--> file
                               tokenize-file
                               prune-and-tag-lines
                               sort-tagged-entries
                               yearify
                               growing-seasons))]
             #:when (and (second gs)
                         (or (not since)
                             (>= (first gs) since))))
    (match-let ([(list yyyy start end) gs])
      (list yyyy (- end start)))))

(define (plot-growing-seasons (f (data-file)) #:since (since #f)
                              #:to (to #f))
  ((if to (curryr plot-file to) plot)
   (lines (growing-season-lengths f #:since since))
   #:title "CET growing season lengths"
   #:x-label "year"
   #:y-label "length in days"))
