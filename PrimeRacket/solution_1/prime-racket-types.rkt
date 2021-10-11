#lang typed/racket/base
(require 
         typed/rackunit
         typed/racket/class)

(define prime-sieve% (class
                       object%
                       (init-field limit : Positive-Integer)
                       (super-new)
                       ;; only need 1/2 b/c evens are not primes (except 2), pretend vector is just odd #s
                       ;; bit-vectors are zero indexed!! must substract one when indexing!
                       (define rawbits : (Vectorof Positive-Integer) (vector (floor (/ (+ 1 limit) 2)) #t)) ; vector of #t and #f vals

                       (field [sieve-size : Positive-Integer limit])

                       (define prime-counts : (HashTable Positive-Integer Positive-Integer) '#hash((10 . 4)
                                                                                                   (100 . 25)
                                                                                                   (1000 . 168)
                                                                                                   (10000 . 1229)
                                                                                                   (100000 . 9592)
                                                                                                   (1000000 . 78498)
                                                                                                   (10000000 . 664579)
                                                                                                   (100000000 . 5761455)))
                       (: validate-results (-> Boolean))
                       (define (validate-results)
                         ;; checks to see if sieve size is in the prime-counts hash
                         ;; if not return false. else return true
                         (if (hash-has-key? prime-counts sieve-size)
                             (= (hash-ref prime-counts sieve-size) (count-primes))
                             #f))

                       (: get-bit (-> Positive-Integer Boolean))
                       (define (get-bit index)
                         ;; gets bit (#t or #f) in bit-vector, but skips all even indexes
                         ;; we divide index by 2 b/c we still loop from start to end of initial sieve size
                         (if (= (modulo index 2) 0) #f (vector-ref rawbits (round (/ (sub1 index) 2)))))

                       (: clear-bit (-> Positive-Integer Boolean))
                       (define (clear-bit index)
                         ;; sets a #t value in vector to #f (not prime)
                         (if (= (modulo index 2) 0)
                             #f
                             (vector-set! rawbits (floor (/ (sub1 index) 2)) #f)))

                       (: run-sieve (-> Void))
                       (define/public (run-sieve)
                         (define factor 3)
                         ;; iterate to sqrt of sieve-size b/c half values already thrown out (even)
                         (define q (sqrt sieve-size))

                         ;; this is just a test of a valid for loop not working in my class body
                         (for ([num : Integer (in-range 1 10)])
                           (println num))
                         
                         (let loop ()
                           (for ([num : Integer (in-range [factor : Integer] [sieve-size : Integer])]
                                 #:break (if (eq? (get-bit num) #t) (set! factor num) #f))
                             void)

                           (for ([num : Integer (in-range (* factor 3) sieve-size (* factor 2))])
                             (clear-bit num))

                           (set! factor (+ factor 2))

                           (unless (> factor q)
                             (loop)))

                         )

                       (: count-primes (-> Positive-Integer))
                       (define (count-primes)
                         ;; return # of bits set. counts all #t bits
                         (vector-length (vector-filter-not false? vec)))

                       (: print-results (-> Boolean Positive-Float Positive-Integer))
                       (define/public (print-results show-results duration passes)
                         (if (eq? show-results #t) (display "2, ") #f)
                         (define count 1)
                         (for ([num : Positive-Integer (in-range 3 sieve-size)])
                           (cond
                             [(and (eq? (get-bit num) #t) (eq? show-results #t)) (begin
                                                                                   ;; use display to keep "" from printing
                                                                                   (display (format "~a, " num))
                                                                                   (set! count (+ count 1)))]
                            [(eq? (get-bit num) #t) (set! count (+ count 1))]))
                        ;; check-equal only complains when there is an error
                        (check-equal? count (count-primes))
                        (displayln "")

                        (println (format "passes: ~a, Time: ~a, Avg: ~a, Limit: ~a, Count: ~a, Valid: ~a"
                                         passes duration (/ duration passes) sieve-size count (validate-results)))

                        (displayln "")
                        (println (format "diego-e-crespo;~a;~a;l;algorithm=base,faithful=yes" passes duration))
                        )))

(let ([start-time (current-inexact-milliseconds)]
      [five-seconds 5]
      [max 100])
  (let loop ([passes 1])
    (define sieve (new prime-sieve% [limit max]))
    (send sieve run-sieve)
    (if (>= (- (/ (current-inexact-milliseconds) 1000) (/ start-time 1000)) 5)
        (send sieve print-results #f (- (/ (current-inexact-milliseconds) 1000) (/ start-time 1000)) passes)
        (loop (+ passes 1)))))
