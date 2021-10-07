#lang racket/base
(require data/bit-vector
         rackunit
         racket/class)

(define prime-sieve (class
                      object%
                      (init-field limit)
                      (super-new)
                      ;; only need 1/2 b/c evens are not primes (except 2), pretend vector is just odd #s
                      ;; bit-vectors are zero indexed!! must substract one when indexing!
                      (define rawbits (make-bit-vector (floor (/ (+ 1 limit) 2)) #t)) ; vector of #t and #f vals

                      (field [sieve-size limit])

                      (define prime-counts '#hash((10 . 4)
                                                  (100 . 25)
                                                  (1000 . 168)
                                                  (10000 . 1229)
                                                  (100000 . 9592)
                                                  (1000000 . 78498)
                                                  (10000000 . 664579)
                                                  (100000000 . 5761455)))

                      (define (validate-results)
                        ;; checks to see if sieve size is in the prime-counts hash
                        ;; if not return false. else return true
                        (if (hash-has-key? prime-counts sieve-size)
                            (= (hash-ref prime-counts sieve-size) (count-primes))
                            #f))

                      (define (get-bit index)
                        ;; gets bit (#t or #f) in bit-vector, but skips all even indexes
                        ;; we divide index by 2 b/c we still loop from start to end of initial sieve size
                        (if (= (modulo index 2) 0) #f (bit-vector-ref rawbits (round (/ (sub1 index) 2)))))

                      (define (clear-bit index)
                        ;; sets a #t value in vector to #f (not prime)
                        (if (= (modulo index 2) 0)
                            #f
                            (bit-vector-set! rawbits (floor (/ (sub1 index) 2)) #f)))

                      (define/public (run-sieve)
                        (define factor 3)
                        ;; iterate to sqrt of sieve-size b/c half values already thrown out (even)
                        (define q (sqrt sieve-size))
                        (let loop ()
                          (for ([num (in-range factor sieve-size)]
                                #:break (if (eq? (get-bit num) #t) (set! factor num) #f))
                            void)

                          (for ([num (in-range (* factor 3) sieve-size (* factor 2))])
                            (clear-bit num))

                          (set! factor (+ factor 2))

                          (unless (> factor q)
                            (loop))))

                      (define (count-primes)
                        ;; return # of bits set. counts all #t bits
                        (bit-vector-popcount rawbits))

                      (define/public (print-results show-results duration passes)
                        ;; (println  (format "Raw Bits ~a" (bit-vector->list rawbits)))
                        ;; (bit-vector-set! rawbits (- (floor (/ 10 2)) 1) #f)
                        ;; (bit-vector-ref rawbits 4)
                        ;; (println  (format "Raw Bits ~a" (bit-vector->list rawbits)))
                        (if (eq? show-results #t) (display "2, ") #f)
                        (define count 1)
                        (for ([num (in-range 3 sieve-size)])
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

(define time-start (current-inexact-milliseconds))
(define passes 0)

(let ([start-time (current-inexact-milliseconds)]
      [five-seconds 5]
      [max 10000])
  (let loop ([passes 1])
    (define sieve (new prime-sieve [limit max]))
    (send sieve run-sieve)
    ;; (println (current-inexact-milliseconds) )
    ;; (println start-time )
    ;; (println (- (current-inexact-milliseconds) start-time))
    (if (>= (- (current-inexact-milliseconds) start-time) 5)
        (send sieve print-results #f (- (current-inexact-milliseconds) start-time) passes)
        (loop (+ passes 1)))))
