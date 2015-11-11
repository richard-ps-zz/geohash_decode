#lang racket 

(require compatibility/mlist)
(require scheme/mpair)
(require racket/flonum)
(require racket/local)

(define m (hash #\0 "00000" #\1 "00001" #\2 "00010" #\3 "00011"
                #\4 "00100" #\5 "00101" #\6 "00110" #\7 "00111"
                #\8 "01000" #\9 "01001" #\b "01010" #\c "01011"
                #\d "01100" #\e "01101" #\f "01110" #\g "01111"
                #\h "10000" #\j "10001" #\k "10010" #\m "10011"
                #\n "10100" #\p "10101" #\q "10110" #\r "10111"
                #\s "11000" #\t "11001" #\u "11010" #\v "11011"
                #\w "11100" #\x "11101" #\y "11110" #\z "11111"))


; Convert a string representing a base32 number to base2
(define (base32->base2 n)
  (define base2 "")
  (for ([id n])
   (set! base2 (string-append base2 (hash-ref m id))))
   base2)

; Return two lists, the first with elements in even positions
; and the second with elements in odd positions.
(define (split ls)
  (if (or (null? ls) (null? (cdr ls)))
      (list ls '())
      (let ((next (split (cddr ls))))
        (list (cons (car ls) (car next))
              (cons (cadr ls) (cadr next))))))

(define (evenPositions ls)
 (car (split (string->list ls))))

(define (oddPositions ls)
 (cadr (split (string->list ls))))

(define (decodeGeohash geohash)
  (define lat (mlist '()))
  (define lng (mlist '()))
  (define binaryGeohash (base32->base2 geohash))
  (local [(define min -90.0)
            (define mid 0.0)
            (define max 90.0)]
    (for ([i (oddPositions binaryGeohash)])
      
      (when (= (char->integer #\0) (char->integer i))
        (set! max mid)
        (set! mid (fl/ (+ min mid) 2.0)))
      (unless (= (char->integer #\0) (char->integer i))
        (set! min mid)
        (set! mid (fl/ (+ max mid) 2.0)))
      ;(println min)
      ;(println mid)
      ;(println max)
      )
    (set-mcar! lat min)
    (set-mcdr! lat max))

  (local [(define min -180.0)
          (define mid 0.0)
          (define max 180.0)]
    (for ([i (evenPositions binaryGeohash)])
      
      (when (= (char->integer #\0) (char->integer i))
        (set! max mid)
        (set! mid (fl/ (+ min mid) 2.0)))
      (unless (= (char->integer #\0) (char->integer i))
        (set! min mid)
        (set! mid (fl/ (+ max mid) 2.0)))
     ; (println min)
     ; (println mid)
     ; (println max)
      )
    (set-mcar! lng min)
    (set-mcdr! lng max))
  (cons lat lng))