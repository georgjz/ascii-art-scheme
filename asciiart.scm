;;;;; A simple ASCII art program. Takes a single image file as input
(require pict 
         racket/draw)

;;; read the input image 
(define input-image (read-bitmap "input.jpg"))

;;; 1. get width and height 
(define width (send input-image get-width))
(define height (send input-image get-height))

;;; 2. get color data
(define pix (make-bytes (* width height 4)))

(send input-image get-argb-pixels 0       ; x
                                  0       ; y
                                  width   ; width of selection 
                                  height  ; height of selection
                                  pix)    ; byte data structure to save in

;;; 3. convert RGB data to single value
(define average 
  (lambda (ls)
    (cond 
      ((null? ls) '())
      (else (cons (/ (apply + (cdr (take ls 4))) 3)
                  (average (drop ls 4)))))))

(define average-values (average (bytes->list pix)))

;;; 4. convert brightness to ASCII symbols
(define asciis "`^,:;Il!i~+_-?][}{1)(|tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$")

(define ascii-list (string->list asciis))

(define ascii-length (length ascii-list))

(define value->ascii 
  (lambda (value)
    (list-ref ascii-list 
              (floor (* value (/ ascii-length 256))))))

(define ascii-matrix (map value->ascii average-values))

;;; 5. get ready for printing and print 
(define split 
  (lambda (ls n)
    (if (null? ls)
        '()
        (cons (take ls n)
              (split (drop ls n) n)))))

(define output (map list->string (split ascii-matrix width)))

(define output-file (open-output-file "output.txt"
                                      #:mode 'binary
                                      #:exists 'replace))

(map (lambda (x) (writeln x output-file))
     output)

(close-output-port output-file)