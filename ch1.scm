(define (square n)
  (* n n))
(define (sum-of-squares a b)
  (+ (square a) (square b)))

;;EX 1.2
(define (sum-three-largest-squares a b c)
  (cond ((and (<= a b) (<= a c)) (sum-of-squares b c))
        ((and (<= b a) (<= b c)) (sum-of-squares a c))
        ((and (<= c a) (<= c b)) (sum-of-squares a b))))

;;Newton's Square Root method
(define (average a b)
  (/ (+ a b) 2))
(define (close-enough-square guess orig)
  (< (abs  (- orig (square guess))) .001))
(define (newton-sqrt guess orig)
  (if (close-enough-square guess orig) guess
      (newton-sqrt (average (/ orig guess) guess) orig)))

(newton-sqrt 3 9)
(newton-sqrt 1 2)
(newton-sqrt 12 155)