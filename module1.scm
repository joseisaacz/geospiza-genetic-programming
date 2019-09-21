(define operadores '( + - * expt / log))

(define (randOperadores)
  (+ 1 (random 6)))

(define devuelvePos
  (lambda (L n)
    (cond ((null? L ) '())
          ((equal? 1 n) (car L))
          ((devuelvePos (cdr L) (- n 1) )))))

