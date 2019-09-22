(define operadores '( + - * expt / log))

;;Dominio: un numero n
;;Codominio: devuelve un numero random entre 1 y n
(define randOperadores
  (lambda (n)
  (+ 1 (random n))))

;;Dominio: Listas de cualquier tamano y un numero N natural mayor que 0
;;Codominio: Elemento de la Posicion N 
(define devuelvePos
  (lambda (L n)
    (cond ((null? L ) '())
          ((equal? 1 n) (car L))
          ((devuelvePos (cdr L) (- n 1) )))))

;;Dominio: arboles binarios de expresion y numeros x y
;;Codominio: resultado de la expresion evaluada con x y

(define evalua
  (lambda (L a b)
    (eval (list (cons 'lambda (cons '(x y) (list L))) a b))))

;; Dominio: 
;; Codominio: x y o un numero natural entre 0 y 1000000
(define x-y-#
  (lambda ()
    (cond ((= (random 3) 0) '(x))
          ((= (random 3) 1) '(y) )
          ((random 1000000)))))

