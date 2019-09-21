(define operadores '( + - * expt / log))

(define randOperadores
  (lambda (n)
  (+ 1 (random 6))))

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