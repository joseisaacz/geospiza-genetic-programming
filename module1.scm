
;;Dominio: un numero n
;;Codominio: devuelve un numero random entre 1 y n
(define random-op
  (lambda ()
  (list-ref '(+ - / * expt log) (random 6))))


;;Dominio: arboles binarios de expresion y numeros a b
;;Codominio: expresion evaluada con a b
(define evalua
  (lambda (L a b)
    (eval (list (cons 'lambda (cons '(x y) (list L))) a b))))

;; Dominio: vacio
;; Codominio: x y o un numero natural entre 0 y 1000000
(define x-y-num
  (lambda ()
    (cond ((= (random 2) 0) 'x)
          ((= (random 2) 1)  'y)
          (else (random 10000)))))

; Dominio: vacio
; Codominio: booleano de si el ramdon es menor que 50
(define es-hoja?
  (lambda ()
    (< (random 100) 50 )))


;dominio: vacio
;codominio: de forma aleatorio se forma un arbol de expresion 
(define arbolaleatorio
  (lambda()
    (cond ((es-hoja?) (x-y-num))
          ((list(random-op)
                (arbolaleatorio)
                (arbolaleatorio))))))


;Dominio: recibe un numero entero mayor que cero
;Codominio: lista con poblacion inicial (arboles de expresion)
(define poblacion
  (lambda (n)
    (cond ((= n 1) (list(arbolaleatorio)))
          ((append (list(arbolaleatorio)) (poblacion (- n 1)))))))

;(define inicial
 ; (lambda (n)
  ;  (cond ((zero? n) '())
   ;       (else
    ;       (cons(list(arbolaleatorio)) (poblacion (- n 1)))))))

