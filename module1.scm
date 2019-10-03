
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
(define xynum
  (lambda ()
    (cond ((= (random 2) 0) 'x)
          ((= (random 2) 1)  'y)
          (else (random 100)))))

; Dominio: vacio
; Codominio: booleano de si el ramdon es menor que 50
(define es-hoja?
  (lambda ()
    (< (random 100) 50 )))


;dominio: vacio
;codominio: de forma aleatorio se forma un arbol de expresion 
(define arbolaleatorio
  (lambda()
    (cond ((es-hoja?) (xynum))
          ((list(random-op)
                (arbolaleatorio)
                (arbolaleatorio))))))


;Dominio: recibe un numero entero mayor que cero
;Codominio: lista con poblacion inicial (arboles de expresion)
(define poblacion
  (lambda (n)
    (cond ((= n 1) (list(arbolaleatorio)))
          ((append (list(arbolaleatorio)) (poblacion (- n 1)))))))

(define inicial
  (lambda (n)
    (cond ((zero? n) '())
          (else
          (cons(arbolaleatorio) (poblacion (- n 1)))))))


;;(let ((p (open-input-file "f1.txt")))
;;  (let f ((x (read p)))
;;    (if (eof-object? x)
;;        (begin
;;          (close-input-port p)
;;          '())
;;        (cons x (f (read p))))))



;;Dominio: archivo con los puntos  x 
;;Codominio: lista con los puntos x del archivo
(define f1-x
 (call-with-input-file "f1_x.txt"
  (lambda (p)
    (let f ((x (read p)))
      (if (eof-object? x )
          '()
          (cons x (f (read p))))))))

;;Dominio: archivo con los puntos  y 
;;Codominio: lista con los puntos y del archivo
(define f1-y
 (call-with-input-file "f1_y.txt"
  (lambda (p)
    (let f ((x (read p)))
      (if (eof-object? x )
          '()
          (cons x (f (read p))))))))

;;Dominio: archivo con los puntos  z 
;;Codominio: lista con los puntos z del archivo
(define f1-z
 (call-with-input-file "f1_z.txt"
  (lambda (p)
    (let f ((x (read p)))
      (if (eof-object? x )
          '()
          (cons x (f (read p))))))))


;Dominio: 2 arboles de expresion
;Codominio: el cruce de los 2 arboles
(define cruce
  (lambda (arbol1 arbol2)
    (cond ((null? arbol1) arbol2)
          ((null? arbol2) arbol2)
          ((mutacion?) (list (car arbol1)
                             (arbolaleatorio)
                             (caddr arbol2)))
          (else (list (car arbol1)
                      (cadr arbol1)
                      (caddr arbol2))))))

;Dominio: vacio
;Codominio: posibilidad de que haya mutacion menor que 5%
(define mutacion?
  (lambda ()
    (< (random 100) 5)))


(define eval-poblacion
  (lambda (poblacion x y)
    (map (lambda (n) (evalua n x y)) poblacion)))

(define dif-puntos
  (lambda (L z)
    (map (lambda (zn) (abs (- z zn))) L)))

(define f
  (lambda (Lx Ly Lz poblacion)
    (map (lambda (x y z) (dif-puntos(eval-poblacion poblacion x y) z)) Lx Ly Lz)))

(define f2
  (lambda (Lx Ly Lz poblacion)
    (map (lambda (n) (apply + n)) (f Lx Ly Lz poblacion))))
    