(load 'evaluar)
(defun test (elemento resultado)
    (if (equal elemento resultado)
        (print "ok")
        (print (list "error:" elemento " es distinto de " resultado))
    )
)
; con numeros
(test (evaluar '2 nil) 2)
; con valores booleanos true false
(test (evaluar nil nil) nil)
(test (evaluar 't nil) t)
;asociaciones en el ambiente
(test (evaluar 'A '(A 2) ) 2)
(test (evaluar 'B '(A 2 B 10)) 10)
;la función quote
(test (evaluar '(quote A) nil) 'A)
(test (evaluar '(quote 1) nil) 1)
(test (evaluar '(quote (car a)) nil ) '(car a))
(test (evaluar '(quote ((2 3) (4 5))) nil) '((2 3) (4 5)))
;funciones booleanas and y or
(test (evaluar '(and (or t nil) t) nil ) t)
(test (evaluar '(and (or t nil) (or nil nil)) nil) 'nil  )
(test (evaluar '(or (or t nil) (or nil nil )) nil) 't  )
;función if
(test (evaluar '(if t 5 9) nil) 5)
(test (evaluar '(if nil 5 9) nil) 9)
;Función car + ambiente
(test (evaluar '(car (list a 2 3)) '(a 100) ) '100  )
;Función cdr + ambiente
(test (evaluar '(cdr (list a b c)) '(a 100 b 99 c 98) ) '(99 98)  )
;Funciones anónimas lambda
(test (evaluar '((lambda (x) (* x 2)) 2) nil ) '4  )
(test (evaluar '((lambda (x y) (+ (* x 2) y)) 2 4) nil) '8  )
(test (evaluar '(lambda (x) (* x 2)) nil) '(lambda (x) (* x 2))  )
(test (evaluar '(mapcar (lambda (x) (cons x (cdr '(3 4 5)))) '(1 2 3)) nil) '((1 4 5) (2 4 5)(3 4 5))  )
;Forma funcional mapcar
(test (evaluar '(mapcar 'numberp (quote (4))) nil) '(t)  )
(test (evaluar '(mapcar 'numberp (quote (4 5 6 nil))) nil) '(t t t nil)  )
(test (evaluar '(mapcar 'car (quote ( (2 3) (4 5 ))) ) nil) '(2 4)  )
;Funciones definidas en el ambiente
(test (evaluar '(doble 5) '(doble 
  (lambda(n)
    (* n 2 )
  ) 
) ) 10  )
;Funcion recursiva definida en el ambiente
(test (evaluar '(fact 5) '(fact 
  (lambda(n)
    (if (eq n 0) 
      1 
      (* n (fact (- n 1)))
    )
  ) 
) ) 120  )
(test 
  (evaluar 
    '(mapcar 
      'fact 
      (quote ( 2 3 4 5 ) )
    ) 
    '(fact 
      (lambda (n) 
        (if
          (eq n 0) 
          1 
          (* n (fact (- n 1)))))
    ) 
  )
  '(2 6 24 120)  
)

(test
  (evaluar
    '(mapcar
      (lambda (x y) (+ x y))
      '(1 2 3)
      '(4 5 6)
    )
    nil
  )
  '(5 7 9)
)

; --------- otra prueba de mapcar n-ádico --------- ;

(test (evaluar '(mapcar 'union '((a v e)(m a s)) '((s e a)(m e n o s)))
  '(union (lambda(x y)(if (null x) y 
                        (if (pertenece (car x)y) (union (cdr x)y)
                             (cons (car x)(union (cdr x)y))
                        )
                      )
           )
    pertenece (lambda (a li)(if (null li)nil
                                 (if (eq a (car li)) t
                                 (pertenece a (cdr li))
                                 )
                            )
               )
    )
   )
'((v s e a) (a m e n o s))
)