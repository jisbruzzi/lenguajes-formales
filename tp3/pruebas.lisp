(load 'interprete)

(defun test (elemento resultado)
  (if (equal elemento resultado)
    (print "ok")
    (print (list "error:" elemento " es distinto de " resultado))
  )
)

(defun alguno_igual (e L)
  (if (null L)
    nil
    (if (equal e (car L))
      T
      (alguno_igual e (cdr L))
    )
  )
)

(defun test_muchas_veces (gen_elemento resultados_posibles veces_restantes)
  (if (eq veces_restantes 0)
    T
    (if (alguno_igual (funcall gen_elemento) resultados_posibles)
      (test_muchas_veces 
        gen_elemento 
        resultados_posibles 
        (- veces_restantes 1)
      )
      nil
    )
  )
)
(defun test_no_determinista (gen_elemento resultados_posibles)
  (if (test_muchas_veces gen_elemento resultados_posibles 100)
    (print "ok")
    (print (list "error: un resultado es distinto de " resultados_posibles))
  )
)

(test (valor* '(1 - 2 * 2) nil nil)  -3)
(test (valor* '(4 < 5) nil nil)  T)
(test (valor* '(N < 5) '(N 4) nil)  T)
(test (valor* '(4 == 4) nil nil)  T)
(test (valor* '(N == 4) '(N 4) nil)  T)
(test (valor* '(5 < 4) nil nil)  0)
(test (valor* '(5 < N) '(N 4) nil)  0)
(test (valor* '(5 < N) nil '(N 4))  0)
(test (valor* '(x + n - 1 < 10) '(x 1) '(N 1))  T)



(test
  (run '(
    (int x)
    (int z A = 10)
    (main (
      (z = A + 1)
      (printf A)
      (scanf x)
      (if (a < X) (
        (z += A)
      ) else (
        (z = 1)
      ))
      (while (x < 10) (
        (printf x)
        (x ++)
      ))
    ))
  ) '(2))
  '(10 2 3 4 5 6 7 8 9)
)






(test (run 
  '((main (
    (if (1) (
      (printf "si")
    )else(
      (printf "no")
    ))
  )))
  nil
) '("si"))

(test (run 
  '((main (
    (if (1) (
      (printf "si")
    ))
  )))
  nil
) '("si"))

(test (run 
  '((main (
    (if (0) (
      (printf "si")
    ))
  )))
  nil
) '())

(test (run 
  '((main (
    (if (0) (
      (printf "si")
    )else(
      (printf "no")
    ))
  )))
  nil
) '("no"))


(test (run
  '( 
    (main (
      (if (5 < 0 )( 
        (printf "negativo" )
      )else(
        (printf "positivo" )
      ))
    ))
  )
  '(5)
) '("positivo"))

(test (run
  '( 
    (int n fact = 1)
    (main (
      (scanf n)
      (if (n < 0 )( 
        (printf "negativo" )
      )else(
        (printf "positivo" )
      ))
    ))
  )
  '(5)
) '("positivo"))

(test 
  (run
    '( 
      (int n fact = 1)
      (main (
        (scanf n)
        (if (n < 0 )( 
          (printf "no existe fact de nro negativo" )
        )else(
          (while (n > 1)( 
            (fact = fact * n)
            (n -- )
          ))
          (printf fact )
        ))
      ))
    )
    '(5)
  )
  '(120)
)

(test 
  (run
    '( 
      (int n fact = 1)
      (main (
        (printf q)
      ))
    )
    '(5)
  )
  '(excepcion ("No se conoce el valor de" Q))
)

(test (run '(
    (int x)
    (int z A = 10)
    (main (
      (z1 = A + 1)
      (printf A)
      (scanf xa)
      (if (a < X) (
        (z += A)
      ) else (
        (z = 1)
      ))
      (while (x < 10) (
        (printf x)
        (x ++)
      ))
    ))
  ) '(2))
  '(excepcion ("Una variable no se encuentra en el ambiente" z1))  
)

(test (run '(
    (int x)
    (int z A = 10)
    (main (
      (printf A)
      (scanf xa)
      (if (a < X) (
        (z += A)
      ) else (
        (z = 1)
      ))
      (while (x < 10) (
        (printf x)
        (x ++)
      ))
    ))
  ) '(2))
  '(excepcion ("Una variable no se encuentra en el ambiente" xa))  
)

(test (run '(
    (int x)
    (int z A = 10)
    (main (
      (z = A + 1)
      (printf A)
      (scanf x)
      (if (a < X) (
        (z += A)
      ) else (
        (z = 1)
      ))
      (while (x < 10) (
        (printf x)
        (x2 = x + 1)
      ))
    ))
  ) '(2))

  '(excepcion ("Una variable no se encuentra en el ambiente" x2))
)

(test (run '( (define n 1)
(int x)
(int z A = 10)
(main (
(z = A + n)
(printf A)
(scanf x)
(if (a < X) (
(z += A)
) else (
(z = 1)
))
(while (x + n - 1 < 10) (
(printf x)
(x = x + n)
))
))
) '(2))
'(10 2 3 4 5 6 7 8 9)
)

(test (run '( (define n 1)
(int x)
(int z A = 10)
(main (
(n = A + 1)
(printf A)
(scanf x)
(if (a < X) (
(z += A)
) else (
(z = 1)
))
(while (x < 10) (
(printf x)
(x = x + 1)
))
))
) '(2))
'(excepcion (N "corresponde a una constante, no se puede reasignar"))
)

(test (run '( (define n 1)
(int x)
(int z A = 10)
(main (
(z = A + 1)
(printf A)
(scanf n)
(if (a < X) (
(z += A)
) else (
(z = 1)
))
(while (x < 10) (
(printf x)
(x = x + 1)
))
))
) '(2))
'(excepcion (N "corresponde a una constante, no se puede reasignar"))
)

(test (run '( (define n 1)
(int x)
(int z A = 10)
(main (
(z = A + n)
(printf A)
(scanf x)
(if (a < X) (
(z += A)
) else (
(z = 1)
))
(while (x + n - 1 < 10) (
(printf n)
(x = x + n)
))
))
) '(2))
'(10 1 1 1 1 1 1 1 1)
)

(test (run '( (define n 1)
(int x)
(int z A = 10)
(main (
  (z = A + n)
  (printf A)
  (scanf x)
  (if (a < X) (
    (z += A)
  ) else (
    (z = 1)
  ))
  (while (x + n - 1 < 10) (
    (printf n)
    (x = x + n)
  ))
))
) '(2))
'(10 1 1 1 1 1 1 1 1)
)


(test_no_determinista 
  (lambda () 
    (run '(
    (main (
      (if ( (1 < 2) (1 < 2) (5 < 2) ) 
      ( ((printf 2)) ((printf 1)) ((printf 3)) )
      )
    ))
    ) '(2))
  )
  '( (1) (2))
)


(test_no_determinista 
  (lambda () 
    (run '(
    (int x = 0)
    (int y = 0)
    (main (
      (while ((x < 2) (y < 2) ) (
        ((printf x)(x = x + 1))
        ((printf y)(y = y + 1))
      ))
    ))
    ) '())
  )
  '( (0 1 0 1) (0 0 1 1))
)


(test (run '(
(int x)
(int z = 10)
(main (
  (z = (z / 2) + 1)
  (printf z)
))
) '(2))
'(6)
)

(test (run '(
(int x)
(int z = 10)
(main (
  (z = (x = z / 2) + 1)
  (printf z)
  (printf x)
))
) '(2))
'(6 5)
)
