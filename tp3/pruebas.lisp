(load 'interprete)
'(
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
      (x++)
    ))
  ))
)



(defun test (elemento resultado)
    (if (equal elemento resultado)
        (print "ok")
        (print (list "error:" elemento " es distinto de " resultado))
    )
)

(test (valor '(4 < 5) nil)  T)
(test (valor '(N < 5) '(N 4))  T)
(test (valor '(4 == 4) nil)  T)
(test (valor '(N == 4) '(N 4))  T)
(test (valor '(5 < 4) nil)  0)
(test (valor '(5 < N) '(N 4))  0)

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