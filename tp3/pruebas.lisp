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

(print 
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
)