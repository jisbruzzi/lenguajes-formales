(load 'mapa)
(load 'generalidades)
(load 'primitivas)
(load 'algoritmo)

(defun test (elemento resultado)
    (if (equal elemento resultado)
        (print "ok")
        (print (list "error:" elemento " es distinto de " resultado))
    )
)

(test (filtrar (lambda (x) (eq x 1) ) '(1 2 3 4 5 1 1 1))  '(1 1 1 1)  )

(test (vecinos 'a grafo) '(b f))
(test (vecinos 'b grafo) '(a c))
(test (vecinos 'i grafo) '(m j))

(test (posibles_vecinos grafo '(a b c)) '(f))
(test (posibles_vecinos grafo '(n d c b a)) '(j m))



(test 
  (expandir_trayectoria grafo '(d c b a))
  '((n d c b a) (e d c b a))
)
(test
  (expandir_trayectoria grafo '(n d c b a))
  '((J N D C B A) (M N D C B A))
)
(test
  (expandir_trayectoria grafo '(e d c b a))
  nil
)

(test
  (expandir_trayectorias grafo '((n d c b a) (e d c b a)) )
  '((J N D C B A) (M N D C B A))
)

(test
  (trayectoria_que_alcanza 'n '((n d c b a) (e d c b a)) )
  '(n d c b a)
)

(test
  (existe_trayectoria_que_alcanza 'n '((n d c b a) (e d c b a)) )
  t
)


(test (GPS 'a 'b grafo) '(b a))
(test (GPS 'a 'd grafo) '(d c b a))
(test (GPS 'a 'l grafo) '(L H G F A))
(test (GPS 'a 'k grafo) '(K J N D C B A))
