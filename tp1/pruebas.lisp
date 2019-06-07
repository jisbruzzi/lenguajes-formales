(load 'algoritmo)


(setq grafo '(
(a(b f)) (b(a c )) (c(b d )) (d(c n e)) (e(d)) (f(g ))
(g(h)) (h(i l)) (i(m j)) (j( k)) (k(o))
(l (b f)) (m (l c)) (n ( j m)) (o(e n))
) )

(setq diccionario '(
(a (PaseoColon Independencia))
(b (PaseoColon Chile))
(c (PaseoColon Mexico ))
(d (PaseoColon Venezuela))
(e (PaseoColon Belgrano))
(f (Independencia Balcarce))
(g (Independencia Defensa))
(h (Defensa Chile))
(i (Defensa Mexico))
(j (Defensa Venezuela))
(k (Defensa Belgrano ))
(l (Balcarce Chile ))
(m (Balcarce Mexico))
(n (Balcarce Venezuela))
(o (Balcarce Belgrano))
) )

; --- pruebas ---- ;

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


(test (GPS 'a 'b grafo) '((b a)))
(test (GPS 'a 'd grafo) '((d c b a)))
(test (GPS 'a 'l grafo) '((L H G F A)))
(test (GPS 'a 'k grafo) '((K J N D C B A) (K J I H G F A)) )

(test 
  (ruta_para_mostrar '(K J N D C B A))
  '((RECORRER 3 CUADRAS POR
   PASEOCOLON Y DOBLAR EN
   VENEZUELA)
  (RECORRER 2 CUADRAS POR
   VENEZUELA Y DOBLAR EN
   DEFENSA)
  (RECORRER 1 CUADRAS POR
   DEFENSA HASTA LLEGAR A
   DESTINO))
)


; -------- PRUEBA GENERAL/DE INTEGRACIÓN ---- ;
(test 
  (GPS_INTERFAZ_ENUNCIADO '(PaseoColon Independencia) '(Defensa Belgrano) grafo)
  '(
    (
      (RECORRER 3 CUADRAS POR PASEOCOLON Y DOBLAR EN VENEZUELA)
      (RECORRER 2 CUADRAS POR VENEZUELA Y DOBLAR EN DEFENSA)
      (RECORRER 1 CUADRAS POR DEFENSA HASTA LLEGAR A DESTINO)
    )
    (
      (RECORRER 2 CUADRAS POR INDEPENDENCIA Y DOBLAR EN DEFENSA)
      (RECORRER 4 CUADRAS POR DEFENSA HASTA LLEGAR A DESTINO)
    )
  )
)