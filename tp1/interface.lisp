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


; ---- fin del mapa --- ;
(defun obtenerCalleValida ()
  (let
    (
      (i  (print (list 'indique 'una 'calle 'camelCase)))
      (intentoDeCalle (read))
      
    )
    (if (calle_valida intentoDeCalle) 
        ((lambda () (print (list intentoDeCalle 'es 'una 'calle 'valida)) intentoDeCalle))
        ((lambda () (print (list intentoDeCalle 'es 'invalida)) (obtenerCalleValida)))
      )
  )
)



(defun obtenerEsquina ()
  (let
    (
      (calle1 (obtenerCalleValida))
      (calle2 (obtenerCalleValida))
    )
    (if (esquina_valida (list calle1 calle2))
      ((lambda () (print (list 'la 'esquina calle1 calle2 'es 'valida )) (list calle1 calle2) ))
      ((lambda () (print (list 'la 'esquina calle1 calle2 'no 'existe )) (obtenerEsquina) ))
    )
    
  )
)
(defun interactuar () (let
  (
    (bienvenida (print (list 'bienvenido. 'indique 'esquina 'de 'partida)))
    (e1 (obtenerEsquina))
    (bienvenida (print (list 'indique 'esquina 'de 'finalizacion)))
    (e2 (obtenerEsquina))
  )
  ((lambda () 
    (print (list 'Voy 'a 'buscar 'el 'camino 'desde e1 'a e2))
    (print (list 'En 'mi 'grafo 'implica 'de (vertice_de_esquina e1) 'a (vertice_de_esquina e2) ))
    (print (ruta_para_mostrar (GPS (vertice_de_esquina e1) (vertice_de_esquina e2) grafo)))
  ))
  
))

(interactuar)
