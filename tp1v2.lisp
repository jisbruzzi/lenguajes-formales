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

(defun trayectoria_alcanza (trayectoria nodo)
  nil
  ;implementación
)

(defun trayectoria_con_vertice (vertice trayectoria) 
  nil
  ;implementación
)

(defun vertices_de_trayectoria (trayectoria)
  nil
  ;implementacion
)

(defun vecinos (vertice grafo)
  nil
  ;implementacion
)

(defun posibles_vecinos (grafo trayectoria)
  (diferencia
    (vecinos (ultimo_vertice trayectoria) grafo)
    (vertices_de_trayectoria trayectoria)
  )
)

(defun expandir_trayectoria (inicio fin grafo trayectoria)
  (mapcar
    (lambda (v) (trayectoria_con_vertice v trayectoria))
    (posibles_vecinos grafo trayectoria)
  )
)

(defun expandir_trayectorias (inicio fin grafo trayectorias)
  (apply 'append (mapcar (lambda (t) (expandir_trayectoria grafo t)) trayectorias))
)

(defun trayectoria_que_alcanza (nodo trayectorias)
  (car 
    (filtrar 
      (lambda (t) (trayectoria_alcanza t nodo ) )
      trayectorias
    )
  )
)

(defun existe_trayectoria_que_alcanza (nodo trayectorias)
  (not (null 
    (trayectoria_que_alcanza nodo trayectorias)
  ))
)


(defun GPS (inicio fin grafo &optional (trayectorias (list (list i))))
  (cond 
    ( (null trayectorias) nil )
    ( (existe_trayectoria_que_alcanza fin trayectorias) (trayectoria_que_alcanza fin trayectorias) )
    ( T (GPS inicio fin grafo (expandir_trayectorias inicio fin grafo trayectorias)))
  )
)