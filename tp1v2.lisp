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


;---------------- parte con primitivas usuales -------------------
(defun pertenece (e l)
  
)

;la trayectoria va a ser una lista de vértices (nombre "código")
;el primer elemento de ña trayectoria es el "fin", el último elemento de la trayectoria es el "inicio"
;un nodo va a ser un vértice en nomnbre "código"

'(defun vertices_iguales (v1 v2) 
  (and
    (eq (car v1) (car v2))
    (eq (cadr v1) (cadr v2))
  )
)

(defun vertices_iguales (v1 v2) 
  (eq v1 v2)
)


;implementación
(defun trayectoria_alcanza (trayectoria nodo)
  (vertices_iguales (car trayectoria) nodo)
)

;implementación
(defun construir_trayectoria_hacia_vertice (vertice trayectoria) 
  (cons vertice trayectoria)
)

;implementación
(defun vertices_de_trayectoria (trayectoria)
  treyectoria
)

;implementación
(defun vecinos (vertice grafo)
  (cadr
    (reduce
      (lambda (x y)
        (cond
          ((eq vertice (car x)) x)
          ((eq vertice (car y)) y)
          ( T nil )
        )
      )
      grafo
    )
  )
)

(assert (equal (vecinos 'a grafo) '(b f)))
(assert (equal (vecinos 'b grafo) '(a c)))
(assert (equal (vecinos 'i grafo) '(m j)))


; ----------------- parte abstracta, independiente de la implementación de los tipos de dato ------------
(defun posibles_vecinos (grafo trayectoria)
  (diferencia
    (vecinos (ultimo_vertice trayectoria) grafo)
    (vertices_de_trayectoria trayectoria)
  )
)

(print (posibles_vecinos grafo '(a b c)))

(defun expandir_trayectoria (inicio fin grafo trayectoria)
  (mapcar
    (lambda (v) (construir_trayectoria_hacia_vertice v trayectoria))
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