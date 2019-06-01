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

(defun filtrar (f L)
  (reduce
    (lambda (x y) 
      (if (funcall f y)
        (append x (list y))
        x
      )
    )
    (cons nil L)
  )
)

(assert (equal (filtrar (lambda (x) (eq x 1) ) '(1 2 3 4 5 1 1 1))  '(1 1 1 1)  ))

(defun pertenece (e l)
  (reduce 
    (lambda (x y) (or x y))
    (mapcar 
      (lambda (x) (eq e x))
      l
    )
  )
)

(defun diferencia (base quitar)
  (cond
    (
      (null base) 
      nil
    )
    (
      (pertenece (car base) quitar) 
      (diferencia (cdr base) quitar) 
    )
    (
      T 
      (cons
        (car base)
        (diferencia (cdr base) quitar)
      )
    )
  )
)

; ------------------------

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
  trayectoria
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

(defun ultimo_vertice (trayectoria)
  (car trayectoria)
)


; ----------------- parte abstracta, independiente de la implementación de los tipos de dato ------------
(defun posibles_vecinos (grafo trayectoria)
  (diferencia
    (vecinos (ultimo_vertice trayectoria) grafo)
    (vertices_de_trayectoria trayectoria)
  )
)
(assert (equal (posibles_vecinos grafo '(a b c)) '(f)))

(assert
  (equal (posibles_vecinos grafo '(n d c b a)) '(j m))
)

(defun expandir_trayectoria (grafo trayectoria)
  (mapcar
    (lambda (v) (construir_trayectoria_hacia_vertice v trayectoria))
    (posibles_vecinos grafo trayectoria)
  )
)

(assert (equal 
  (expandir_trayectoria grafo '(d c b a))
  '((n d c b a) (e d c b a))
))

(assert (equal
  (expandir_trayectoria grafo '(n d c b a))
  '((J N D C B A) (M N D C B A))
))

(assert (equal
  (expandir_trayectoria grafo '(e d c b a))
  nil
))


(defun expandir_trayectorias (grafo trayectorias)
  (apply 
    'append 
    (mapcar 
      (lambda (tr) (expandir_trayectoria grafo tr)) 
      trayectorias
    )
  )
)

(assert (equal
  (expandir_trayectorias grafo '((n d c b a) (e d c b a)) )
  '((J N D C B A) (M N D C B A))
))

(defun trayectoria_que_alcanza (nodo trayectorias)
  (car 
    (filtrar 
      (lambda (tr) (trayectoria_alcanza tr nodo ) )
      trayectorias
    )
  )
)

(assert (equal
  (trayectoria_que_alcanza 'n '((n d c b a) (e d c b a)) )
  '(n d c b a)
))

(defun existe_trayectoria_que_alcanza (nodo trayectorias)
  (not (null 
    (trayectoria_que_alcanza nodo trayectorias)
  ))
)

(assert (equal
  (existe_trayectoria_que_alcanza 'n '((n d c b a) (e d c b a)) )
  t
))


(defun GPS (inicio fin grafo &optional (trayectorias (list (list inicio))))
  (cond 
    ( (null trayectorias) nil )
    ( (existe_trayectoria_que_alcanza fin trayectorias) (trayectoria_que_alcanza fin trayectorias) )
    ( T (GPS inicio fin grafo (expandir_trayectorias grafo trayectorias)))
  )
)

(assert (equal (GPS 'a 'b grafo) '(b a)))
(assert (equal (GPS 'a 'd grafo) '(d c b a)))

(assert (equal
  (GPS 'a 'l grafo)
  '(L H G F A)
))

(assert (equal 
  (GPS 'a 'k grafo) 
  '(K J N D C B A)
))
