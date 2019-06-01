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
(defun vecinos (n grafo)
  (cadr 
    (reduce
      (lambda (x y) (cond 
	      ( (eq (car x) n) x)
	      ( (eq (car y) n) y)
	      (T x)
      ))
      grafo
    )
  )
)

(assert (equal (vecinos 'k grafo) '(o)) )

(defun pertenece (e L)
  (reduce (lambda (x y) (or x y)) (mapcar (lambda (x) (eq e x)) L))
)

(assert (pertenece 1 '(1 2 3)))
(assert (pertenece 2 '(1 2 3)))
(assert (pertenece 3 '(1 2 3)))

(defun construir_agregar_si_corresponde (L_quitar)
  (lambda (L e)
    (if (pertenece e L_quitar)
      L
      (append L (list e))
    )
  )
)


(defun diferencia (L L_quitar)
  (reduce (construir_agregar_si_corresponde L_quitar) (cons nil L))
)

(assert (equal (diferencia '(1 2 3 4 5 6 7 8 9 10) '(1 5 6))  '(2 3 4 7 8 9 10) ))


(defun trayectorias_posibles_desde_trayectoria () )
;construye un paso más de la trayectoria de la cabeza
(defun construir_trayectorias (i f grafo tray)
  (append
    (mapcar
      (lambda (x) (cons x (car tray) )  )
      (diferencia
	      (vecinos i grafo);este vecinos i grafo me extraña
	      (car tray)
      )
    )
    (cdr tray)
  )
)


'(defun GPS (i f grafo &optional (tray (list (list i))))
  (cond 
    ( (null tray) nil )
    ( (eq (caar tray) f) (reverse (car tray)))
    ( T (GPS i f grafo (construir_trayectorias i f grafo tray)))
  )
)



(GPS '(PaseoColon Independencia) '(Defensa Belgrano)
 grafo diccionario
)
