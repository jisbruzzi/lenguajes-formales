; ------------------------
(load 'generalidades)
(load 'primitivas)

; ----------------- parte abstracta, independiente de la implementación de los tipos de dato ------------
(defun posibles_vecinos (grafo trayectoria)
  (diferencia
    (vecinos (ultimo_vertice trayectoria) grafo)
    (vertices_de_trayectoria trayectoria)
  )
)

(defun expandir_trayectoria (grafo trayectoria)
  (mapcar
    (lambda (v) (construir_trayectoria_hacia_vertice v trayectoria))
    (posibles_vecinos grafo trayectoria)
  )
)

(defun expandir_trayectorias (grafo trayectorias)
  (apply 
    'append 
    (mapcar 
      (lambda (tr) (expandir_trayectoria grafo tr)) 
      trayectorias
    )
  )
)



(defun trayectoria_que_alcanza (nodo trayectorias)
  (car 
    (filtrar 
      (lambda (tr) (trayectoria_alcanza tr nodo ) )
      trayectorias
    )
  )
)



(defun existe_trayectoria_que_alcanza (nodo trayectorias)
  (not (null 
    (trayectoria_que_alcanza nodo trayectorias)
  ))
)



(defun GPS (inicio fin grafo &optional (trayectorias (list (list inicio))))
  (cond 
    ( (null trayectorias) nil )
    ( (existe_trayectoria_que_alcanza fin trayectorias) (trayectoria_que_alcanza fin trayectorias) )
    ( T (GPS inicio fin grafo (expandir_trayectorias grafo trayectorias)))
  )
)

