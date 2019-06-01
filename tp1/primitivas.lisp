;la trayectoria va a ser una lista de vértices (nombre "código")
;el primer elemento de ña trayectoria es el "fin", el último elemento de la trayectoria es el "inicio"
;un nodo va a ser un vértice en nomnbre "código"

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



(defun ultimo_vertice (trayectoria)
  (car trayectoria)
)