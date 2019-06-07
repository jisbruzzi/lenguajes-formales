; ------------------------ GENERALIDADES -----;

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

; ------------- PRIMITIVAS Se refieren a los caminos y los vértices en un nivel de abstracción menor ----- ;

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
; -------------------------- ALGORITMO --------a------- ;
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

(defun trayectorias_que_alcanzan (nodo trayectorias)
  (filtrar 
    (lambda (tr) (trayectoria_alcanza tr nodo ) )
    trayectorias
  )
)

(defun trayectoria_que_alcanza (nodo trayectorias)
  (car (trayectorias_que_alcanzan nodo trayectorias))
)



(defun existe_trayectoria_que_alcanza (nodo trayectorias)
  (not (null 
    (trayectoria_que_alcanza nodo trayectorias)
  ))
)



(defun GPS (inicio fin grafo &optional (trayectorias (list (list inicio))))
  (cond 
    ( (null trayectorias) nil )
    ( (existe_trayectoria_que_alcanza fin trayectorias) (trayectorias_que_alcanzan fin trayectorias) )
    ( T (GPS inicio fin grafo (expandir_trayectorias grafo trayectorias)))
  )
)


; --------------------- INTERFAZ con esto se puede tener una función GPS que traduzca caminos al castellano como en el enunciado ----- ;


(defun calle_valida (calle)
  (pertenece (print calle )
    (reduce
      'append
      (mapcar 
        (lambda (x) (cadr x))
        diccionario
      )
    )
  )
)

(defun misma_esquina (e1 e2)
  (or
    (and
      (eq (car e1) (car e2))
      (eq (cadr e1) (cadr e2))
    )
    (and
      (eq (car e1) (cadr e2))
      (eq (cadr e1) (car e2))
    )
  )
  
)

(defun vertice_de_esquina (esq &optional (dic diccionario))
  (cond 
    ( (null dic) nil)
    ( (misma_esquina esq (cadar dic)) (caar dic))
    (T (vertice_de_esquina esq (cdr dic)))
  )
)

(defun esquina_valida (esq)
  (not (null (vertice_de_esquina esq)))
)

(defun esquina_de_vertice (v &optional (dic diccionario))
  (cond
    ((null dic) nil)
    ((eq (caar dic) v) (cadar dic))
    ( t (esquina_de_vertice v (cdr dic)))
  )
)

(defun sinprimero (l) (cdr l))
(defun sinultimo (l) (reverse (sinprimero (reverse l))))
(defun calle_comun (e1 e2)
  (cond
    ((pertenece (car e1) e2) (car e1))
    ((pertenece (cadr e1) e2) (cadr e1))
    (t nil)
  )
)
(defun calles_recorridas_por_ruta (ruta_esquinas)
  (mapcar
    (lambda (e1 e2) (calle_comun e1 e2))
    (sinultimo ruta_esquinas)
    (sinprimero ruta_esquinas)
  )
)
(defun colocar_unos (calles_recorridas)
  (mapcar
    (lambda (c) (list c 1))
    calles_recorridas
  )
)

(defun agregar_movimiento (movimiento resto_movimientos)
  (cond
    ((null resto_movimientos) (list movimiento))
    (
      (eq (car movimiento) (caar resto_movimientos))
      (cons
        (list (car movimiento) (+ (cadr movimiento) (cadar resto_movimientos)) )
        (cdr resto_movimientos)
      )
    )
    (
      t
      (cons movimiento resto_movimientos)
    )
  )
)

(defun contar_movimientos (calles_recorridas)
  (cond
    ( (null calles_recorridas) nil)
    ( t (agregar_movimiento 
      (car calles_recorridas) 
      (contar_movimientos (cdr calles_recorridas))
    ))
  )
)

(defun agregar_calle_siguiente (movimientos)
  (mapcar
    (lambda (actual siguiente) 
      (append actual (list (car siguiente)))
    )
    movimientos
    (append (sinprimero movimientos) '(nil))
  )
)

(defun traducir_movimiento_completo_al_castellano (m)
  ;(calle cuadras siguiente calle)
  (append 
    (list 'recorrer (cadr m) 'cuadras 'por (car m))
    (if (null (caddr m))
      (list 'hasta 'llegar 'a 'destino)
      (list 'y 'doblar 'en (caddr m))
    )
  )
)

(defun valor_default (default v)
  (if (null v)
    default
    v
  )
)

(defun ruta_para_mostrar (ruta)
  (valor_default (list 'ya 'se 'encuentra 'en 'destino) 
    (mapcar 'traducir_movimiento_completo_al_castellano
      (agregar_calle_siguiente 
        (contar_movimientos 
          (colocar_unos 
            (calles_recorridas_por_ruta 
              (mapcar 'esquina_de_vertice (reverse ruta))
            )
          )
        )
      )
    )
  )
)

; ---------- INTERFAZ "EN CASTELLANO" ------ ;

(defun GPS_INTERFAZ_ENUNCIADO (inicio fin grafo)
  (mapcar 'ruta_para_mostrar
    (GPS
      (vertice_de_esquina inicio)
      (vertice_de_esquina fin)
      grafo
    )
  )
)


