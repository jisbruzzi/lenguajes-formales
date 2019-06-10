; ------------------------ GENERALIDADES -----;
(defun iguales_a_todo_nivel (x y)
  (cond
    (
      (and (atom x) (atom y))
      (eq x y)
    )
    (
      (and (listp x) (listp y))
      (reduce (lambda (xx yy) (and xx yy))
        (mapcar 'iguales_a_todo_nivel x y)
      )
    )
    (T nil)
  )
)
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

(defun trayectoria_pasa_por (trayectoria nodo)
  (cond 
    ( (null trayectoria ) nil)
    ( (eq (car trayectoria) nodo) T)
    (T (trayectoria_pasa_por (cdr trayectoria) nodo))
  )
)
; -------------------------- ALGORITMO --------a------- ;
; ----------------- parte abstracta, independiente de la implementación de los tipos de dato ------------
(defun trayectoria_hasta (nodo trayectoria)
  (cond
    ( (null trayectoria) nil)
    ( (eq (car trayectoria) nodo) trayectoria)
    (T (trayectoria_hasta nodo (cdr trayectoria)))
  )
)
(defun trayectorias_hasta (nodo trayectorias)
  (mapcar (lambda (tr) (trayectoria_hasta nodo tr)) trayectorias)
)
(defun posibles_vecinos (grafo trayectoria)
  (diferencia
    (vecinos (ultimo_vertice trayectoria) grafo)
    (vertices_de_trayectoria trayectoria)
  )
)

(defun expandir_trayectoria (grafo trayectoria lam)
  (funcall lam trayectoria
    (mapcar
      (lambda (v) (construir_trayectoria_hacia_vertice v trayectoria))
      (posibles_vecinos grafo trayectoria)
    )
  )
)

(defun expandir_trayectorias_lambda (grafo trayectorias lam)
  (apply 
    'append 
    (mapcar 
      (lambda (tr) (expandir_trayectoria grafo tr lam)) 
      trayectorias
    )
  )
)


(defun expandir_trayectorias (grafo trayectorias)
  (expandir_trayectorias_lambda grafo trayectorias (lambda (tray res) res))
)

(defun expandir_trayectorias_si_posible (grafo trayectorias)
  (expandir_trayectorias_lambda grafo trayectorias 
    (lambda (tray res) 
      (if (null res)
        (list tray)
        res
      )
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

(defun trayectorias_que_pasan_por (nodo trayectorias)
  (filtrar
    (lambda (tr) (trayectoria_pasa_por tr nodo))
    trayectorias
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
    ( (existe_trayectoria_que_alcanza fin trayectorias) (trayectorias_que_alcanzan fin trayectorias) )
    ( T (GPS inicio fin grafo (expandir_trayectorias grafo trayectorias)))
  )
)


(defun GPS_TODOS_CAMINOS (inicio fin grafo &optional (trayectorias (list (list inicio))) (trayectorias_anteriores nil) )
  (cond 
    ( (null trayectorias) nil )
    (
      (iguales_a_todo_nivel trayectorias trayectorias_anteriores) 
      (trayectorias_hasta fin (trayectorias_que_pasan_por fin trayectorias))
    )
    ( T (GPS_TODOS_CAMINOS inicio fin grafo (expandir_trayectorias_si_posible grafo trayectorias) trayectorias))
  )
)

(defun trayectoria_mas_lambda (trayectorias lam &optional (la_mas nil))
  (cond
    ((null trayectorias) la_mas)
    ((null la_mas) 
      (trayectoria_mas_lambda trayectorias lam (car trayectorias))
    )
    (
      (funcall lam (length (car trayectorias)) (length la_mas) )
      (trayectoria_mas_lambda (cdr trayectorias) lam (car trayectorias))
    )
    (
      T
      (trayectoria_mas_lambda (cdr trayectorias) lam la_mas)
    )
  )
)
(defun trayectoria_mas_corta (trayectorias)
  (trayectoria_mas_lambda trayectorias (lambda (x y) (< x y)) )
)
(defun trayectoria_mas_larga (trayectorias)
  (trayectoria_mas_lambda trayectorias (lambda (x y) (> x y)) )
)

(defun GPS_UNO_MENOR_UNO_MAYOR (inicio fin grafo)
  (list 
    (trayectoria_mas_corta (GPS_TODOS_CAMINOS inicio fin grafo))
    (trayectoria_mas_larga (GPS_TODOS_CAMINOS inicio fin grafo))
  )
)


; --------------------- INTERFAZ con esto se puede tener una función GPS que traduzca caminos al castellano como en el enunciado ----- ;


(defun calle_valida (calle)
  (pertenece calle
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

(defun GPS_UNO_MENOR_UNO_MAYOR_INTERFAZ_ENUNCIADO (inicio fin grafo)
  (mapcar 'ruta_para_mostrar
    (GPS_UNO_MENOR_UNO_MAYOR
      (vertice_de_esquina inicio)
      (vertice_de_esquina fin)
      grafo
    )
  )
)




