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

(defun ruta_para_mostrar (ruta)
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