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
  (and
    (eq (car e1) (car e2))
    (eq (cadr e1) (cadr e2))
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
