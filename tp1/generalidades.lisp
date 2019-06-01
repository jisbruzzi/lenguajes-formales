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