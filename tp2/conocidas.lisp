(load 'diccionario)
(load 'macros)
(setq conocidas
  (list
    (cf car)
    (cf cdr)
    (cf list)
    (cf *)
    (cf +)
    (cf -)
    (cf cons)
    (cf numberp)
    (cf eq)
    (list 
      (quote mapcar)
      (lambda (args evfn)
        (apply 
          'mapcar 
          (cons
            (lambda (&rest elementos)
              (funcall evfn
                (cons 
                  (car args) 
                  (mapcar (lambda (e) (list `quote e)) elementos)
                )
              )
            )
            (cdr args)
          )
        )
      )
    )
  )
)

(defun es_conocida (nombre)
  (en_diccionario conocidas nombre)
)

(defun funcion_conocida (nombre)
  (buscar_diccionario conocidas nombre)
)