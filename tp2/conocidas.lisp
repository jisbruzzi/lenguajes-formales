(load 'diccionario)
(load 'macros)
(setq conocidas
  (list
    (cf car)
    (cf cdr)
    (cf list)
    (cf *)
    (cf +)
    (cf cons)
    (cf numberp)
    (list 
      (quote mapcar)
      (lambda (args evfn)
        (apply 
          'mapcar 
          (cons
            (lambda (elemento)
              (funcall evfn 
                  (list (car args) (list `quote elemento))
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