(load 'diccionario)
(load 'macros)
(setq control_flujo
  (list
    (l quote (cadr expr))
    (l and 
      (if (null (ev (cadr expr)))
        nil
        (ev (caddr expr))
      )
    )
    (l or
      (if (null (ev (cadr expr)))
        (ev (caddr expr))
        (ev (cadr expr))
      )
    )
    (l if
      (if (not (null (ev (cadr expr)) ))
        (ev (caddr expr))
        (ev (cadddr expr))
      )
    )
  )
)

(defun es_control_flujo (nombre)
    (en_diccionario control_flujo nombre)
)

(defun funcion_control_flujo (nombre)
    (buscar_diccionario control_flujo nombre)
)