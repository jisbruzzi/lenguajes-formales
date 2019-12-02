(defun polaquizar (arbol_ops)
  (funcall 
    (lambda (operacion izq der)
      (cond
        (
          (eq operacion 'asignar)
          (append 
            (polaquizar der)
            (list (list 'asignar_y_push_referencia izq))
          )
        )
        (
          (eq operacion 'literal)
          (list (list 'push_literal izq))
        )
        (
          (eq operacion 'variable)
          (list (list 'push_referencia izq))
        )
        (
          T
          (append
            (polaquizar izq)
            (polaquizar der)
            (list (list 'pop_pop_operar_push operacion))
          )
        )
      )
    )
    (car arbol_ops)
    (cadr arbol_ops)
    (caddr arbol_ops)
  )
)