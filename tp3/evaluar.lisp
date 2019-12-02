(load 'lispizar)
(load 'polaquizar)


(defun valor_numerico (instruccion memoria buscar*)
  (cond
    ((eq (car instruccion) 'literal ) (cadr instruccion))
    ((eq (car instruccion) 'referencia ) (funcall buscar* (cadr instruccion) memoria))
  )
)

(defun operar (nombre_operacion izq der memoria buscar*)
  (funcall 
    (lambda (vi vd op)
      (cond
        ( (eq op 'sumar) (+ vi vd))
        ( (eq op 'restar) (- vi vd))
        ( (eq op 'multiplicar) (* vi vd))
        ( (eq op 'dividir) (/ vi vd))
        ( (eq op 'modulo) (% vi vd))
        ( (eq op 'menor) (< vi vd))
        ( (eq op 'mayor) (> vi vd))
        ( (eq op 'menor_o_igual) (<= vi vd))
        ( (eq op 'mayor_o_igual) (>= vi vd))
        ( (eq op 'and) (and vi vd))
        ( (eq op 'or) (or vi vd))
        ( (eq op 'xor) (and (or vi vd) (not (and vi vd))))
        ( (eq op 'igual) (eq vi vd))
      )
    )
    (valor_numerico izq memoria  buscar*)
    (valor_numerico der memoria  buscar*)
    nombre_operacion
  )
)

(defun correr_instruccion_polaca (instruccion stack memoria buscar* reasignar_var*)
  (cond
    (
      (eq (car instruccion) 'push_literal)
      (list 
        (cons 
          (list 
            'literal 
            (cadr instruccion)
          ) 
          stack
        ) 
        memoria
      )
    )
    (
      (eq (car instruccion) 'pop_pop_operar_push)
      (list 
        (cons 
          (list 
            'literal 
            (operar (cadr instruccion) (cadr stack) (car stack) memoria buscar*)
          )
          (cddr stack) 
        )
        memoria
      )
    )
    (
      (eq (car instruccion) 'push_referencia)
      (list
        (cons
          (list
            'referencia
            (cadr instruccion)
          )
          stack
        )
        memoria
      )
    )
    (
      (eq (car instruccion) 'ASIGNAR_Y_PUSH_REFERENCIA)
      (list
        (cons
          (list 'referencia (cadr instruccion))
          (cdr stack)
        )
        (funcall reasignar_var* 
          (cadr instruccion) 
          (valor_numerico (car stack) memoria buscar*)
          memoria 
        )
      )
    )
    (
      (eq (car instruccion) 'finalizar)
      (list
        (list (list 'literal (valor_numerico (car stack) memoria buscar*)));stack
        memoria
      )
    )
  )
)

(defun correr_polaca (instrucciones stack memoria buscar* reasignar*)
  (if (null instrucciones)
    (list stack memoria)
    (funcall 
      (lambda (resultado)
        (correr_polaca 
          (cdr instrucciones) 
          (car resultado)  
          (cadr resultado) 
          buscar* 
          reasignar*
        )
      )
      (correr_instruccion_polaca (car instrucciones) stack memoria buscar* reasignar*)
    )
  )
)

(defun nil_si_0 (v)
  (if (null v) 0 v)
)


; primitivas
(defun evaluar* (expresion memoria buscar* reasignar*)
  (nil_si_0 
    (cadr (caar (correr_polaca 
      (append (polaquizar (lispizar* expresion)) (list (list 'finalizar)) )
      (list ) 
      memoria 
      buscar* 
      reasignar* 
    )));caar: primer elemento del stack ;cadr: el literal ('literal x)
  )
)

(defun ejecutar* (expresion memoria buscar* reasignar*)
  (cadr (correr_polaca 
    (append (polaquizar (lispizar* expresion)) (list (list 'finalizar)) )
    (list ) 
    memoria  
    buscar* 
    reasignar* 
  ))
)
