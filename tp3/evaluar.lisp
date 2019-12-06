(load 'lispizar)
(load 'polaquizar)


(defun valor_numerico (instruccion memoria buscar*)
  (cond
    ((eq (car instruccion) 'literal ) (cadr instruccion))
    ((eq (car instruccion) 'referencia ) (funcall buscar* (cadr instruccion) memoria))
  )
)

(defun n2b (n) (> n 0))
(defun b2n (b) (if b 1 0))

(defun operar (nombre_operacion izq der memoria buscar*)
  (funcall 
    (lambda (vi vd op)
      (cond
        ( (eq op 'sumar) (+ vi vd))
        ( (eq op 'restar) (- vi vd))
        ( (eq op 'multiplicar) (* vi vd))
        ( (eq op 'dividir) (/ vi vd))
        ( (eq op 'modulo) (% vi vd))
        ( (eq op 'menor) (b2n (< vi vd)))
        ( (eq op 'mayor) (b2n (> vi vd)))
        ( (eq op 'menor_o_igual) (b2n (<= vi vd)))
        ( (eq op 'mayor_o_igual) (b2n (>= vi vd)))
        ( (eq op 'and) (b2n (and (n2b vi) (n2b vd))))
        ( (eq op 'or) (b2n (or (n2b vi) (n2b vd)) ))
        ( (eq op 'xor) (b2n (and (or (n2b vi) (n2b vd)) (not (and (n2b vi) (n2b vd))))))
        ( (eq op 'igual) (b2n (eq vi vd)))
      )
    )
    (valor_numerico izq memoria  buscar*)
    (valor_numerico der memoria  buscar*)
    nombre_operacion
  )
)

(defun correr_instruccion_polaca (instruccion stack memoria buscar* reasignar_var*)
  (funcall 
    (lambda (push pop_pop_push pop_push stack_limpio soy)
      (list
        (cond
          (
            (funcall soy 'push_literal) (funcall push 'literal  (cadr instruccion) ) 
          )
          (
            (funcall soy 'pop_pop_operar_push)
            (funcall pop_pop_push  
              'literal 
              (operar (cadr instruccion) (cadr stack) (car stack) memoria buscar*)
            )
          )
          (
            (funcall soy 'push_referencia)
            (funcall push 'referencia (cadr instruccion) )
          )
          (
            (funcall soy 'ASIGNAR_Y_PUSH_REFERENCIA) (funcall pop_push 'referencia (cadr instruccion))
          )
          (
            (funcall soy 'finalizar)
            (funcall stack_limpio 'literal (valor_numerico (car stack) memoria buscar*));stack
          )
        )

        (if (funcall soy 'ASIGNAR_Y_PUSH_REFERENCIA)
          (funcall reasignar_var* 
            (cadr instruccion) 
            (valor_numerico (car stack) memoria buscar*)
            memoria 
          )
          memoria
        )
      )
    )
    (lambda (tipo arg) (cons (list tipo arg) stack));push
    (lambda (tipo arg) (cons (list tipo arg) (cddr stack)));pop_pop_push
    (lambda (tipo arg) (cons (list tipo arg) (cdr stack)));pop_push
    (lambda (tipo arg) (list (list tipo arg) ));stack_limpio
    (lambda (v) (eq (car instruccion) v))
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
