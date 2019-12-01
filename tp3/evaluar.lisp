(load 'lispizar)

(defun operar* (op izq der)
  (reemplazar_nil_por_cero (cond
    ((eq op 'sumar) (+ izq der))
    ((eq op 'restar) (- izq der))
    ((eq op 'multiplicar) (* izq der))
    ((eq op 'dividir) (/ izq der))
    ((eq op 'modulo) (% izq der))
    ((eq op 'menor) (< izq der))
    ((eq op 'mayor) (> izq der))
    ((eq op 'menor_o_igual) (<= izq der))
    ((eq op 'mayor_o_igual) (>= izq der))
    ((eq op 'and) (and izq der))
    ((eq op 'or) (or izq der))
    ((eq op 'xor) (and (or izq der) (not (and izq der))))
    ((eq op 'igual) (eq izq der))
  ))
)

(defun es_terminal_algebraico (e)
  (pertenece (car e) '(variable asignar literal multiplicar dividir modulo and or xor))
)

;Primitivas sobre la lispización (que está oculta al resto del intérprete)
(defun evaluar* (expresion memoria constantes buscar* reasignar*)
  (evaluar_real* (lispizar* expresion) memoria constantes buscar* reasignar*)
)

(defun evaluar_real* (expresion memoria constantes buscar* reasignar*)
  (funcall 
    (lambda (operacion izq der ejecutar** evaluar**)
      (cond
        ( (eq operacion 'literal) izq )
        ( 
          (pertenece operacion '(variable asignar))
          (funcall buscar* izq constantes memoria) 
        )
        ( 
          (pertenece operacion '(sumar restar menor mayor menor_o_igual mayor_o_igual igual))
          
          (funcall 
            (lambda (memizq)
              (funcall 
                (lambda (memder)
                  (operar*
                    operacion
                    (funcall evaluar** izq 
                      (funcall 
                        (lambda (ti td)
                          (cond
                            ( (and ti td) memder)
                            ( (and ti (not td)) memizq)
                            ( (not ti) memoria)
                          )
                        )
                        (es_terminal_algebraico izq) 
                        (es_terminal_algebraico der)
                      )
                    )
                    (funcall evaluar** 
                      der 
                      (if (es_terminal_algebraico der) memder memizq ) 
                    )
                  )
                )
                (funcall ejecutar** der memizq)
              )
            )
            (funcall ejecutar** izq memoria)
          )
              
            
        )
        
        ( 
          (pertenece operacion '(multiplicar dividir modulo and or xor))
          (operar*
            operacion
            (funcall evaluar** izq memoria)
            (funcall evaluar** der memoria)
          )
        )
      )
      
      
    )
    (car expresion)
    (cadr expresion)
    (caddr expresion)
    (lambda (expresion memoria) 
      (ejecutar_real* expresion memoria constantes buscar* reasignar*) 
    )
    (lambda (expresion memoria) 
      (evaluar_real* expresion memoria constantes buscar* reasignar*) 
    )
  )
)
(defun ejecutar* (expresion memoria constantes buscar* reasignar*)
  (ejecutar_real* (lispizar* expresion) memoria constantes buscar* reasignar*)
)

(defun ejecutar_real* (expresion memoria constantes buscar* reasignar*)
  (funcall 
    (lambda (operacion izq der ejecutar** evaluar**)
      (cond
        ( 
          (eq operacion 'asignar)
          (funcall 
            (lambda (memder)
              (reasignar* 
                izq 
                (funcall evaluar** der memder) 
                constantes 
                memder
              )
            )
            (funcall ejecutar** der memoria)
          )
          
        )

        ( 
          (pertenece operacion '(literal variable))
          memoria
        )
        (
          T
          (funcall 
            ejecutar** 
            der 
            (funcall ejecutar** izq memoria);memoria izq
          );memoria der
        )
      )
      
      
    )
    (car expresion)
    (cadr expresion)
    (caddr expresion)
    (lambda (expresion memoria) 
      (ejecutar_real* expresion memoria constantes buscar* reasignar*) 
    )
    (lambda (expresion memoria) 
      (evaluar_real* expresion memoria constantes buscar* reasignar*) 
    )
  )
)