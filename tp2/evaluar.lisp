(load 'control_flujo)


(defun buscar (nombre ambiente)
  (cond
    ((null nombre) nil)
    ((eq 'T nombre) T)
    ((numberp nombre) nombre)
    ((null ambiente) nil);estom implica que no es una constante
    ((eq (car ambiente) nombre) (cadr ambiente))
    (T (buscar nombre (cddr ambiente)))
  )
)

(defun aplicar (fn argumentos ambiente)
  fn
)
(defun evaluar (expresion ambiente)
  (cond 
    ( (atom expresion) (buscar expresion ambiente)); en buscar puede entrar null!
    (
      (es_control_flujo (car expresion))
      (funcall 
        (funcion_control_flujo (car expresion))
        expresion 
        (lambda (e) (evaluar e ambiente))
      )
    )
    (
      T
      (aplicar 
        (print (car expresion))
        (mapcar
          (lambda (e) (evaluar e ambiente))
          (cdr expresion)
        )
        ambiente
      )
    )
  )
)