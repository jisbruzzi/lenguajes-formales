(load 'control_flujo)
(load 'conocidas)

(defun buscar (nombre ambiente)
  (cond
    ((null  nombre) nil)
    ((eq 'T nombre) T)
    ((numberp nombre) nombre)
    ((null ambiente) nil);estom implica que no es una constante
    ((eq (car ambiente) nombre) (cadr ambiente))
    (T (buscar nombre (cddr ambiente)))
  )
)

(defun ampliar_ambiente (nombres valores ambiente)
  (append
    (reduce 'append (mapcar (lambda (x y) (list x y)) nombres valores))
    ambiente
  )
)

(defun aplicar (fn argumentos ambiente)
  (print 'aplicar)
  (if (not (null fn))
    (let 
      (print fn)
      (print argumentos)
      (print ambiente)
    )
    (+ 1 2)
  )
  
  (cond
    ((numberp fn) fn)
    ((null fn) nil)
    (
      (and (atom  fn) (es_conocida fn))
      (funcall 
        (funcion_conocida fn) 
        argumentos 
        (lambda (e) (evaluar e ambiente))
      ) 
    )
    (
      (and (atom  fn) (not (es_conocida fn)))
      (aplicar (buscar fn  ambiente) argumentos ambiente)
    )
    (
      (eq (car fn) 'lambda)
      (evaluar 
        (caddr fn)
        (ampliar_ambiente (cadr fn) argumentos ambiente)
      )
    )
    (
      T
      nil
    )
  )
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
      (eq (car expresion) 'lambda) 
      expresion
    )
    (
      T
      (aplicar 
        (car expresion)
        (mapcar
          (lambda (e) (evaluar e ambiente))
          (cdr expresion)
        )
        ambiente
      )
    )
  )
)