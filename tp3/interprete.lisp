(defun ejec (prg ent mem &optional (sal nil))
  (cond
    ( (null prg) (reverse sal) )
    (
      (es_funcion_conocida (car prg)) 
      (correr_funcion_conocida 
	(car prg)
	Y más cosasssss
      )
    )
  )
)
(defun run (prg ent &optional (mem nil))
  (cond
    ((null prg) (exc "no hay programa"))
    (
      (eq (caar prog) 'int)
      (run 
	(cdr prg) 
	ent 
	(asignar (cdr prg) mem)
      )
    )
    (
      (eq (caar prg) 'main)
      (ejec (cadar prg) ent mem)
    )
    (
      T
      (exc "El programa no empieza con main ni int")
    )
  )
)
