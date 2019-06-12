(defun exc (mensaje)
  (print mensaje)
  (exit)
)
(defun modificar_flujo (instruccion cola_programa entrada memoria salida)
  (cond
    (
      (es_if  instruccion)
      (if (not (eq (valor (cadr instruccion) memoria) 0 ))
        (cons (caddr instruccion) cola_programa)
        (if (es_if_con_else instruccion)
          (cons (car (cddddr instruccion)) cola_programa)
          cola_programa
        )
      )
    )
    (
      (es_while instruccion)
      (if (not (eq (valor (cadr instruccion) memoria) 0 ))
        (append
          (caddr instruccion)
          (cons instruccion cola_programa)
        )
        cola_programa
      )
    )
    (
      T
      cola_programa
    )
  )
)

(defun modificar_memoria (instruccion entrada memoria salida)
  (cond
    (
      (es_scanf instruccion)
      (asignar (cadr instruccion) (car entrada)  memoria)
    )
    (
      (es_asignacion instruccion)
      (asignar (car instruccion) (valor (cddr entrada) memoria) memoria)
    )
    (
      (es_asignacion_operacion instruccion)
      (asignar 
        (car instruccion)
        (modificar_memoria
          (list 
            (car instruccion) 
            '= 
            (car instruccion) 
            (operacion_de (cadr instruccion))
            (if (es_postfijo instruccion)
              1
              (valor (cddr entrada) memoria)
            )
          )
          entrada
          memoria
          salida
        )
        memoria
      )
    )
    (
      (es_prefijo instruccion)
      (modificar_memoria
        (list 
          (cadr instruccion) 
          '= 
          (cadr instruccion) 
          (operacion_de (car instruccion))
          1
        )
        entrada
        memoria
        salida
      )
    )
    (
      T
      memoria
    )
  )
)
(defun modificar_salida (instruccion entrada memoria salida) 
  (cond
    ( 
      (es_printf instruccion)
      (cons (valor (cdr instruccion) memoria) salida)
    )
    (T salida)
  )
)
(defun modificar_entrada (instruccion entrada memoria salida) 
  (cond
    ( 
      (es_scanf instruccion)
      (cdr entrada)
    )
    (T entrada)
  )
)

(defun es_scanf (instruccion) (eq (car instruccion) 'scanf))
(defun es_printf (instruccion) (eq (car instruccion) 'printf))
(defun es_asignacion (instruccion) (eq (cadr instruccion) '=))
(defun es_asignacion_operacion (instruccion) 
  (pertenece (cadr instruccion) '(+= -= *= /= %= ++ --))
)
(defun es_prefijo (instruccion) 
  (pertenece (car instruccion) '(++ --))
)
(defun es_if (instruccion) 
  (eq (car instruccion) 'if)
)
(defun es_while (instruccion) 
  (eq (car instruccion) 'while)
)

(defun pertenece (v conjunto)
  (cond
    ((null conjunto) nil)
    ((eq (car conjunto) v) T)
    (T (pertenece v (cdr conjunto)))
  )
)

(defun es_funcion_conocida (instruccion)
  (or
    (es_scanf instruccion)
    (es_printf instruccion)
    (es_asignacion instruccion)
    (es_asignacion_operacion instruccion)
    (es_prefijo instruccion)
    (es_if instruccion)
    (es_while instruccion)
  )
)
(defun ejec (prg ent mem &optional (sal nil))
  (print 'eeeeeeeeeeeeeee)
  (print prg)
  (print ent)
  (print mem)
  (print sal)
  (cond
    ( (null prg) (reverse sal) )
    (
      (es_funcion_conocida (car prg)) 
      (ejec
        (cdr prg)
        (modificar_entrada (car prg) ent mem sal)
        (modificar_memoria (car prg) ent mem sal)
        (modificar_salida  (car prg) ent mem sal)
      )
    )
    (
      T
      (exc "Esa funcion no es conocida")
    )
  )
)

(defun asignar (k v memoria)
  (append (list k v) memoria)
)

(defun asignar_declaracion_variables (instruccion memoria)
  (cond
    (
      (eq (car instruccion) 'int) 
      (asignar_declaracion_variables (cdr instruccion) memoria)
    )
    ((null instruccion) memoria)
    (
      (and 
	(symbolp (car instruccion)) 
	(symbolp (cadr instruccion)) 
      )
      (asignar_declaracion_variables
        (cdr instruccion)
        (asignar (car instruccion) 0 memoria)
      )
    )
    (
      (and 
	(print (symbolp (car instruccion))) 
	(print (eq (print (cadr instruccion)) '=))
	(print (not (symbolp (caddr instruccion)))) 
      )
      (asignar_declaracion_variables
        (cdddr instruccion)
        (asignar (car instruccion) (cadr instruccion) memoria)
      )
    )
    (
      T
      (exc "No es una asignación de variable")
    )
  )
)
; -------------------------------- PRINCIPAL --------------------- ;
(defun run (prg ent &optional (mem nil))
  (print 'rrrrrrrrrrrrrr)
  (print prg)
  (print mem)
  (print ent)
  (cond
    ((null prg) (exc "no hay programa"))
    (
      (eq (caar prg) 'int)
      (run 
        (cdr prg) 
        ent 
        (asignar_declaracion_variables (car prg) mem)
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
