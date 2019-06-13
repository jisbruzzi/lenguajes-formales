(defun exc (mensaje)
  (print mensaje)
  (exit)
)

(defun es_operador (a)
  (pertenece a '(+ - * / % < > <= >= && || ^^ ==))
)

(defun buscar_en_memoria (k memoria)
  (cond
    ( (eq (car memoria) k) (cadr memoria) )
    ( 
      (null memoria) 
      (exc (list "Una variable no se encuentra en el ambiente" k))
    )
    ( T (buscar_en_memoria k (cdr memoria)) )
  )
)

(defun reemplazar_nil_por_cero (x) (if (null x) 0 x) )

(defun operar (op izq der)
  (reemplazar_nil_por_cero (cond
    ((eq op '+) (+ izq der))
    ((eq op '-) (- izq der))
    ((eq op '*) (* izq der))
    ((eq op '/) (/ izq der))
    ((eq op '%) (% izq der))
    ((eq op '<) (< izq der))
    ((eq op '>) (> izq der))
    ((eq op '<=) (<= izq der))
    ((eq op '>=) (>= izq der))
    ((eq op '&&) (and izq der))
    ((eq op '||) (or izq der))
    ((eq op '^^) (and (or izq der) (not (and izq der))))
    ((eq op '==) (eq izq der))
    (T (exc (list "No se sabe operar con el operador" op)))
  ))
)

(defun valor (expresion memoria &optional (operadores nil) (operandos nil))
  (cond
    (
      (and (atom expresion) (not (null expresion)))
      (cond 
        ((numberp expresion) expresion)
        ((stringp expresion) expresion)
        (T (buscar_en_memoria expresion memoria))
      )
    )
    (
      (and (null expresion) (null operadores))
      (car operandos)
    )
    (
      (and (null expresion) (not (null operadores)))
      (valor
        expresion
        memoria
        (cdr operadores)
        (cons 
          (operar (car operadores) (cadr operandos) (car operandos)) 
          (cddr operandos)
        )
      )
    )
    (
      (and (es_operador (car expresion)) (null operadores))
      (valor 
        (cdr expresion) 
        memoria 
        (cons (car expresion) operadores) 
        operandos 
      )
    )
    (
      (and (es_operador (car expresion)) (not (null operadores)) )
      (if (< (peso (car operadores)) (peso (car expresion)) )
        (valor 
          (cdr expresion) 
          memoria 
          (cons (car expresion) operadores) 
          operandos
        )
        (valor
          expresion
          memoria
          (cdr operadores)
          operandos
        )
      )
    )
    (
      (or (numberp (car expresion)) (symbolp (car expresion)))
      (valor
        (cdr expresion)
        memoria
        operadores
        (cons (valor (car expresion) memoria) operandos)
      )
    )

    (
      T
      (exc (list "No es una expresión cuyo valor sepa calcular." expresion))
    )

  )
)


(defun modificar_flujo (instruccion cola_programa entrada memoria salida)
  (cond
    (
      (es_if  instruccion)
      (if (not (eq (valor (cadr instruccion) memoria) 0 ))
        (append (caddr instruccion) cola_programa)
        (if (es_if_con_else instruccion)
          (append (car (cddddr instruccion)) cola_programa)
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
      (asignar (car instruccion) (valor (cddr instruccion) memoria) memoria)
    )
    (
      (es_asignacion_operacion instruccion)
      (modificar_memoria
        (list 
          (car instruccion) 
          '= 
          (car instruccion) 
          (operacion_de (cadr instruccion))
          (if (es_postfijo (cadr instruccion))
            1
            (valor (cddr instruccion) memoria)
          )
        )
        entrada
        memoria
        salida
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
      (cons (valor (cadr instruccion) memoria) salida)
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
(defun operacion_de (eqop) 
  (cond
    ((eq eqop '+=) '+)
    ((eq eqop '-=) '-)
    ((eq eqop '*=) '*)
    ((eq eqop '/=) '/)
    ((eq eqop '%=) '%)
    ((eq eqop '++) '+)
    ((eq eqop '--) '-)
    (T (exc (list "No conozco la operación correspondiente a " eqop)))
  )
)
(defun es_postfijo (eqop)
  (or (eq '++ eqop) (eq '-- eqop))
)
(defun es_prefijo (instruccion) 
  (pertenece (car instruccion) '(++ --))
)
(defun es_if (instruccion) 
  (eq (car instruccion) 'if)
)
(defun es_if_con_else (instruccion) 
  (eq (car (cdddr instruccion)) 'else)
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
  (cond
    ( (null prg) (reverse sal) )
    (
      (es_funcion_conocida (car prg))
      (ejec
        (modificar_flujo (car prg) (cdr prg) ent mem sal)
        (modificar_entrada (car prg) ent mem sal)
        (modificar_memoria (car prg) ent mem sal)
        (modificar_salida  (car prg) ent mem sal)
      )
    )
    (
      T
      (exc (list "Esa funcion no es conocida" (car prg)))
    )
  )
)

(defun asignar (k v memoria)
  (cond
    (
      (eq (car memoria) k)
      (append (list k v) (cddr memoria))
    )
    (
      (null memoria)
      (list k v)
    )
    (
      T
      (append 
        (list (car memoria) (cadr memoria)) 
        (asignar k v (cddr memoria))
      )
    )
  )
)
; ---- ejecuta  instrucciones de tipo INT ------ ;
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
	      (eq (cadr instruccion) '=)
	      (not (symbolp (caddr instruccion)))
      )
      (asignar_declaracion_variables
        (cdddr instruccion)
        (asignar (car instruccion) (caddr instruccion) memoria)
      )
    )
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
      T
      (exc "No es una asignación de variable")
    )
  )
)
; -------------------------------- PRINCIPAL --------------------- ;
(defun run (prg ent &optional (mem nil))
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
