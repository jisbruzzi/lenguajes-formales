(defun ultimo (L)
  (car (reverse L))
)
(defun sin_ultimo (L)
  (reverse (cdr (reverse L)))
)

(defun es_valido (x)
  (not (and
    (listp x)
    (eq (car x) 'excepcion)
  ))
)

(defun todos (f L)
  (reduce
    (lambda (x y) (and x y))
    (mapcar (lambda (x) (funcall f x)) L )
  )
)
(defun primero (f L)
  (reduce
    (lambda (x y) 
      (if (funcall f x)
        x
        y
      )
    )
    (append L '(nil))
  )
)

(defun if_valido_lambda (&rest argumentos)
  (if (todos 'es_valido (sin_ultimo argumentos))
    (apply (ultimo argumentos) (sin_ultimo argumentos))
    (primero (lambda (x) (not (es_valido x))) (sin_ultimo argumentos))
  )
)

(defun exc (mensaje)
  (list 'excepcion mensaje)
)

(defun es_operador (a)
  (pertenece a '(+ - * / % < > <= >= && || ^^ ==))
)

(defun buscar_en_memoria* (k memoria)
  (cond
    ( (eq (car memoria) k) (cadr memoria) )
    ( 
      (null memoria) 
      (exc (list "Una variable no se encuentra en el ambiente" k))
    )
    ( T (buscar_en_memoria* k (cdr memoria)) )
  )
)

(defun reemplazar_nil_por_cero (x) (if (null x) 0 x) )

(defun operar* (op izq der)
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

(defun valor* (expresion memoria &optional (operadores nil) (operandos nil))
  (cond
    (
      (and (atom expresion) (not (null expresion)))
      (cond 
        ((numberp expresion) expresion)
        ((stringp expresion) expresion)
        (T (buscar_en_memoria* expresion memoria))
      )
    )
    (
      (and (null expresion) (null operadores))
      (car operandos)
    )
    (
      (and (null expresion) (not (null operadores)))
      (if_valido_lambda (operar* (car operadores) (cadr operandos) (car operandos))
        (lambda (resultado_operacion)
          (valor*
            expresion
            memoria
            (cdr operadores)
            (cons 
              resultado_operacion      
              (cddr operandos)
            )
          )
        )
      )
    )
    (
      (and (es_operador (car expresion)) (null operadores))
      (valor*
        (cdr expresion) 
        memoria 
        (cons (car expresion) operadores) 
        operandos 
      )
    )
    (
      (and (es_operador (car expresion)) (not (null operadores)) )
      (if (< (peso (car operadores)) (peso (car expresion)) )
        (valor*
          (cdr expresion) 
          memoria 
          (cons (car expresion) operadores) 
          operandos
        )
        (valor*
          expresion
          memoria
          (cdr operadores)
          operandos
        )
      )
    )
    (
      (or (numberp (car expresion)) (symbolp (car expresion)))
      (if_valido_lambda (valor* (car expresion) memoria)
        (lambda (v) 
          (valor*
            (cdr expresion)
            memoria
            operadores
            (cons v operandos)
          )
        )
      )
    )

    (
      T
      (exc (list "No es una expresión cuyo valor sepa calcular." expresion))
    )

  )
)


(defun modificar_flujo* (instruccion cola_programa entrada memoria salida)
  (cond
    (
      (es_if  instruccion)
      (if_valido_lambda (valor* (cadr instruccion) memoria)
        (lambda (valor)
          (if (not (eq valor 0 ))
            (append (caddr instruccion) cola_programa)
            (if (es_if_con_else instruccion)
              (append (car (cddddr instruccion)) cola_programa)
              cola_programa
            )
          )
        )
      )
    )
    (
      (es_while instruccion)
      (if_valido_lambda (valor* (cadr instruccion) memoria)
        (lambda (valor)
          (if (not (eq valor 0 ))
            (append
              (caddr instruccion)
              (cons instruccion cola_programa)
            )
            cola_programa
          )
        )
      )
    )
    (
      T
      cola_programa
    )
  )
)

(defun modificar_memoria* (instruccion entrada memoria salida)
  (cond
    (
      (es_scanf instruccion)
      (reasignar* (cadr instruccion) (car entrada)  memoria)
    )
    (
      (es_asignacion instruccion)
      (if_valido_lambda (valor* (cddr instruccion) memoria)
        (lambda (valor)
          (reasignar* (car instruccion) valor memoria)  
        )
      )
    )
    (
      (es_asignacion_operacion instruccion)
      (if_valido_lambda (valor* (cddr instruccion) memoria) (operacion_de* (cadr instruccion))
        (lambda (valor operacion)
          (modificar_memoria*
            (list 
              (car instruccion) 
              '= 
              (car instruccion) 
              operacion
              (if (es_postfijo (cadr instruccion))
                1
                valor
              )
            )
            entrada
            memoria
            salida
          )
        )
      )
    )
    (
      (es_prefijo instruccion)
      (if_valido_lambda (operacion_de* (car instruccion))
        (lambda (operacion)
          (modificar_memoria*
            (list 
              (cadr instruccion) 
              '= 
              (cadr instruccion) 
              operacion
              1
            )
            entrada
            memoria
            salida
          )
        )
      )
    )
    (
      T
      memoria
    )
  )
)
(defun modificar_salida* (instruccion entrada memoria salida) 
  (cond
    (
      (es_printf instruccion)
      (if_valido_lambda (valor* (cadr instruccion) memoria)
        (lambda (valor) (cons valor salida))
      )
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
(defun operacion_de* (eqop) 
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
(defun ejec* (prg ent mem &optional (sal nil))
  (cond
    ( (null prg) (reverse sal) )
    (
      (es_funcion_conocida (car prg))
      (if_valido_lambda 
        (modificar_flujo* (car prg) (cdr prg) ent mem sal) 
        (modificar_memoria* (car prg) ent mem sal)
        (modificar_salida*  (car prg) ent mem sal)
        (lambda (programa_nuevo memoria_nueva salida_nueva)
          (ejec*
            programa_nuevo
            (modificar_entrada (car prg) ent mem sal)
            memoria_nueva
            salida_nueva
          )
        )
      )
    )
    (
      T
      (exc (list "Esa funcion no es conocida" (car prg)))
    )
  )
)

(defun reasignar* (k v memoria)
  (if_valido_lambda (buscar_en_memoria* k memoria)
    (lambda (valor_inicial)
      (asignar* k v memoria)
    )
  )
)
(defun asignar* (k v memoria)
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
      (if_valido_lambda (asignar* k v (cddr memoria))
        (lambda (m)
          (append (list (car memoria) (cadr memoria)) m)
        )
      )
    )
  )
)
; ---- ejecuta  instrucciones de tipo INT ------ ;
(defun asignar_declaracion_variables* (instruccion memoria)
  (cond
    (
      (eq (car instruccion) 'int) 
      (asignar_declaracion_variables* (cdr instruccion) memoria)
    )
    ((null instruccion) memoria)
    (
      (and 
	      (symbolp (car instruccion))
	      (eq (cadr instruccion) '=)
	      (not (symbolp (caddr instruccion)))
      )
      (if_valido_lambda 
        (asignar* 
          (car instruccion) 
          (caddr instruccion) 
          memoria
        )
        (lambda (mem)
          (asignar_declaracion_variables*
            (cdddr instruccion)
            mem
          )
        )
      )
    )
    (
      (and 
	      (symbolp (car instruccion)) 
	      (symbolp (cadr instruccion)) 
      )
      (if_valido_lambda (asignar* (car instruccion) 0 memoria)
        (lambda (mem)
          (asignar_declaracion_variables*
            (cdr instruccion)
            mem
          )
        )
      )
    )
    (
      T
      (exc "No es una asignación de variable")
    )
  )
)
; -------------------------------- PRINCIPAL --------------------- ;
(defun run* (prg ent &optional (mem nil))
  (cond
    ((null prg) (exc "no hay programa"))
    (
      (eq (caar prg) 'int)
      (if_valido_lambda (asignar_declaracion_variables* (car prg) mem)
        (lambda (mem_inicial)
          (run*
            (cdr prg) 
            ent 
            mem_inicial
          )
        )
      )
    )
    (
      (eq (caar prg) 'main)
      (ejec* (cadar prg) ent mem)
    )
    (
      T
      (exc "El programa no empieza con main ni int")
    )
  )
)

(defun run (prg ent &optional (mem nil))
  (run* prg ent mem)
)
