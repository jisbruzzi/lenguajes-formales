
(defun pertenece (v conjunto)
  (cond
    ((null conjunto) nil)
    ((eq (car conjunto) v) T)
    (T (pertenece v (cdr conjunto)))
  )
)
(defun peso (op)
  (cond
  ((pertenece op '(< > <= >= ==)) 1)
    ((pertenece op '(+ -)) 2)
    ((pertenece op '(* / %)) 3)
    ((pertenece op '(&& || ^^)) 4)
  )
)

(defun es_operador (a)
  (pertenece a '(+ - * / % < > <= >= && || ^^ ==))
)

(defun nombre_operacion (a)
  (cond
    ( (eq a '+) 'sumar) 
    ( (eq a '-) 'restar) 
    ( (eq a '*) 'multiplicar) 
    ( (eq a '/) 'dividir) 
    ( (eq a '%) 'modulo)
    ( (eq a '<) 'menor)
    ( (eq a '>) 'mayor)
    ( (eq a '<=) 'menor_o_igual)
    ( (eq a '>=) 'mayor_o_igual)
    ( (eq a '&&) 'and)
    ( (eq a '||) 'or)
    ( (eq a '^^) 'xor)
    ( (eq a '==) 'igual)
  )
)

(defun es_asignacion (instruccion) (and (listp instruccion) (eq (cadr instruccion) '=)))
(defun es_asignacion_operacion (instruccion) 
  (and (listp instruccion) (pertenece (cadr instruccion) '(+= -= *= /= %= ++ --)))
)

(defun es_asignacion_operacion (instruccion) 
  (and (listp instruccion) (pertenece (cadr instruccion) '(+= -= *= /= %= ++ --)))
)

(defun es_postfijo (eqop)
  (or (eq '++ eqop) (eq '-- eqop))
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

(defun como_asignacion (expresion)
  (append
    (list
      (car expresion)
      '=
      (car expresion)
      (operacion_de* (cadr expresion))
      
    )
    (if (es_postfijo (cadr expresion))
      (list 1)
      (cddr expresion)
    )
  )
)

(defun lispizar* (expresion)
  (cond
    (
      (or (numberp expresion) (stringp expresion) )
      (list 'literal expresion)
    )
    (
      (symbolp expresion)
      (list 'variable expresion)
    )
    (
      (listp expresion)
      (cond
        (
          (es_asignacion expresion);x = ....
          (list 'asignar (car expresion) (lispizar* (cddr expresion)))
        )
        (
          (es_asignacion_operacion expresion);x += ....
          (list 'asignar (car expresion) (lispizar* (cddr (como_asignacion expresion))))
        )
        (
          (= (length expresion) 1)
          (lispizar* (car expresion))
        )
        (
          T
          (lispizar_algebraica* expresion nil nil)
        )
      )
    )
    (
      T
      (list 'nada)
    )
  )
)


(defun lispizar_algebraica* (expresion operadores operandos)
  (funcall (lambda  (operar_luego operar_ahora agregar_expresion_como_operando)
    (cond
      (
        (and (null expresion) (null operadores))
        (car operandos)
      )
      (
        (and (null expresion) (not (null operadores)))
        (funcall operar_ahora)
      )
      (
        (and (es_operador (car expresion)) (null operadores))
        (funcall operar_luego)
      )
      (
        (and (es_operador (car expresion)) (not (null operadores)) )
        (if (< (peso (car operadores)) (peso (car expresion)) )
          (funcall operar_luego)
          (funcall operar_ahora)
        )
      )
      (
        (or (numberp (car expresion)) (symbolp (car expresion)) (listp (car expresion)))
        (funcall agregar_expresion_como_operando)
      )
    )
  )

  (lambda ();operar_luego
    (lispizar_algebraica*
      (cdr expresion)
      (cons (car expresion) operadores) 
      operandos
    )
  )
  (lambda ();operar_ahora
    (funcall (lambda (izquierda derecha operador otros_operadores otros_operandos)
        (
          lispizar_algebraica* expresion otros_operadores (cons 
          (list
            (nombre_operacion operador)
            izquierda
            derecha
          ) 
          otros_operandos)
        )
        
      )
      (cadr operandos);izquierda
      (car operandos);derecha
      (car operadores);operador
      (cdr operadores);otros_operadores
      (cddr operandos);otros_operandos
    )
  )
  (lambda ();agregar_expresion_como_operando
    (lispizar_algebraica*
      (cdr expresion)
      operadores
      (cons (lispizar* (car expresion)) operandos)
    )
  )
  
  )
)