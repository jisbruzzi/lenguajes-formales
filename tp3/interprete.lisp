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

(defun peso (op)
  (cond
  ((pertenece op '(< > <= >= ==)) 1)
    ((pertenece op '(+ -)) 2)
    ((pertenece op '(* / %)) 3)
    ((pertenece op '(&& || ^^)) 4)
  )

)

(defun valor* (expresion memoria constantes &optional (operadores nil) (operandos nil))
  ;(print 'vvvvvvvvvvvvvv)
  ;(print expresion)
  ;(print memoria)
  ;(print constantes)
  ;(print operadores)
  ;(print operandos)
  (cond
    (
      (and (atom expresion) (not (null expresion)))
      (cond 
        ((numberp expresion) expresion)
        ((stringp expresion) expresion)
        (
          (es_valido (buscar_en_memoria* expresion memoria))
          (buscar_en_memoria* expresion memoria)
        )
        (
          (es_valido (buscar_en_memoria* expresion constantes))
          (buscar_en_memoria* expresion constantes)
        )
        (
          T (exc (list "No se conoce el valor de" expresion))
        )
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
            constantes
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
        constantes
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
          constantes
          (cons (car expresion) operadores) 
          operandos
        )
        (valor*
          expresion
          memoria
          constantes
          (cdr operadores)
          (cons 
            (operar* (car operadores) (cadr operandos) (car operandos))
            (cddr operandos)
          )
        )
      )
    )
    (
      (or (numberp (car expresion)) (symbolp (car expresion)))
      (if_valido_lambda 
        (valor* (car expresion) memoria constantes)
        (lambda (v) 
          (valor*
            (cdr expresion)
            memoria
            constantes
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

(defun mapcar* (f L)
  (if (null L) nil
    (if_valido_lambda (funcall f (car L))
      (lambda (cabeza)
        (if_valido_lambda (mapcar* f (cdr L))
          (lambda (cola)
            (cons cabeza cola)
          )
        )
      )
    )
  )
)
(defun valores* (expresiones memoria constantes)
  (mapcar* (lambda (e) (valor* e memoria constantes)) expresiones)
)

(defun hay_algun_1 (valores)
  (if (null valores) nil
    (if (or (eq (car valores) 1)(eq (car valores) T))
      T
      (hay_algun_1 (cdr valores))
    )
  )
)

(defun range (n)
  (if (= n 0)
    (list 0)
    (reverse (cons n (reverse (range (- n 1)))))
  )
)

(defun filtrar (f L)
  (if (null L) nil
    (if (funcall f (car L))
      (cons (car L) (filtrar f (cdr L)))
      (filtrar f (cdr L))
    )
  )
)

(setf *random-state* (make-random-state t))

(defun cualquiera (L)
  (nth (random (length L)) L)
)

(defun indice_cualquiera_1 (valores)
  (cualquiera (mapcar
    (lambda (v) (cadr v))
    (filtrar 
      (lambda (v) (or (eq (car v) T) (eq (car v) 1)))
      (mapcar 'list  valores (range (- (length valores) 1)))
    )
  ))
)

(defun modificar_flujo* (instruccion cola_programa entrada memoria salida constantes)
  (cond
    (
      (es_if_deterministico  instruccion)
      (if_valido_lambda 
        (valor* (cadr instruccion) memoria constantes)
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
      (es_if_no_deterministico instruccion)
      (if_valido_lambda
        (valores* (cadr instruccion) memoria constantes)
        (lambda (valores)
          (if (hay_algun_1 valores)
            (append
              (nth 
                (indice_cualquiera_1 valores) 
                (caddr instruccion)
              ) 
              cola_programa
            )
            cola_programa
          )
        )
      )
    )
    (
      (es_while instruccion)
      
      (if_valido_lambda 
        (valor* (cadr instruccion) memoria constantes)
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
      (es_while_no_deterministico instruccion)

      (if_valido_lambda
        (valores* (cadr instruccion) memoria constantes)
        (lambda (valores)
          (if (hay_algun_1 valores)
            (append
              (nth 
                (indice_cualquiera_1 valores) 
                (caddr instruccion)
              ) 
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

(defun modificar_memoria* (instruccion entrada memoria salida constantes)
  (cond
    (
      (es_scanf instruccion)
      (reasignar* (cadr instruccion) (car entrada)  constantes memoria)
    )
    (
      (es_asignacion instruccion)
      (if_valido_lambda 
        (valor* (cddr instruccion) memoria constantes)
        (lambda (valor)
          (reasignar* (car instruccion) valor constantes memoria)  
        )
      )
    )
    (
      (es_asignacion_operacion instruccion)
      (if_valido_lambda 
        (valor* (cddr instruccion) memoria constantes)
        (operacion_de* (cadr instruccion))
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
            constantes
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
(defun modificar_salida* (instruccion entrada memoria salida constantes) 
  (cond
    (
      (es_printf instruccion)
      (if_valido_lambda 
        (valor* (cadr instruccion) memoria constantes)
        (lambda (valor) (cons valor salida))
      )
    )
    (T salida)
  )
)
(defun modificar_entrada (instruccion entrada memoria salida constantes) 
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

(defun es_lista_de_listas (l) (todos 'listp l))



(defun es_no_deterministico (instruccion)
  (and
    (todos 'listp (cadr instruccion))
    (todos 'es_lista_de_listas (caddr instruccion))
    (=
      (length (cadr instruccion))
      (length (caddr instruccion))
    )
  )
)

(defun es_if_deterministico (instruccion) 
  (and 
    (eq (car instruccion) 'if)
    (not (es_no_deterministico instruccion))
  )
)

(defun es_if_no_deterministico (instruccion) 
  (and 
    (eq (car instruccion) 'if)
    (es_no_deterministico instruccion)
  )
)
(defun es_while_no_deterministico (instruccion)
  (and 
    (eq (car instruccion) 'while)
    (es_no_deterministico instruccion)
  )
)
(defun es_if_con_else (instruccion) 
  (eq (car (cdddr instruccion)) 'else)
)
(defun es_while (instruccion) 
  (and
    (eq (car instruccion) 'while)
    (not (es_no_deterministico instruccion))
  )
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
    (es_if_deterministico instruccion)
    (es_if_no_deterministico instruccion)
    (es_while instruccion)
    (es_while_no_deterministico instruccion)
  )
)
(defun ejec* (prg ent mem const &optional (sal nil))
  (cond
    ( (null prg) (reverse sal) )
    (
      (es_funcion_conocida (car prg))
      (if_valido_lambda 
        (modificar_flujo* (car prg) (cdr prg) ent mem sal const)
        (modificar_memoria* (car prg) ent mem sal const)
        (modificar_salida*  (car prg) ent mem sal const)
        (lambda (programa_nuevo memoria_nueva salida_nueva)
          (ejec*
            programa_nuevo
            (modificar_entrada (car prg) ent mem sal const)
            memoria_nueva
            const
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

(defun reasignar* (k v constantes memoria)
  (if (es_valido (buscar_en_memoria* k constantes))
    (exc (list k "corresponde a una constante, no se puede reasignar"))
    (if_valido_lambda (buscar_en_memoria* k memoria)
      (lambda (valor_inicial)
        (asignar* k v memoria)
      )
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
; ---- ejecuta  instrucciones de tipo define ------ ;
(defun asignar_declaracion_constantes* (instruccion constantes)
  (if (eq (car instruccion) 'define)
    (asignar* (cadr instruccion) (caddr instruccion) constantes)
    (exc "define mal construido")
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
(defun run* (prg ent &optional (mem nil) (const nil))
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
            const
          )
        )
      )
    )
    (
      (eq (caar prg) 'define)
      (if_valido_lambda 
        (asignar_declaracion_constantes* (car prg) const)
        (lambda (const_inicial)
          (run*
            (cdr prg)
            ent
            mem
            const_inicial
          )
        )
      )
    )
    (
      (eq (caar prg) 'main)
      (ejec* (cadar prg) ent mem const)
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
