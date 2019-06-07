; ---------- PRIMITIVAS DE DICCIONARIO ----- ;
(defun en_diccionario (diccionario clave)
  (not (null (buscar_diccionario diccionario clave)))
)
(defun buscar_diccionario (diccionario clave)
  (cond
    ((null diccionario) nil)
    ((eq (caar diccionario) clave) (cadar diccionario))
    (T (buscar_diccionario (cdr diccionario) clave))
  )
)

; ---------- DICCIONARIO DE FUNCIONES CONOCIDAS QUE CONTROLAN EL FLUJO DEL PROGRAMA ----- ;
(defmacro l (nombre form)
  `(list (quote ,nombre) (lambda (expr evfn) ,form))
)
(defmacro ev (form)
  `(funcall evfn ,form)
)

(setq control_flujo
  (list
    (l quote (cadr expr))
    (l and 
      (if (null (ev (cadr expr)))
        nil
        (ev (caddr expr))
      )
    )
    (l or
      (if (null (ev (cadr expr)))
        (ev (caddr expr))
        (ev (cadr expr))
      )
    )
    (l if
      (if (not (null (ev (cadr expr)) ))
        (ev (caddr expr))
        (ev (cadddr expr))
      )
    )
  )
)

(defun es_control_flujo (nombre)
    (en_diccionario control_flujo nombre)
)

(defun funcion_control_flujo (nombre)
    (buscar_diccionario control_flujo nombre)
)

; --------- DICCIONARIO DE FUNCIONES CONOCIDAS --------- ;
(defmacro cf (nombre)
  `(list 
    (quote ,nombre)
    (lambda (argumentos evfn)
      (apply ',nombre argumentos)
    )
  )
)

(setq conocidas
  (list
    (cf car)
    (cf cdr)
    (cf list)
    (cf *)
    (cf +)
    (cf -)
    (cf cons)
    (cf numberp)
    (cf eq)
    (cf null)
    (list 
      (quote mapcar)
      (lambda (args evfn)
        (apply 
          'mapcar 
          (cons
            (lambda (&rest elementos)
              (funcall evfn
                (cons 
                  (car args) 
                  (mapcar (lambda (e) (list `quote e)) elementos)
                )
              )
            )
            (cdr args)
          )
        )
      )
    )
  )
)

(defun es_conocida (nombre)
  (en_diccionario conocidas nombre)
)

(defun funcion_conocida (nombre)
  (buscar_diccionario conocidas nombre)
)

; ------ DEFINICIÓN DE LA FUNCIÓN EVALUAR ---- ;
(defun pertenece (e l)
  (reduce 
    (lambda (x y) (or x y))
    (mapcar 
      (lambda (x) (eq e x))
      l
    )
  )
)

(defun buscar (nombre ambiente)
  (cond
    ((null  nombre) nil)
    ((eq 'T nombre) T)
    ((numberp nombre) nombre)
    ((null ambiente) (exit (print "INTERRUPCION: NOMBRE NO ESTÁ EN EL AMBIENTE")))
    ((eq (car ambiente) nombre) (cadr ambiente))
    (T (buscar nombre (cddr ambiente)))
  )
)

(defun ampliar_ambiente (nombres valores ambiente)
  (cond
    (
      (null ambiente) 
      (reduce 'append (mapcar (lambda (x y) (list x y)) nombres valores))
    )
    (
      (pertenece (car ambiente) nombres)
      (ampliar_ambiente nombres valores (cddr ambiente))
    )
    (
      T
      (append
        (list (car ambiente) (cadr ambiente))
        (ampliar_ambiente nombres valores (cddr ambiente))
      )
    )
  )
)

(defun aplicar (fn argumentos ambiente)
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
      (aplicar 
          (buscar fn  ambiente) 
          argumentos 
          ambiente
      )
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
