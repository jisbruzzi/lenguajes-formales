(defmacro l (nombre form)
  `(list (quote ,nombre) (lambda (expr evfn) ,form))
)
(defmacro ev (form)
  `(funcall evfn ,form)
)
(defmacro cf (nombre)
  `(list 
    (quote ,nombre)
    (lambda (argumentos evfn)
      (apply ',nombre argumentos)
    )
  )
)