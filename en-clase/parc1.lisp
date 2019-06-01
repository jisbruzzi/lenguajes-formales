;eliminar las ocurrencias de un átomo en una lista a todo nivel

(defun filtrar (f L)
  (reduce
    (lambda (x y) 
      (if (funcall f y)
        (append x (list y))
        x
      )
    )
    (cons nil L)
  )
)

(assert (equal (filtrar (lambda (x) (eq x 1) ) '(1 2 3 4 5 1 1 1))  '(1 1 1 1)  ))

(defun eliminar_a_todo_nivel (a X)
  (cond
    (
      (listp X)
      (mapcar 
        (lambda (x) (eliminar_a_todo_nivel a x))
        (filtrar (lambda (x) (not (eq a x)) ) X)
      )
    )
    (
      (atom X) X
    )
  )

)
'(defun eliminar_a_todo_nivel (a L)
  (cond
    (
      (null L) ;condicion
      nil
    )
    (
      (and 
        (atom (car L)) 
        (eq a (car L))
      );condicion
      (eliminar_a_todo_nivel a (cdr L))
    )
    (
      (listp (car L));condicion
      (cons
        (eliminar_a_todo_nivel a (car L))
        (eliminar_a_todo_nivel a  (cdr L))
      )
    )
    (
      (and 
        (atom (car L))
        (not (eq a (car L)))
      );condicion
      (cons (car L) (eliminar_a_todo_nivel a (cdr L)))
    )
    
  )
)
(assert (equal (eliminar_a_todo_nivel 1 '(1 2 3 4 5)) '(2 3 4 5) ))
(assert (equal (eliminar_a_todo_nivel 1 '( (1 2 3 4 5) 2 3 4 5)) '( (2 3 4 5) 2 3 4 5)) )
(assert (equal (eliminar_a_todo_nivel 1 '( (1 2 3 4 5) (2 3 4 1) 3 4 5)) 
  '( (2 3 4 5) (2 3 4) 3 4 5)
))

(assert (equal (eliminar_a_todo_nivel 1 '( (1 2 3 4 5) (2 3 1 1 1 4 1) 3 4 1 1 1 1 () 5 1 (1 1 1) )) 
  '( (2 3 4 5) (2 3 4) 3 4 () 5 () )
))