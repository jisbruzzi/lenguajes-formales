(defun pert (a L)
  (cond 
    ((null L) nil)
    ((eq a (car L)) T)
    ((listp (car L)) (if (pert a (car L)) T (pert a (cdr L))))
    (T (pert a (cdr L)))
  )
)
(defun consr (a L)
  (append L (list a))
)
(defun iota (n)
  (cond
    ((eq n 0) nil)
    (T (append (iota (- n 1)) (list n) ))
  )
)

(defun B (lf p)
  (mapcar (lambda (f) (funcall f  p)) lf)
)

(defun quitar_primero (L)
  (cond
    ((null L) L)
    ((atom L) L)
    ((listp L) (mapcar #'quitar_primero (cdr L)))
   )
)
