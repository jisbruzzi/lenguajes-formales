;ejercicio de fp: < <alumno materia nota> <alumno materia nota> ...... >
;un listado de alumnos con la cantidad de materias que desaprobó cada uno

(defun desaprobado (evaluacion)
  (<= (caddr evaluacion) 4)
)


(defun agregar_uno_a_alumno (alumno resumen)
  (mapcar 
    (lambda (r) 
      (if (eq (car r) alumno)
        (list (car r) (+ (cadr r) 1))
        r
      )
    )
    resumen
  )
)

(defun alumno_en_resumen (alumno resumen)
  (reduce 
    (lambda (x y) (or x y))
    (mapcar
      (lambda (r) (eq (car r) alumno))
      resumen
    )
  )
)

(defun agregar_a_desaprobados (evaluacion resumen) 
  (cond
    (
      (null resumen)
      (list (list (car evaluacion) 1))

    )
    (
      (alumno_en_resumen (car evaluacion) resumen)
      (agregar_uno_a_alumno (car evaluacion) resumen)
    )
    (
      T
      (cons (list (car evaluacion) 1) resumen)
    )
  )
)

(defun desaprobados (L)
  (cond
    (
      (null L) 
      nil
    )
    (
      (desaprobado (car L))
      (agregar_a_desaprobados (car L) (desaprobados (cdr L)))
    )
    (
      T
      (desaprobados (cdr L))
    )
  )
)

(assert (equal (agregar_uno_a_alumno 'a '((a 2) (b 5)) ) '((a 3) (b 5)) ))
(assert (equal (agregar_uno_a_alumno 'b '((a 2) (b 5)) ) '((a 2) (b 6)) ))
(assert (equal (agregar_uno_a_alumno 'c '((a 2) (b 5)) ) '((a 2) (b 5)) ))

(assert (alumno_en_resumen 'a '((a 2) (b 5)) ))
(assert (alumno_en_resumen 'b '((a 2) (b 5)) ))
(assert (not (alumno_en_resumen 'c '((a 2) (b 5)) )))

(print (desaprobados 
  '( 
    (x m 1)
    (x m 1)
    (j m 1)
    (x m 1)
    (j m 1)
    (j m 8)
  ) 
))