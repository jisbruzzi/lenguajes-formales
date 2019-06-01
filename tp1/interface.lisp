(load 'mapa)
(load 'algoritmo)

(defun calle_valida (calle)
  (pertenece (print calle )
    (reduce
      'append
      (mapcar 
        (lambda (x) (cadr x))
        diccionario
      )
    )
  )
)

(defun obtenerCalleValida ()
  (let
    (
      (i  (print (list 'indique 'una 'calle 'camelCase)))
      (intentoDeCalle (read))
      
    )
    (if (calle_valida intentoDeCalle) 
        ((lambda () (print (list intentoDeCalle 'es 'una 'calle 'valida)) intentoDeCalle))
        ((lambda () (print (list intentoDeCalle 'es 'invalida)) (obtenerCalleValida)))
      )
  )
)

(defun misma_esquina (e1 e2)
  (and
    (eq (car e1) (car e2))
    (eq (cadr e1) (cadr e2))
  )
)

(defun vertice_de_esquina (esq &optional (dic diccionario))
  (cond 
    ( (null dic) nil)
    ( (misma_esquina esq (cadar dic)) (caar dic))
    (T (vertice_de_esquina esq (cdr dic)))
  )
)
(defun esquina_valida (esq)
  (not (null (vertice_de_esquina esq)))
)


(defun obtenerEsquina ()
  (let
    (
      (calle1 (obtenerCalleValida))
      (calle2 (obtenerCalleValida))
    )
    (if (esquina_valida (list calle1 calle2))
      ((lambda () (print (list 'la 'esquina calle1 calle2 'es 'valida )) (list calle1 calle2) ))
      ((lambda () (print (list 'la 'esquina calle1 calle2 'no 'existe )) (obtenerEsquina) ))
    )
    
  )
)
(defun interactuar () (let
  (
    (bienvenida (print (list 'bienvenido. 'indique 'esquina 'de 'partida)))
    (e1 (obtenerEsquina))
    (bienvenida (print (list 'indique 'esquina 'de 'finalizacion)))
    (e2 (obtenerEsquina))
  )
  ((lambda () 
    (print (list 'Voy 'a 'buscar 'el 'camino 'desde e1 'a e2))
    (print (list 'En 'mi 'grafo 'implica 'de (vertice_de_esquina e1) 'a (vertice_de_esquina e2) ))
    (print (GPS (vertice_de_esquina e1) (vertice_de_esquina e2) grafo))
  ))
  
))

(interactuar)