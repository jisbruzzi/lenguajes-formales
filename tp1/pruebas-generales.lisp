(load 'pruebas)
(defun GPS_INTERFAZ_ENUNCIADO (inicio fin grafo)
  (ruta_para_mostrar
    (GPS
      (vertice_de_esquina inicio)
      (vertice_de_esquina fin)
      grafo
    )
  )
)

(test 
  (GPS_INTERFAZ_ENUNCIADO '(PaseoColon Independencia) '(Defensa Belgrano) grafo)
  '((RECORRER 3 CUADRAS POR
   PASEOCOLON Y DOBLAR EN
   VENEZUELA)
  (RECORRER 2 CUADRAS POR
   VENEZUELA Y DOBLAR EN
   DEFENSA)
  (RECORRER 1 CUADRAS POR
   DEFENSA HASTA LLEGAR A
   DESTINO))
)