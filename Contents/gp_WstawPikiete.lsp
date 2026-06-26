(vl-load-com)  
(load "gp_Core.lsp" "\nBLAD: Nie znaleziono pliku gp_Core.lsp!")  
  
(defun c:WSTAW_PIKIETE ( / old-err doc space pt z-val pt-3d batch)  
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))) 
  (setq space (vla-get-ModelSpace doc))

  (setq old-err *error*
      *error*
      (lambda (msg)
        (if batch
          (progn
            (setq batch (geocad-pikieta-batch-end batch))
            (setq batch nil)
          )
        )

        (setq *error* old-err)

        (if (not (member msg '("Function cancelled" "quit / exit abort")))
          (princ (strcat "\nPrzerwano: " msg))
          (princ "\nPrzerwano.")
        )

        (princ)
      )
  )
   
  (princ "\n==============================================") 
  (princ "\nTRYB WSTAWIANIA PIKIET (Numeracja automatyczna)") 
  (princ "\n(Aby zmienic przedrostek, wpisz: GEO_SETUP)") 
  (princ "\n==============================================") 
   
  (while (setq pt (getpoint "\nKliknij punkt wstawienia pikiety (lub wcisnij ENTER aby zakonczyc): ")) 
    (setq z-val (getreal "\nPodaj rzedna (Z) <0.00>: ")) 
    (if (not z-val) (setq z-val 0.0)) 
     
    (setq pt-3d (list (car pt) (cadr pt) z-val)) 
     
    ;; Numer automatyczny obsluguje batch.
;; Batch startuje leniwie przy pierwszej faktycznie wstawianej pikiecie.
(if (not batch)
  (setq batch (geocad-pikieta-batch-start doc))
)

(setq batch
  (geocad-pikieta-batch-insert
    batch
    space
    pt-3d
    nil
    T
  )
) 
  ) 
  (if batch
  (progn
    (setq batch (geocad-pikieta-batch-end batch))
    (setq batch nil)
  )
)

(setq *error* old-err)

(princ "\nZakonczono wstawianie pikiet.")
(princ)
) 
 
(princ "\nKomenda wczytana: WSTAW_PIKIETE")  
(princ)