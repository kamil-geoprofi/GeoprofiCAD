(vl-load-com)  
(load "gp_Core.lsp" "\nBLAD: Nie znaleziono pliku gp_Core.lsp!")  

(defun c:SPADKIPRO ( / old-err doc space ent obj pt1 z1 default_slope pt2 slope dist deltaZ z2 newPt)  
  (setq old-err *error* *error* (lambda (msg) (princ (strcat "\nPrzerwano: " msg)) (princ))) 
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)) space (vla-get-ModelSpace doc)) 
 
  (setq ent (car (entsel "\nKliknij obiekt bazowy (Blok, Punkt lub Tekst z rzedna): ")))  
  (if (not ent) (exit)) 
  (setq obj (vlax-ename->vla-object ent)) 
   
  (setq pt1 (get-pt-from-obj obj)) 
  (if pt1 (setq z1 (caddr pt1))) 
 
  (if (or (not z1) (= z1 0.0)) 
    (setq z1 (getreal "\nBrak rzednej. Podaj rzedna startowa recznie [m]: ")) 
  ) 
  (if (not z1) (exit)) 
  (setq pt1 (list (car pt1) (cadr pt1) z1)) 
 
  (if z1  
    (progn  
      (princ (strcat "\n-> ROZPOCZYNAMY. Startowa rzedna: Z = " (rtos z1 2 3)))  
      (setq default_slope -2.0)  
          
      (while (setq pt2 (getpoint pt1 "\nWskaz miejsce na NOWA pikiete [ENTER = Koniec]: "))  
        (setq slope (getreal (strcat "\nPodaj spadek w % <" (rtos default_slope 2 2) ">: ")))  
        (if (not slope) (setq slope default_slope))  
        (setq default_slope slope)  
            
        (setq dist (distance (list (car pt1) (cadr pt1)) (list (car pt2) (cadr pt2))))  
        (setq deltaZ (* dist (/ slope 100.0)))  
        (setq z2 (+ z1 deltaZ))  
        (setq newPt (list (car pt2) (cadr pt2) z2))  

        ;; Wstawianie bez podawania numeru
        (geocad-wstaw-pikiete-full doc space newPt "" T)  
            
        (princ (strcat "\n-> WSTAWIONO: Z=" (rtos z1 2 3) " + " (rtos slope 2 2) "% (" (rtos dist 2 2) "m) -> NOWA Z=" (rtos z2 2 3)))  
        (setq z1 z2 pt1 newPt)  
      )  
    )  
  )  
  (setq *error* old-err) 
  (princ "\nKoniec lancucha spadkow.")   
  (princ)  
) 
(princ "\nKomenda wczytana: SPADKIPRO") (princ)