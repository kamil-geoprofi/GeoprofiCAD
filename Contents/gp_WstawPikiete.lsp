(vl-load-com)  
(load "gp_Core.lsp" "\nBLAD: Nie znaleziono pliku gp_Core.lsp!")  
  
(defun c:WSTAW_PIKIETE ( / doc space pt z-val pt-3d) 
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))) 
  (setq space (vla-get-ModelSpace doc)) 
   
  (princ "\n==============================================") 
  (princ "\nTRYB WSTAWIANIA PIKIET (Numeracja automatyczna)") 
  (princ "\n(Aby zmienic przedrostek, wpisz: GEO_SETUP)") 
  (princ "\n==============================================") 
   
  (while (setq pt (getpoint "\nKliknij punkt wstawienia pikiety (lub wcisnij ENTER aby zakonczyc): ")) 
    (setq z-val (getreal "\nPodaj rzedna (Z) <0.00>: ")) 
    (if (not z-val) (setq z-val 0.0)) 
     
    (setq pt-3d (list (car pt) (cadr pt) z-val)) 
     
    ;; Wysyłamy pusty string ("") - Fabryka wygeneruje kolejny wolny numer
    (geocad-wstaw-pikiete-full doc space pt-3d "" T) 
  ) 
  (princ "\nZakonczono wstawianie pikiet.") (princ) 
) 
 
(princ "\nKomenda wczytana: WSTAW_PIKIETE")  
(princ)