(vl-load-com) 
(load "gp_Core.lsp" "\nBLAD: Nie znaleziono pliku gp_Core.lsp!") 
 
(defun c:WSTAW_PIKIETE ( / doc space txt-h z-prec prefix kolor styl display nr-start pt z-val pt-3d)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq space (vla-get-ModelSpace doc))
  
  ;; --- Odczytanie informacji dla użytkownika ---
  (setq txt-h (geocad-get-cfg "TxtH" "1.0")
        z-prec (geocad-get-cfg "Prec" "2")
        prefix (geocad-get-cfg "Prefix" "POMIAR")
        kolor (geocad-get-cfg "Color" "3")
        styl (geocad-get-cfg "Styl" "Blok")
        display (geocad-get-cfg "Display" "Oba"))

  (princ "\n==============================================")
  (princ (strcat "\nAKTYWNY STANDARD: [" styl "] Widocznosc: [" display "]"))
  (princ (strcat "\nWys: " txt-h " | Prec: " z-prec " | Warstwa: " prefix " | Kolor: " kolor))
  (princ "\n(Aby zmienic standard, wcisnij ESC i wpisz: GEO_SETUP)")
  (princ "\n==============================================")

  ;; --- Pytamy o numerację tylko raz ---
  (setq nr-start (getint "\nPodaj numer poczatkowy pikiety <1>: ")) 
  (if (not nr-start) (setq nr-start 1))
  
  ;; --- Główna pętla wstawiania (Brak zbędnych pytań!) ---
  (while (setq pt (getpoint (strcat "\nKliknij punkt wstawienia pikiety nr " (itoa nr-start) " (lub wcisnij ENTER aby zakonczyc): ")))
    (setq z-val (getreal "\nPodaj rzedna (Z) <0.00>: "))
    (if (not z-val) (setq z-val 0.0))
    
    (setq pt-3d (list (car pt) (cadr pt) z-val))
    
    ;; Oddelegowanie całej brudnej roboty rysunkowej do Biblioteki.
    ;; Podajemy "T" (True) dla show-z, bo i tak "Mózg" sam decyduje czy ukryć Z w zależności od ustawień z GEO_SETUP!
    (geocad-wstaw-pikiete-full doc space pt-3d (itoa nr-start) T)
    
    (setq nr-start (1+ nr-start))
  )
  (princ "\nZakonczono wstawianie pikiet.") (princ)
)

(princ "\nKomenda wczytana: WSTAW_PIKIETE") 
(princ)