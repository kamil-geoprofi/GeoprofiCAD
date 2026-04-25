(vl-load-com) 
(load "gp_Core.lsp" "\nBLAD: Nie znaleziono pliku gp_Core.lsp!") 

;; --- UNIWERSALNY RADAR OBIEKTÓW (Bez niego ani rusz!) ---
(defun get-pt-from-obj (obj / type pt-list val)  
  (setq type (vla-get-ObjectName obj))  
  (cond 
    ((= type "AcDbPoint") 
     (setq pt-list (vlax-safearray->list (vlax-variant-value (vla-get-Coordinates obj))))
    )
    ((member type '("AcDbBlockReference" "AcDbText" "AcDbMText"))
     (setq pt-list (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint obj))))
     (if (= type "AcDbBlockReference")  
       (foreach att (vlax-invoke obj 'GetAttributes)  
         (if (member (strcase (vla-get-TagString att)) '("H" "Z" "RZEDNA"))
           (progn
             (setq val (vla-get-TextString att))
             (if (and val (/= val "") (/= val "---") (distof (vl-string-translate "," "." val)))
               (setq pt-list (list (car pt-list) (cadr pt-list) (atof (vl-string-translate "," "." val))))
             )
           )
         )
       )
     )
    )
  )
  pt-list 
) 

;; ========================================== 
;; GŁÓWNY SKRYPT: ŁAŃCUCH SPADKÓW
;; ========================================== 
(defun c:SPADKIPRO ( / old-err doc space ent obj pt1 z1 nr-start nr-str default_slope pt2 slope dist deltaZ z2 newPt) 
  
  (setq old-err *error* *error* (lambda (msg) (princ (strcat "\nPrzerwano: " msg)) (princ)))
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)) space (vla-get-ModelSpace doc))

  ;; 1. Wybór bazy za pomocą inteligentnego radaru
  (setq ent (car (entsel "\nKliknij obiekt bazowy (Blok, Punkt lub Tekst z rzedna): "))) 
  (if (not ent) (exit))
  (setq obj (vlax-ename->vla-object ent))
  
  (setq pt1 (get-pt-from-obj obj))
  (if pt1 (setq z1 (caddr pt1)))

  ;; Jeśli obiekt ma Z=0 (np. płaska mapa), pytamy o rzędną ręcznie
  (if (or (not z1) (= z1 0.0))
    (setq z1 (getreal "\nBrak lub zerowa rzedna w obiekcie. Podaj rzedna startowa recznie [m]: "))
  )
  (if (not z1) (exit))
  (setq pt1 (list (car pt1) (cadr pt1) z1))

  ;; 2. Opcjonalna numeracja
  (setq nr-start (getint "\nPodaj poczatkowy numer dla wstawianych pikiet (lub ENTER aby pominac numery): "))

  ;; 3. Główny łańcuch projektowy
  (if z1 
    (progn 
      (princ (strcat "\n-> ROZPOCZYNAMY. Startowa rzedna: Z = " (rtos z1 2 3))) 
      (princ "\n(Ustawienia wizualne pikiet zostana pobrane z GEO_SETUP)")
      (setq default_slope -2.0) 
        
      (while (setq pt2 (getpoint pt1 "\nWskaz miejsce na NOWA pikiete [ENTER = Koniec]: ")) 
        (setq slope (getreal (strcat "\nPodaj spadek w % <" (rtos default_slope 2 2) ">: "))) 
        (if (not slope) (setq slope default_slope)) 
        (setq default_slope slope) 
          
        ;; Obliczenia geodezyjne 
        (setq dist (distance (list (car pt1) (cadr pt1)) (list (car pt2) (cadr pt2)))) 
        (setq deltaZ (* dist (/ slope 100.0))) 
        (setq z2 (+ z1 deltaZ)) 
        (setq newPt (list (car pt2) (cadr pt2) z2)) 
          
        ;; Zarządzanie numeracją w locie
        (if nr-start 
          (progn (setq nr-str (itoa nr-start)) (setq nr-start (1+ nr-start)))
          (setq nr-str "")
        )

        ;; ODDELEGOWANIE DO FABRYKI! Pikieta rysuje się automatycznie.
        (geocad-wstaw-pikiete-full doc space newPt nr-str T) 
          
        (princ (strcat "\n-> WSTAWIONO: Z=" (rtos z1 2 3) " + " (rtos slope 2 2) "% (" (rtos dist 2 2) "m) -> NOWA Z=" (rtos z2 2 3))) 
          
        ;; Przeskok (Nowy punkt staje się bazą dla następnego kliknięcia)
        (setq z1 z2 pt1 newPt) 
      ) 
    ) 
  ) 
  (setq *error* old-err)
  (princ "\nKoniec lancucha spadkow.")  
  (princ) 
)
(princ "\nKomenda wczytana: SPADKIPRO") (princ)