(vl-load-com)   
(load "gp_Core.lsp" "\nBLAD: Nie znaleziono pliku gp_Core.lsp!")   
    
;; --- FUNKCJA POMOCNICZA: Strażnik Osi ---
;; Jeśli wyliczony punkt (L) mieści się na fizycznej linii, zwraca jego współrzędne.
;; Jeśli punkt wisi w powietrzu (poza linią), zwraca 'nil' (czyli zignoruj go).
(defun get-safe-curve-pt (crv L / total)
  (setq total (vlax-curve-getDistAtParam crv (vlax-curve-getEndParam crv)))
  (if (and (>= L -0.001) (<= L (+ total 0.001)))
    (vlax-curve-getPointAtDist crv L)
    nil 
  )
)

(defun c:NIWELACJA_2PKT ( / old-err old-osmode old-cmdecho crvEnt crvObj obj1 obj2 pt1 pt2 z1 z2 p1-proj p2-proj  
                          L1-base L2-base gap1 gap2 param1 param2 deriv1 deriv2 len1 len2  
                          U1x U1y U2x U2y v1x v1y v2x v2y dot1 dot2 long1 long2  
                          perp-sq1 perp-sq2 perp1 perp2 L1-virt L2-virt  
                          dL dZ slope pct-slope mode step num-pts steps-list L-cur  
                          doc space pt-cur z-cur pt-list max-offset ans1 ans2 zlicz
                          L-start L-end draw-3d poly-pts)   
      
  ;; --- OBSŁUGA BŁĘDÓW (Zabezpieczenie ustawień CAD) ---
  (setq old-osmode (getvar "OSMODE") old-cmdecho (getvar "CMDECHO"))
  (setq old-err *error* *error* (lambda (msg) 
                                  (if old-osmode (setvar "OSMODE" old-osmode))
                                  (if old-cmdecho (setvar "CMDECHO" old-cmdecho))
                                  (princ (strcat "\nPrzerwano: " msg)) 
                                  (princ)))   
    
  (setq max-offset 0.15) ;; 15 cm tolerancji od osi
    
  ;; --- 1. WYBÓR DANYCH ---
  (setq crvEnt (car (entsel "\n1. Wybierz os trasy (Linia/Polilinia/Luk): "))) (if (not crvEnt) (exit))   
  (setq crvObj (vlax-ename->vla-object crvEnt))   
    
  (setq obj1 (vlax-ename->vla-object (car (entsel "\n2. Wybierz PUNKT 1 (Bazowy): "))))   
  (setq pt1 (get-pt-from-obj obj1))   
  (if (not pt1) (progn (alert "Blad odczytu rzednej punktu 1!") (exit)))   
  (setq z1 (caddr pt1))   
  (if (or (not z1) (= z1 0.0)) (setq z1 (getreal "\nPodaj RZEDNA 1 recznie: ")))   
    
  (setq obj2 (vlax-ename->vla-object (car (entsel "\n3. Wybierz PUNKT 2 (Docelowy): "))))   
  (setq pt2 (get-pt-from-obj obj2))   
  (if (not pt2) (progn (alert "Blad odczytu rzednej punktu 2!") (exit)))   
  (setq z2 (caddr pt2))   
  (if (or (not z2) (= z2 0.0)) (setq z2 (getreal "\nPodaj RZEDNA 2 recznie: ")))   
  
  ;; --- 2. RZUTOWANIE I MATEMATYKA (Przedłużanie stycznych) ---
  (setq p1-proj (vlax-curve-getClosestPointTo crvObj pt1))   
  (setq L1-base (vlax-curve-getDistAtPoint crvObj p1-proj))   
  (setq gap1 (distance (list (car pt1) (cadr pt1)) (list (car p1-proj) (cadr p1-proj)))) 
  (if (> gap1 0.001) 
    (progn 
      (setq param1 (vlax-curve-getParamAtPoint crvObj p1-proj)) 
      (setq deriv1 (vlax-curve-getFirstDeriv crvObj param1)) 
      (setq len1 (distance '(0.0 0.0) (list (car deriv1) (cadr deriv1)))) 
      (setq U1x (/ (car deriv1) len1) U1y (/ (cadr deriv1) len1)) 
      (setq v1x (- (car pt1) (car p1-proj)) v1y (- (cadr pt1) (cadr p1-proj))) 
      (setq dot1 (+ (* v1x U1x) (* v1y U1y))) 
      (setq perp1 (sqrt (max 0.0 (- (* gap1 gap1) (* dot1 dot1))))) 
      (setq L1-virt (+ L1-base dot1))
    ) 
    (setq perp1 0.0 L1-virt L1-base) 
  ) 

  (setq p2-proj (vlax-curve-getClosestPointTo crvObj pt2))   
  (setq L2-base (vlax-curve-getDistAtPoint crvObj p2-proj))   
  (setq gap2 (distance (list (car pt2) (cadr pt2)) (list (car p2-proj) (cadr p2-proj)))) 
  (if (> gap2 0.001) 
    (progn 
      (setq param2 (vlax-curve-getParamAtPoint crvObj p2-proj)) 
      (setq deriv2 (vlax-curve-getFirstDeriv crvObj param2)) 
      (setq len2 (distance '(0.0 0.0) (list (car deriv2) (cadr deriv2)))) 
      (setq U2x (/ (car deriv2) len2) U2y (/ (cadr deriv2) len2)) 
      (setq v2x (- (car pt2) (car p2-proj)) v2y (- (cadr pt2) (cadr p2-proj))) 
      (setq dot2 (+ (* v2x U2x) (* v2y U2y)))  
      (setq perp2 (sqrt (max 0.0 (- (* gap2 gap2) (* dot2 dot2))))) 
      (setq L2-virt (+ L2-base dot2))
    ) 
    (setq perp2 0.0 L2-virt L2-base) 
  ) 

  ;; --- 3. KONTROLA OFFSETA ---
  (if (or (> perp1 max-offset) (> perp2 max-offset))
    (progn
      (initget "Tak Nie")
      (if (= "Nie" (getkword (strcat "\nJeden z punktow ucieka od osi (P1=" (rtos perp1 2 2) "m, P2=" (rtos perp2 2 2) "m). Kontynuowac? [Tak/Nie] <Tak>: "))) (exit))
    )
  )

  (setq dL (- L2-virt L1-virt))  
  (if (= (abs dL) 0.0) (progn (alert "Punkty rzutuja sie w to samo miejsce!") (exit)))  
  (setq slope (/ (- z2 z1) dL))  
  (princ (strcat "\n>>> Obliczony spadek: " (rtos (* (abs slope) 100.0) 2 3) " % <<<"))  

  ;; --- 4. METODA I OPCJA 3D ---
  (setq L-start (min L1-virt L2-virt) L-end (max L1-virt L2-virt))
  (initget 1 "Odleglosc Podzial") (setq mode (getkword "\n4. Metoda generowania pikiet [Odleglosc/Podzial]: "))   
  (initget "Tak Nie") (setq draw-3d (getkword "\n5. Czy rysowac Polilinie 3D na aktualnej warstwie? [Tak/Nie] <Tak>: "))
  (if (not draw-3d) (setq draw-3d "Tak"))

  ;; --- 5. GENEROWANIE LISTY PIKIETAŻU ---
  (setq steps-list '())
  (if (= mode "Odleglosc")
    (progn (setq step (getreal "\nPodaj odstep [m]: ") L-cur L-start) (while (<= L-cur L-end) (setq steps-list (cons L-cur steps-list) L-cur (+ L-cur step))))
    (progn (setq num-pts (getint "\nPodaj ilosc odcinkow: ") step (/ (abs dL) (float num-pts)) L-cur L-start) (repeat (1+ num-pts) (setq steps-list (cons L-cur steps-list) L-cur (+ L-cur step))))
  )
  (setq steps-list (reverse steps-list))

  ;; --- 6. RYSOWANIE PIKIET I ZBIERANIE PUNKTÓW DO 3D ---
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)) space (vla-get-ModelSpace doc) zlicz 0 poly-pts '())
  
  ;; Próba dodania początku osi do linii 3D (Zostanie zignorowana, jeśli jest poza osią)
  (setq pt-cur (get-safe-curve-pt crvObj L-start) z-cur (+ z1 (* (- L-start L1-virt) slope)))
  (if pt-cur (setq poly-pts (append poly-pts (list (list (car pt-cur) (cadr pt-cur) z-cur)))))

  ;; Rysuj pikiety z Fabryki (Ignoruje te, które wiszą w powietrzu poza linią)
  (foreach L steps-list
    (if (and (> (abs (- L L1-virt)) 0.01) (> (abs (- L L2-virt)) 0.01))
      (progn
        (setq z-cur (+ z1 (* (- L L1-virt) slope)) pt-cur (get-safe-curve-pt crvObj L))
        (if pt-cur
          (progn
            (geocad-wstaw-pikiete-full doc space (list (car pt-cur) (cadr pt-cur) z-cur) "" T)
            (setq zlicz (1+ zlicz))
            (setq poly-pts (append poly-pts (list (list (car pt-cur) (cadr pt-cur) z-cur))))
          )
        )
      )
    )
  )

  ;; Próba dodania końca osi do linii 3D (Zostanie zignorowana, jeśli jest poza osią)
  (setq pt-cur (get-safe-curve-pt crvObj L-end) z-cur (+ z1 (* (- L-end L1-virt) slope)))
  (if pt-cur (setq poly-pts (append poly-pts (list (list (car pt-cur) (cadr pt-cur) z-cur)))))

  ;; --- 7. BEZPOŚREDNIE RYSOWANIE POLILINII 3D (Komenda _3DPOLY) ---
  (if (and (= draw-3d "Tak") (> (length poly-pts) 1))
    (progn
      (setvar "CMDECHO" 0)   ;; Wyłączamy "śmiecenie" w komendach
      (setvar "OSMODE" 0)    ;; Wyłączamy przyciąganie
      
      (command "._3DPOLY")   ;; Rozpocznij linię 3D
      (foreach pt poly-pts
        (command pt)         ;; Wrzucaj kolejne punkty
      )
      (command "")           ;; Wciśnij ENTER, żeby zakończyć rysowanie
      
      (princ "\n-> Narysowano Polilinie 3D na aktualnej warstwie.")
    )
  )

  ;; --- 8. ZAKOŃCZENIE I PRZYWRÓCENIE USTAWIEŃ ---
  (setvar "OSMODE" old-osmode)
  (setvar "CMDECHO" old-cmdecho)
  (setq *error* old-err)   
  
  (princ (strcat "\nSukces! Wstawiono " (itoa zlicz) " nowych pikiet.")) 
  (princ)
)
(princ "\nKomenda: NIWELACJA_2PKT wczytana.") (princ)