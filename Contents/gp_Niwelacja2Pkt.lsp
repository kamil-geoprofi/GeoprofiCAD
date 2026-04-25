(vl-load-com)   
(load "gp_Core.lsp" "\nBLAD: Nie znaleziono pliku gp_Core.lsp!")   
    
(defun c:NIWELACJA_2PKT ( / old-err crvEnt crvObj obj1 obj2 pt1 pt2 z1 z2 p1-proj p2-proj  
                          L1-base L2-base gap1 gap2 param1 param2 deriv1 deriv2 len1 len2  
                          U1x U1y U2x U2y v1x v1y v2x v2y dot1 dot2 long1 long2  
                          perp-sq1 perp-sq2 perp1 perp2 L1-virt L2-virt  
                          dL dZ slope pct-slope mode total-L step num-pts steps-list L-cur  
                          doc space pt-cur z-cur pt-list max-offset ans1 ans2 zlicz)   
      
  (setq old-err *error* *error* (lambda (msg) (princ (strcat "\nPrzerwano: " msg)) (princ)))   
    
  (setq max-offset 0.15)   
    
  (setq crvEnt (car (entsel "\n1. Wybierz os trasy (Linia/Polilinia/Luk): "))) (if (not crvEnt) (exit))   
  (setq crvObj (vlax-ename->vla-object crvEnt))   
    
  (setq obj1 (vlax-ename->vla-object (car (entsel "\n2. Wybierz PUNKT 1 (Blok/Punkt/Tekst): "))))   
  (setq pt1 (get-pt-from-obj obj1))   
  (if (not pt1) (progn (alert "Wybrano nieobslugiwany obiekt!") (exit)))   
  (setq z1 (caddr pt1))   
  (if (or (not z1) (= z1 0.0)) (setq z1 (getreal "\nPodaj RZEDNA 1 z klawiatury [m]: ")))   
    
  (setq obj2 (vlax-ename->vla-object (car (entsel "\n3. Wybierz PUNKT 2 (Blok/Punkt/Tekst): "))))   
  (setq pt2 (get-pt-from-obj obj2))   
  (if (not pt2) (progn (alert "Wybrano nieobslugiwany obiekt!") (exit)))   
  (setq z2 (caddr pt2))   
  (if (or (not z2) (= z2 0.0)) (setq z2 (getreal "\nPodaj RZEDNA 2 z klawiatury [m]: ")))   
  
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
      (setq long1 (abs dot1)) 
      (setq perp-sq1 (- (* gap1 gap1) (* long1 long1))) 
      (if (< perp-sq1 0.0) (setq perp-sq1 0.0)) 
      (setq perp1 (sqrt perp-sq1)) 
      (setq L1-virt (if (> dot1 0) (+ L1-base long1) (- L1-base long1))) 
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
      (setq long2 (abs dot2)) 
      (setq perp-sq2 (- (* gap2 gap2) (* long2 long2))) 
      (if (< perp-sq2 0.0) (setq perp-sq2 0.0)) 
      (setq perp2 (sqrt perp-sq2)) 
      (setq L2-virt (if (> dot2 0) (+ L2-base long2) (- L2-base long2))) 
    ) 
    (setq perp2 0.0 L2-virt L2-base) 
  ) 
 
  (if (> perp1 max-offset)  
    (progn  
      (initget "Tak Nie") 
      (setq ans1 (getkword (strcat "\n[UWAGA] Punkt 1 ucieka W BOK od osi o " (rtos perp1 2 2) " m. Wymusic? [Tak/Nie] <Nie>: "))) 
      (if (not (= ans1 "Tak")) (progn (princ "\nPrzerwano.") (exit))) 
    )  
  )  
 
  (if (> perp2 max-offset)  
    (progn  
      (initget "Tak Nie") 
      (setq ans2 (getkword (strcat "\n[UWAGA] Punkt 2 ucieka W BOK od osi o " (rtos perp2 2 2) " m. Wymusic? [Tak/Nie] <Nie>: "))) 
      (if (not (= ans2 "Tak")) (progn (princ "\nPrzerwano.") (exit))) 
    )  
  )  
  
  (setq dL (- L2-virt L1-virt))  
  (if (= (abs dL) 0.0)   
    (progn (alert "BLAD: Punkty rzutuja sie w to samo miejsce na osi. Brak mozliwosci wyliczenia spadku!") (exit))  
  )  
    
  (setq slope (/ (- z2 z1) dL))  
  (setq pct-slope (* (abs slope) 100.0))  
  
  (princ (strcat "\n>>> WYLICZONY SPADEK Z INTERPOLACJI: " (rtos pct-slope 2 3) " % <<<"))  
  
  (initget 1 "Odleglosc Podzial") (setq mode (getkword "\n4. Metoda generowania pikiet [Odleglosc/Podzial]: "))   
  (setq total-L (vlax-curve-getDistAtParam crvObj (vlax-curve-getEndParam crvObj)) steps-list '())   
  (if (= mode "Odleglosc")   
    (progn (setq step (getreal "\nPodaj odstep [m]: ") L-cur 0.0) (while (<= L-cur total-L) (setq steps-list (cons L-cur steps-list) L-cur (+ L-cur step))) (if (not (equal (car steps-list) total-L 0.001)) (setq steps-list (cons total-L steps-list))))   
    (progn (setq num-pts (getint "\nPodaj ilosc rownych odcinkow: ") step (/ total-L (float num-pts)) L-cur 0.0) (repeat (1+ num-pts) (setq steps-list (cons L-cur steps-list) L-cur (+ L-cur step)))))   
        
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)) space (vla-get-ModelSpace doc) zlicz 0)   
  (princ "\nGenerowanie pikiet...")   
        
  (foreach L (reverse steps-list)   
    (setq z-cur (+ z1 (* (- L L1-virt) slope))) 
    (setq pt-cur (vlax-curve-getPointAtDist crvObj L))  
    (setq pt-list (list (car pt-cur) (cadr pt-cur) z-cur))   
     
    (geocad-wstaw-pikiete-full doc space pt-list "" T)   
    (setq zlicz (1+ zlicz))   
  )   
  (setq *error* old-err)   
  (princ (strcat "\nSukces! Wygenerowano " (itoa zlicz) " pikiet z interpolacji."))   
  (princ)   
)   
(princ "\nKomenda wczytana: NIWELACJA_2PKT") (princ)