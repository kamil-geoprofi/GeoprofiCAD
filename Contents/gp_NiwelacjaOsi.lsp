(vl-load-com)   
(load "gp_Core.lsp" "\nBLAD: Nie znaleziono pliku gp_Core.lsp!")   
    
(defun c:NIWELACJA_OSI ( / old-err crvEnt crvObj baseEnt baseObj pt-base z-base p-proj gap-dist dist-base-dir dist-proj-dir z-proj L-base slope-pct dirPt L-dir dir-mult mode total-L step num-pts steps-list L-cur doc space pt-cur delta-L z-cur pt-list max-offset param deriv dx dy len-deriv Ux Uy vx vy long-gap perp-sq perp-offset)   
      
  (setq old-err *error* *error* (lambda (msg) (princ (strcat "\nPrzerwano: " msg)) (princ)))   
   
  (setq max-offset 0.10)  
    
  (setq crvEnt (car (entsel "\n1. Wybierz os trasy (Linia/Polilinia/Luk): "))) (if (not crvEnt) (exit))   
  (setq crvObj (vlax-ename->vla-object crvEnt))   
    
  (setq baseEnt (car (entsel "\n2. Wybierz punkt bazowy na trasie: "))) (if (not baseEnt) (exit))   
  (setq baseObj (vlax-ename->vla-object baseEnt))   
    
  (setq pt-base (get-pt-from-obj baseObj))  
  (if (not pt-base) (progn (alert "Wybrano nieobslugiwany obiekt!") (exit)))  
    
  (setq z-base (caddr pt-base))   
  (if (or (not z-base) (= z-base 0.0))   
    (progn  
      (setq z-base (getreal "\nPodaj RZEDNA BAZOWA z klawiatury [m]: "))  
      (if z-base (setq pt-base (list (car pt-base) (cadr pt-base) z-base)))  
    )  
  )   
  (if (not z-base) (exit))   
    
  (setq p-proj (vlax-curve-getClosestPointTo crvObj pt-base))   
  (setq L-base (vlax-curve-getDistAtPoint crvObj p-proj))   
 
  (setq gap-dist (distance (list (car pt-base) (cadr pt-base)) (list (car p-proj) (cadr p-proj)))) 
   
  (if (> gap-dist 0.001)  
    (progn 
      (setq param (vlax-curve-getParamAtPoint crvObj p-proj)) 
      (setq deriv (vlax-curve-getFirstDeriv crvObj param)) 
      (setq dx (car deriv) dy (cadr deriv)) 
      (setq len-deriv (distance '(0.0 0.0) (list dx dy))) 
      (setq Ux (/ dx len-deriv) Uy (/ dy len-deriv)) 
      (setq vx (- (car pt-base) (car p-proj)) vy (- (cadr pt-base) (cadr p-proj))) 
      (setq long-gap (abs (+ (* vx Ux) (* vy Uy)))) 
      (setq perp-sq (- (* gap-dist gap-dist) (* long-gap long-gap))) 
      (if (< perp-sq 0.0) (setq perp-sq 0.0))  
      (setq perp-offset (sqrt perp-sq)) 
 
      (if (> perp-offset max-offset) 
        (progn 
          (alert (strcat "UWAGA: ZADZIALAL BEZPIECZNIK!\nPunkt ucieka w bok o " (rtos perp-offset 2 2) " m.")) 
          (exit) 
        ) 
      ) 
    ) 
    (setq long-gap 0.0) 
  ) 
 
  (setq slope-pct (getreal "\n3. Podaj spadek [%] (+ w gore, - w dol): ")) (if (not slope-pct) (exit))   
  (setq dirPt (getpoint p-proj "\n4. Wskaz myszka kierunek trasy: ")) (if (not dirPt) (exit))   
 
  (if (> long-gap 0.001) 
    (progn 
      (setq dist-base-dir (distance (list (car pt-base) (cadr pt-base)) (list (car dirPt) (cadr dirPt)))) 
      (setq dist-proj-dir (distance (list (car p-proj) (cadr p-proj)) (list (car dirPt) (cadr dirPt)))) 
      (if (> dist-base-dir dist-proj-dir) 
        (setq z-proj (+ z-base (* long-gap (/ slope-pct 100.0)))) 
        (setq z-proj (- z-base (* long-gap (/ slope-pct 100.0)))) 
      ) 
    ) 
    (setq z-proj z-base) 
  ) 
 
  (setq L-dir (vlax-curve-getDistAtPoint crvObj (vlax-curve-getClosestPointTo crvObj (trans dirPt 1 0))))   
  (setq dir-mult (if (> L-dir L-base) 1.0 -1.0))   
    
  (initget 1 "Odleglosc Podzial") (setq mode (getkword "\n5. Metoda generowania [Odleglosc/Podzial]: "))   
  (setq total-L (vlax-curve-getDistAtParam crvObj (vlax-curve-getEndParam crvObj)) steps-list '())   
  (if (= mode "Odleglosc")   
    (progn (setq step (getreal "\nPodaj odstep [m]: ") L-cur 0.0) (while (<= L-cur total-L) (setq steps-list (cons L-cur steps-list) L-cur (+ L-cur step))) (if (not (equal (car steps-list) total-L 0.001)) (setq steps-list (cons total-L steps-list))))   
    (progn (setq num-pts (getint "\nPodaj ilosc rownych odcinkow: ") step (/ total-L (float num-pts)) L-cur 0.0) (repeat (1+ num-pts) (setq steps-list (cons L-cur steps-list) L-cur (+ L-cur step)))))   
      
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)) space (vla-get-ModelSpace doc))   
  (princ "\nGenerowanie pikiet...")   
      
  (foreach L (reverse steps-list)   
    (setq pt-cur (vlax-curve-getPointAtDist crvObj L) delta-L (* (- L L-base) dir-mult) z-cur (+ z-proj (* delta-L (/ slope-pct 100.0))))   
    (setq pt-list (list (car pt-cur) (cadr pt-cur) z-cur))   
    
    (geocad-wstaw-pikiete-full doc space pt-list "" T)   
  )   
  (setq *error* old-err) (princ (strcat "\nSukces! Wygenerowano " (itoa (length steps-list)) " pikiet.")) (princ)   
)   
(princ "\nKomenda wczytana: NIWELACJA_OSI") (princ)