(vl-load-com)   
(load "gp_Core.lsp" "\nBLAD: Nie znaleziono pliku gp_Core.lsp!")   
   
;; --- UNIWERSALNA FUNKCJA POBIERANIA PUNKTU --- 
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
   
;; --- GŁÓWNY SKRYPT NIWELACJI Z 2 PUNKTÓW Z ZAAWANSOWANYM WEKTOREM ---
(defun c:NIWELACJA_2PKT ( / old-err crvEnt crvObj obj1 obj2 pt1 pt2 z1 z2 p1-proj p2-proj 
                          L1-base L2-base gap1 gap2 param1 param2 deriv1 deriv2 len1 len2 
                          U1x U1y U2x U2y v1x v1y v2x v2y dot1 dot2 long1 long2 
                          perp-sq1 perp-sq2 perp1 perp2 L1-virt L2-virt 
                          dL dZ slope pct-slope mode total-L step num-pts steps-list L-cur 
                          doc space pt-cur z-cur nr-str auto-nr pt-list max-offset ans1 ans2)   
     
  (setq old-err *error* *error* (lambda (msg) (princ (strcat "\nPrzerwano: " msg)) (princ)))   
   
  ;; Tolerancja odchylenia bocznego (Bezpiecznik)
  (setq max-offset 0.15)  
   
  ;; --- KROK 1: Wybór linii --- 
  (setq crvEnt (car (entsel "\n1. Wybierz os trasy (Linia/Polilinia/Luk): "))) (if (not crvEnt) (exit))   
  (setq crvObj (vlax-ename->vla-object crvEnt))   
   
  ;; --- KROK 2: Wybór Punktu 1 --- 
  (setq obj1 (vlax-ename->vla-object (car (entsel "\n2. Wybierz PUNKT 1 (Blok/Punkt/Tekst): "))))   
  (setq pt1 (get-pt-from-obj obj1))  
  (if (not pt1) (progn (alert "Wybrano nieobslugiwany obiekt dla Punktu 1!") (exit)))  
  (setq z1 (caddr pt1))   
  (if (or (not z1) (= z1 0.0)) (setq z1 (getreal "\nPunkt 1 ma Z=0. Podaj RZEDNA 1 z klawiatury [m]: ")))   
   
  ;; --- KROK 3: Wybór Punktu 2 --- 
  (setq obj2 (vlax-ename->vla-object (car (entsel "\n3. Wybierz PUNKT 2 (Blok/Punkt/Tekst): "))))   
  (setq pt2 (get-pt-from-obj obj2))  
  (if (not pt2) (progn (alert "Wybrano nieobslugiwany obiekt dla Punktu 2!") (exit)))  
  (setq z2 (caddr pt2))   
  (if (or (not z2) (= z2 0.0)) (setq z2 (getreal "\nPunkt 2 ma Z=0. Podaj RZEDNA 2 z klawiatury [m]: ")))   
 
  ;; --- KROK 4A: Zaawansowana matematyka dla PUNKTU 1 --- 
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
      (setq dot1 (+ (* v1x U1x) (* v1y U1y))) ; Iloczyn skalarny
      (setq long1 (abs dot1))
      (setq perp-sq1 (- (* gap1 gap1) (* long1 long1)))
      (if (< perp-sq1 0.0) (setq perp-sq1 0.0))
      (setq perp1 (sqrt perp-sq1))
      ;; Określenie wirtualnego pikietażu (czy przedłużamy zgodnie z wektorem, czy pod prąd)
      (setq L1-virt (if (> dot1 0) (+ L1-base long1) (- L1-base long1)))
    )
    (setq perp1 0.0 L1-virt L1-base)
  )

  ;; --- KROK 4B: Zaawansowana matematyka dla PUNKTU 2 --- 
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

  ;; --- KROK 5: Ostrzeżenia Bezpiecznika (Teraz reaguje tylko na błąd "W BOK") --- 
  (if (> perp1 max-offset) 
    (progn 
      (initget "Tak Nie")
      (setq ans1 (getkword (strcat "\n[UWAGA] Punkt 1 ucieka W BOK od fizycznej osi o " (rtos perp1 2 2) " m. Wymusic obliczenia? [Tak/Nie] <Nie>: ")))
      (if (not (= ans1 "Tak")) (progn (princ "\nPrzerwano z powodu odchylki Punktu 1.") (exit)))
    ) 
  ) 

  (if (> perp2 max-offset) 
    (progn 
      (initget "Tak Nie")
      (setq ans2 (getkword (strcat "\n[UWAGA] Punkt 2 ucieka W BOK od fizycznej osi o " (rtos perp2 2 2) " m. Wymusic obliczenia? [Tak/Nie] <Nie>: ")))
      (if (not (= ans2 "Tak")) (progn (princ "\nPrzerwano z powodu odchylki Punktu 2.") (exit)))
    ) 
  ) 
 
  ;; --- KROK 6: Obliczenie matematyki spadku --- 
  (setq dL (- L2-virt L1-virt)) 
  (if (= (abs dL) 0.0)  
    (progn (alert "BŁĄD: Punkty rzutuja sie dokladnie w to samo miejsce na osi pikietażu (L1=L2). Nie mozna wyliczyc spadku!") (exit)) 
  ) 
   
  (setq slope (/ (- z2 z1) dL)) 
  (setq pct-slope (* (abs slope) 100.0)) 
 
  (princ (strcat "\n>>> WYLICZONY SPADEK Z INTERPOLACJI: " (rtos pct-slope 2 3) " % <<<")) 
 
  ;; --- KROK 7: Generowanie podziału --- 
  (initget 1 "Odleglosc Podzial") (setq mode (getkword "\n4. Metoda generowania pikiet [Odleglosc/Podzial]: "))   
  (setq total-L (vlax-curve-getDistAtParam crvObj (vlax-curve-getEndParam crvObj)) steps-list '())   
  (if (= mode "Odleglosc")   
    (progn (setq step (getreal "\nPodaj odstep [m]: ") L-cur 0.0) (while (<= L-cur total-L) (setq steps-list (cons L-cur steps-list) L-cur (+ L-cur step))) (if (not (equal (car steps-list) total-L 0.001)) (setq steps-list (cons total-L steps-list))))   
    (progn (setq num-pts (getint "\nPodaj ilosc rownych odcinkow: ") step (/ total-L (float num-pts)) L-cur 0.0) (repeat (1+ num-pts) (setq steps-list (cons L-cur steps-list) L-cur (+ L-cur step)))))   
       
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)) space (vla-get-ModelSpace doc) auto-nr 1)   
  (princ "\nGenerowanie pikiet (ustawienia wizualne pobrano z GEO_SETUP)...")   
       
  ;; --- KROK 8: Rysowanie --- 
  (foreach L (reverse steps-list)   
    ;; Genialnie prosty wzór działający dla wszystkich kombinacji wektorów:
    (setq z-cur (+ z1 (* (- L L1-virt) slope)))
       
    (setq pt-cur (vlax-curve-getPointAtDist crvObj L)) 
    (setq pt-list (list (car pt-cur) (cadr pt-cur) z-cur) nr-str (itoa auto-nr))   
    
    (geocad-wstaw-pikiete-full doc space pt-list nr-str T)   
   
    (setq auto-nr (1+ auto-nr))   
  )   
  (setq *error* old-err)  
  (princ (strcat "\nSukces! Wygenerowano " (itoa (length steps-list)) " pikiet na bazie interpolacji z 2 punktow."))  
  (princ)   
)   
(princ "\nKomenda wczytana: NIWELACJA_2PKT") (princ)