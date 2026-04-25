(defun c:NIWELACJA_KRAWEDZI ( / old-err srcEnt srcObj ssAll i obj pt projPt ptsList tgtEnt tgtObj slope-pct doc space pt-tgt dist2d z-tgt pt-list nr-str auto-nr is-orthogonal param deriv Ux Uy vx vy dot lenU cos-theta skipped-nr) 
   
  (setq old-err *error* *error* (lambda (msg) (princ (strcat "\nPrzerwano: " msg)) (princ))) 
   
  ;; --- KROK 1: Wybór linii źródłowej --- 
  (setq srcEnt (car (entsel "\n1. Wybierz linie ZRODLOWA (na ktorej sa juz pikiety): ")))  
  (if (not srcEnt) (exit)) 
  (setq srcObj (vlax-ename->vla-object srcEnt)) 
   
  (princ "\nSkanowanie rysunku w poszukiwaniu pikiet na tej linii...") 
   
  (setq ssAll (ssget "X" '((0 . "INSERT,POINT") (-4 . "<OR") (2 . "Pikieta_Geo") (0 . "POINT") (-4 . "OR>")))) 
  (setq ptsList '()) 
   
  (if ssAll 
    (progn 
      (setq i 0) 
      (while (< i (sslength ssAll)) 
        (setq obj (vlax-ename->vla-object (ssname ssAll i))) 
        (setq pt (get-pt-from-obj obj)) 
         
        (if pt 
          (progn 
            (setq projPt (vlax-curve-getClosestPointTo srcObj pt)) 
            (if projPt 
              (progn 
                (if (< (distance (list (car pt) (cadr pt)) (list (car projPt) (cadr projPt))) 0.01) 
                  (setq ptsList (cons pt ptsList)) 
                ) 
              ) 
            ) 
          ) 
        ) 
        (setq i (1+ i)) 
      ) 
    ) 
  ) 
   
  (if (null ptsList)  
    (progn (alert "Nie znaleziono zadnych pikiet lezacych na wybranej linii!") (exit)) 
  ) 
  (princ (strcat "\nZnaleziono " (itoa (length ptsList)) " pikiet nalezacych do tej linii.")) 
 
  ;; --- KROK 2: Wybór linii docelowej --- 
  (setq tgtEnt (car (entsel "\n2. Wybierz linie DOCELOWA (na ktora przeniesc pikiety): ")))  
  (if (not tgtEnt) (exit)) 
  (setq tgtObj (vlax-ename->vla-object tgtEnt)) 
 
  ;; --- KROK 3: Spadek poprzeczny --- 
  (setq slope-pct (getreal "\n3. Podaj spadek poprzeczny [%] (- w dol, + w gore): "))  
  (if (not slope-pct) (exit)) 
 
  ;; --- KROK 4: Generowanie z Filtrem Prostopadłości --- 
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)) space (vla-get-ModelSpace doc) auto-nr 1 skipped-nr 0) 
  (princ "\nGenerowanie pikiet (ustawienia wizualne pobrano z GEO_SETUP)...") 
 
  (foreach pt-src (reverse ptsList) 
     
    (setq pt-tgt (vlax-curve-getClosestPointTo tgtObj pt-src)) 
     
    (if pt-tgt 
      (progn 
        (setq dist2d (distance (list (car pt-src) (cadr pt-src)) (list (car pt-tgt) (cadr pt-tgt)))) 
        (setq is-orthogonal T)

        ;; MATEMATYCZNY TEST PROSTOPADŁOŚCI
        (if (> dist2d 0.001)
          (progn
            (setq param (vlax-curve-getParamAtPoint tgtObj pt-tgt))
            (setq deriv (vlax-curve-getFirstDeriv tgtObj param))
            
            (setq Ux (car deriv) Uy (cadr deriv))
            (setq vx (- (car pt-src) (car pt-tgt)) vy (- (cadr pt-src) (cadr pt-tgt)))
            
            ;; Iloczyn skalarny (Dot Product)
            (setq dot (abs (+ (* Ux vx) (* Uy vy))))
            (setq lenU (distance '(0.0 0.0) (list Ux Uy)))
            (setq cos-theta (/ dot (* lenU dist2d)))
            
            ;; Jeśli wektory nie przecinają się pod kątem 90 stopni (cosinus bliski 0)
            ;; oznacza to, że punkt ześlizgnął się na koniec linii. Tolerancja ~2.8 stopnia.
            (if (> cos-theta 0.05)
              (setq is-orthogonal nil)
            )
          )
        )
        
        ;; Jeśli przeszedł test - rysujemy
        (if is-orthogonal
          (progn
            (setq z-tgt (+ (caddr pt-src) (* dist2d (/ slope-pct 100.0)))) 
            (setq pt-list (list (car pt-tgt) (cadr pt-tgt) z-tgt)) 
            (setq nr-str (itoa auto-nr)) 
             
            (geocad-wstaw-pikiete-full doc space pt-list nr-str T) 
             
            (setq auto-nr (1+ auto-nr)) 
          )
          ;; Jeśli oblał test - zliczamy jako pominięty
          (setq skipped-nr (1+ skipped-nr))
        )
      ) 
    ) 
  ) 
   
  (setq *error* old-err)  
  (princ (strcat "\nSukces! Wygenerowano " (itoa (1- auto-nr)) " nowych pikiet na krawedzi."))  
  (if (> skipped-nr 0) 
    (princ (strcat " (Zignorowano " (itoa skipped-nr) " pikiet rzutujacych sie poza fizyczny zakres krawedzi)."))
  )
  (princ) 
) 
(princ "\nKomenda wczytana: NIWELACJA_KRAWEDZI") (princ)