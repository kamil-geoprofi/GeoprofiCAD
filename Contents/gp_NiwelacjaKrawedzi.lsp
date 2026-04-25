(vl-load-com)
(load "gp_Core.lsp" "\nBLAD: Nie znaleziono pliku gp_Core.lsp!")

(defun c:NIWELACJA_KRAWEDZI ( / old-err srcEnt srcObj ssAll i obj pt projPt ptsList tgtEnt tgtObj slope-pct doc space pt-tgt dist2d z-tgt pt-list is-orthogonal param deriv Ux Uy vx vy dot lenU cos-theta skipped-nr zlicz)  
    
  (setq old-err *error* *error* (lambda (msg) (princ (strcat "\nPrzerwano: " msg)) (princ)))  
    
  (setq srcEnt (car (entsel "\n1. Wybierz linie ZRODLOWA (na ktorej sa juz pikiety): ")))   
  (if (not srcEnt) (exit))  
  (setq srcObj (vlax-ename->vla-object srcEnt))  
    
  (princ "\nSkanowanie rysunku...")  
    
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
    (progn (alert "Nie znaleziono pikiet lezacych na linii!") (exit))  
  )  
  (princ (strcat "\nZnaleziono " (itoa (length ptsList)) " pikiet."))  
  
  (setq tgtEnt (car (entsel "\n2. Wybierz linie DOCELOWA (na ktora przeniesc pikiety): ")))   
  (if (not tgtEnt) (exit))  
  (setq tgtObj (vlax-ename->vla-object tgtEnt))  
  
  (setq slope-pct (getreal "\n3. Podaj spadek poprzeczny [%] (- w dol, + w gore): "))   
  (if (not slope-pct) (exit))  
  
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)) space (vla-get-ModelSpace doc) skipped-nr 0 zlicz 0)  
  (princ "\nGenerowanie pikiet...")  
  
  (foreach pt-src (reverse ptsList)  
      
    (setq pt-tgt (vlax-curve-getClosestPointTo tgtObj pt-src))  
      
    (if pt-tgt  
      (progn  
        (setq dist2d (distance (list (car pt-src) (cadr pt-src)) (list (car pt-tgt) (cadr pt-tgt))))  
        (setq is-orthogonal T) 
 
        (if (> dist2d 0.001) 
          (progn 
            (setq param (vlax-curve-getParamAtPoint tgtObj pt-tgt)) 
            (setq deriv (vlax-curve-getFirstDeriv tgtObj param)) 
            (setq Ux (car deriv) Uy (cadr deriv)) 
            (setq vx (- (car pt-src) (car pt-tgt)) vy (- (cadr pt-src) (cadr pt-tgt))) 
            (setq dot (abs (+ (* Ux vx) (* Uy vy)))) 
            (setq lenU (distance '(0.0 0.0) (list Ux Uy))) 
            (setq cos-theta (/ dot (* lenU dist2d))) 
            (if (> cos-theta 0.05) 
              (setq is-orthogonal nil) 
            ) 
          ) 
        ) 
          
        (if is-orthogonal 
          (progn 
            (setq z-tgt (+ (caddr pt-src) (* dist2d (/ slope-pct 100.0))))  
            (setq pt-list (list (car pt-tgt) (cadr pt-tgt) z-tgt))  
              
            (geocad-wstaw-pikiete-full doc space pt-list "" T)  
            (setq zlicz (1+ zlicz))  
          ) 
          (setq skipped-nr (1+ skipped-nr)) 
        ) 
      )  
    )  
  )  
    
  (setq *error* old-err)   
  (princ (strcat "\nSukces! Wygenerowano " (itoa zlicz) " nowych pikiet."))   
  (if (> skipped-nr 0)  
    (princ (strcat " (Zignorowano " (itoa skipped-nr) " pikiet rzutujacych sie poza krawedz).")) 
  ) 
  (princ)  
)  
(princ "\nKomenda wczytana: NIWELACJA_KRAWEDZI") (princ)