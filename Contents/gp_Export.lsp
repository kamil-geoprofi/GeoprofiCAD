(vl-load-com)   

;; ==========================================  
;; --- FUNKCJE POMOCNICZE I MATEMATYCZNE ---  
;; ==========================================  

(defun dist-2d (p1 p2)  
  (distance (list (car p1) (cadr p1)) (list (car p2) (cadr p2)))  
)  

(defun categorize-text (txt / norm-txt is-num has-sep)  
  ;; Ulepszone czyszczenie tekstu (np. ze spacji lub litery "m")
  (setq norm-txt (vl-string-trim " mM\r\n\t" (vl-string-translate "," "." txt)))  
  (setq is-num (distof norm-txt))  
  (setq has-sep (or (vl-string-search "." norm-txt) (vl-string-search "," txt)))  
  (if (and is-num has-sep) "Z" "ID")  
)  

;; ZMODYFIKOWANY ALGORYTM AABB (Podwójna Metryka: Krawędź i Środek)  
(defun get-dist-to-txt (pt t-item / px py rMinX rMaxX rMinY rMaxY cX cY d-edge cx-center cy-center d-center)  
  (setq px (car pt) py (cadr pt))  
  (setq rMinX (min (caar t-item) (caadr t-item)) rMaxX (max (caar t-item) (caadr t-item)))  
  (setq rMinY (min (cadar t-item) (cadadr t-item)) rMaxY (max (cadar t-item) (cadadr t-item)))  

  ;; 1. Odległość do krawędzi (Do zaliczenia w Radarze)  
  (setq cX (max rMinX (min px rMaxX)) cY (max rMinY (min py rMaxY)))  
  (setq d-edge (dist-2d pt (list cX cY)))  

  ;; 2. Odległość do fizycznego środka (Jako Tie-breaker / Sędzia)  
  (setq cx-center (/ (+ rMinX rMaxX) 2.0) cy-center (/ (+ rMinY rMaxY) 2.0))  
  (setq d-center (dist-2d pt (list cx-center cy-center)))  

  (list d-edge d-center)  
)  

(defun geocad-exp-err (msg)   
  (if f (vl-catch-all-apply 'close (list f)))   
  (if saved-sysvars (foreach pair saved-sysvars (vl-catch-all-apply 'setvar (list (car pair) (cdr pair)))))   
  (setq *error* old-err)    
  (princ (if (member msg '("Function cancelled" "quit / exit abort")) "\nPrzerwano." (strcat "\nBlad: " msg)))   
  (princ)   
)   

(defun get-native-windows-save-file (out-format / wsh tmpFile shellCmd f res filter title default-ext )    
  ;; Dialog zapisu dopasowany do wybranego formatu eksportu.
  ;; TXT - klasyczny eksport pikiet z numerem.
  ;; PTS - prosta chmura punktow z liczba punktow w pierwszej linii.
  (if (= out-format "pts")
    (progn
      (setq filter "Chmura punktow PTS (*.pts)|*.pts|Pliki tekstowe (*.txt)|*.txt|Wszystkie pliki (*.*)|*.*")
      (setq title "Zapisz chmure punktow PTS")
      (setq default-ext "pts")
    )

    (progn
      (setq filter "Pliki tekstowe (*.txt)|*.txt|Wszystkie pliki (*.*)|*.*")
      (setq title "Zapisz pikiety TXT")
      (setq default-ext "txt")
    )
  )

  (setq wsh (vlax-create-object "WScript.Shell") tmpFile (vl-filename-mktemp "file_res.txt"))    
  (setq shellCmd
    (strcat
      "powershell.exe -WindowStyle Hidden -Command \"& {"
      "Add-Type -AssemblyName System.Windows.Forms;"
      "$d = New-Object System.Windows.Forms.SaveFileDialog;"
      "$d.Filter = '"
      filter
      "';"
      "$d.DefaultExt = '"
      default-ext
      "';"
      "$d.AddExtension = $true;"
      "$d.Title = '"
      title
      "';"
      "if($d.ShowDialog() -eq 'OK') { [System.IO.File]::WriteAllText('"
      (vl-string-translate "\\" "/" tmpFile)
      "', $d.FileName) }}"
      "\""
    )
  )    
  (vlax-invoke-method wsh 'Run shellCmd 0 :vlax-true)   
  (if (findfile tmpFile) (progn (setq f (open tmpFile "r")) (setq res (read-line f)) (close f) (vl-file-delete tmpFile)))    
  res    
)   

(defun detect-epsg (pt / x y res)   
  (if (not pt) (setq res "Brak")   
    (progn (setq x (car pt) y (cadr pt) res "Uklad Lokalny")   
      (if (and (> y 4900000) (< y 6100000))   
        (cond ((and (> x 5300000) (< x 5900000)) (setq res "Uklad 2000 (S5)"))   
              ((and (> x 6300000) (< x 6900000)) (setq res "Uklad 2000 (S6)"))   
              ((and (> x 7300000) (< x 7900000)) (setq res "Uklad 2000 (S7)"))   
              ((and (> x 8300000) (< x 8900000)) (setq res "Uklad 2000 (S8)"))))   
      (if (and (= res "Uklad Lokalny") (> x 3000000) (< x 6000000) (> y 3000000) (< y 6000000))   
          (setq res "Uklad 1965 (?)"))   
      res)))   

(defun extract-v22 (obj mode-solid / res type start-p end-p i ent-data)   
  (setq res '())   
  (setq type (vla-get-ObjectName obj))   
  (cond   
    ((member type '("AcDbPoint" "AcDbBlockReference"))   
     (setq res (list (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint obj))))))   

    ;; OKRĄG - eksportowany jest wyłącznie środek okręgu
    ((= type "AcDbCircle")
     (setq res (list (vlax-safearray->list (vlax-variant-value (vla-get-Center obj))))))

    ((member type '("AcDbLine" "AcDbPolyline" "AcDb2dPolyline" "AcDb3dPolyline" "AcDbArc"))  
     (cond  
       ((= type "AcDbArc")   
        (setq start-p (vlax-curve-getStartParam obj) end-p (vlax-curve-getEndParam obj))  
        (setq res (list (vlax-curve-getPointAtParam obj start-p)   
                        (vlax-curve-getPointAtParam obj (+ start-p (/ (- end-p start-p) 2.0)))   
                        (vlax-curve-getPointAtParam obj end-p))))  
       ((= type "AcDbLine")  
        (setq res (list (vlax-curve-getStartPoint obj) (vlax-curve-getEndPoint obj))))  
       (t   
        (setq start-p (fix (vlax-curve-getStartParam obj)) end-p (fix (vlax-curve-getEndParam obj)))  
        (setq i start-p)   
        (while (<= i end-p) (setq res (cons (vlax-curve-getPointAtParam obj i) res)) (setq i (1+ i)))   
        (setq res (reverse res)))))   
    ((= type "AcDbSolid")   
     (setq ent-data (entget (vlax-vla-object->ename obj)))   
     (setq res (list (trans (cdr (assoc 10 ent-data)) (vlax-vla-object->ename obj) 0)))   
     (if (/= mode-solid "1")   
       (setq res (append res (list (trans (cdr (assoc 11 ent-data)) (vlax-vla-object->ename obj) 0)   
                                   (trans (cdr (assoc 12 ent-data)) (vlax-vla-object->ename obj) 0)   
                                   (trans (cdr (assoc 13 ent-data)) (vlax-vla-object->ename obj) 0))))))   
  )   
  res   
)   

(defun parse-tags (str / res tmp)   
  (setq res '() tmp "")   
  (foreach ch (vl-string->list (strcase str)) (if (member ch '(44 59 32 9)) (if (/= tmp "") (progn (setq res (cons tmp res)) (setq tmp ""))) (setq tmp (strcat tmp (chr ch)))))   
  (if (/= tmp "") (setq res (cons tmp res))) (reverse res)   
)   

(defun format-coord (val) (vl-string-translate "," "." (rtos val 2 3)))   


;; ==========================================  
;; --- GŁÓWNA KOMENDA EKSPORTU ---  
;; ==========================================  

(defun c:EKSPORT_PIKIET_V22 ( / ss i ent obj type c-pts c-blks c-lines c-solids c-arcs c-circles c-txt-z c-txt-id found-tags pts-data txt-list detected-sys sys-warn tags-str dcl-file dcl-fn dcl-id status filename f u-keys dupes pk pt x y z nr m-z m-id c-z c-id dists d-edge d-center t-val cat b-tags z-tags txt_rad geo_mode dupe_mode solid_mode auto_pref blk_tag z_tag auto_start count-exp run-analysis unique-pts d_tol d_tol_str renum_all fix_dupes auto_start_str accepted-pts used-ids is-dupe needs_new_id z_offset z_offset_str export_format export-lines export-line)   

  (setq old-err *error* *error* geocad-exp-err f nil)   

  ;; Dodano CIRCLE do filtra wyboru
  (setq ss (ssget '((0 . "POINT,INSERT,TEXT,MTEXT,LINE,LWPOLYLINE,POLYLINE,SOLID,ARC,CIRCLE"))))   
  (if (not ss) (exit))   

  (princ "\nAnaliza geometrii... prosze czekac.")  

  ;; 1. GROMADZENIE DANYCH  
  (setq i 0 c-pts 0 c-blks 0 c-lines 0 c-arcs 0 c-circles 0 c-solids 0 c-txt-z 0 c-txt-id 0)  
  (setq pts-data '() txt-list '() found-tags '())  

  (while (< i (sslength ss))   
    (setq ent (ssname ss i) obj (vlax-ename->vla-object ent) type (vla-get-ObjectName obj))   
    (cond   
      ((= type "AcDbPoint") (setq c-pts (1+ c-pts)) (setq pts-data (cons (list (car (extract-v22 obj "1")) obj) pts-data)))   
      ((= type "AcDbBlockReference")    
       (setq c-blks (1+ c-blks))   
       (if (= (vla-get-HasAttributes obj) :vlax-true)   
         (foreach att (vlax-invoke obj 'GetAttributes) (setq tstr (strcase (vla-get-TagString att))) (if (not (member tstr found-tags)) (setq found-tags (cons tstr found-tags)))))  
       (setq pts-data (cons (list (car (extract-v22 obj "1")) obj) pts-data)))   
      ((member type '("AcDbText" "AcDbMText"))   
       (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'minPt 'maxPt))))  
         (progn  
           (setq t-val (vla-get-TextString obj) cat (categorize-text t-val))  
           (if (= cat "Z") (setq c-txt-z (1+ c-txt-z)) (setq c-txt-id (1+ c-txt-id)))  
           (setq txt-list (cons (list (vlax-safearray->list minPt) (vlax-safearray->list maxPt) t-val cat) txt-list)))))   

      ;; OKRĄG - do analizy trafia środek okręgu
      ((= type "AcDbCircle")
       (setq c-circles (1+ c-circles))
       (setq pts-data (cons (list (car (extract-v22 obj "1")) obj) pts-data)))

      ((= type "AcDbSolid")   
       (setq c-solids (1+ c-solids))   
       (setq pts-data (cons (list (car (extract-v22 obj "1")) obj) pts-data)))   
      (t   
       (if (= type "AcDbArc") (setq c-arcs (1+ c-arcs)) (setq c-lines (1+ c-lines)))  
       (foreach p (extract-v22 obj "1") (setq pts-data (cons (list p obj) pts-data))))  
    )   
    (setq i (1+ i))   
  )   

  ;; FILTROWANIE UNIKALNYCH PUNKTÓW DO RAPORTU  
  (setq u-keys '() unique-pts '() dupes 0)  
  (foreach p pts-data   
    (setq pk (strcat (rtos (caar p) 2 3) "_" (rtos (cadar p) 2 3)))   
    (if (not (member pk u-keys))  
      (progn (setq u-keys (cons pk u-keys) unique-pts (cons p unique-pts)))  
      (setq dupes (1+ dupes))  
    )  
  )  

  (if unique-pts   
    (progn (setq res1 (detect-epsg (car (nth 0 unique-pts))) res2 (detect-epsg (car (nth (/ (length unique-pts) 2) unique-pts))) res3 (detect-epsg (car (last unique-pts))))  
           (if (and (= res1 res2) (= res2 res3)) (setq detected-sys res1 sys-warn nil) (setq detected-sys "ALARM: NIEZGODNOŚĆ UKŁADÓW!" sys-warn T)))  
    (setq detected-sys "Brak punktow" sys-warn nil))  

  (setq tags-str "")   
  (if found-tags   
    (progn (foreach tg (reverse found-tags) (setq tags-str (strcat tags-str tg ", "))) (setq tags-str (strcat "[" (substr tags-str 1 (- (strlen tags-str) 2)) "]")))   
    (setq tags-str "Brak"))   

  ;; Funkcja radaru dzialajaca TYLKO na unikalnych punktach  
  (setq run-analysis (lambda (rad / z-hit id-hit has-z has-id)  
    (setq z-hit 0 id-hit 0)  
    (foreach p unique-pts  
      (setq pt (car p) has-z nil has-id nil)  
      (foreach t-i txt-list  
        (setq dists (get-dist-to-txt pt t-i))  
        (if (<= (car dists) rad)  
          (progn (if (and (= (nth 3 t-i) "Z") (not has-z)) (setq has-z T z-hit (1+ z-hit)))  
                 (if (and (= (nth 3 t-i) "ID") (not has-id)) (setq has-id T id-hit (1+ id-hit))))))  
    )  
    (set_tile "rep_z" (strcat "Dopasowano rzednych: " (itoa z-hit) " / " (itoa (length unique-pts))))  
    (set_tile "rep_id" (strcat "Dopasowano numerow: " (itoa id-hit) " / " (itoa (length unique-pts))))  
  ))  

  ;; 2. DYNAMICZNE OKNO DCL  
  (setq dcl-file (vl-filename-mktemp "geo10.dcl") dcl-fn (open dcl-file "w"))    
  (write-line "Geo10 : dialog { label = \"Eksport V10 (Pelna Kontrola)\";" dcl-fn)    
  (write-line "  : boxed_column { label = \"Analiza Przestrzenna (WCS)\";" dcl-fn)    
  (write-line (strcat "    : text { label = \"Rozpoznany Uklad: " detected-sys "\"; }") dcl-fn)   
  (if sys-warn (write-line "    : text { label = \"UWAGA: Wykryto rozbieznosci wspolrzednych!\"; }" dcl-fn))   
  (write-line "  }" dcl-fn)   

  (write-line "  : row { " dcl-fn)  
  (write-line "  : boxed_column { label = \"Wykryte Obiekty\";" dcl-fn)    
  (if (> (+ c-pts c-blks) 0) (write-line (strcat "    : text { label = \"- Punkty/Bloki: " (itoa (+ c-pts c-blks)) "\"; }") dcl-fn))   
  (if (> c-lines 0) (write-line (strcat "    : text { label = \"- Linie: " (itoa c-lines) "\"; }") dcl-fn))   
  (if (> c-arcs 0) (write-line (strcat "    : text { label = \"- Luki: " (itoa c-arcs) "\"; }") dcl-fn))   

  ;; Raport liczby okręgów
  (if (> c-circles 0) (write-line (strcat "    : text { label = \"- Okregi: " (itoa c-circles) "\"; }") dcl-fn))   

  (if (> c-solids 0) (write-line (strcat "    : text { label = \"- Bryly SOLID: " (itoa c-solids) "\"; }") dcl-fn))   
  (write-line "  }" dcl-fn)   

  (write-line "  : boxed_column { label = \"Teksty w strefie\";" dcl-fn)  
  (write-line (strcat "    : text { label = \"- Liczby (Rzedne): " (itoa c-txt-z) "\"; }") dcl-fn)  
  (write-line (strcat "    : text { label = \"- Opisy (Numery): " (itoa c-txt-id) "\"; }") dcl-fn)  
  (write-line "  } }" dcl-fn)  

  (write-line "  : boxed_column { label = \"Raport Dopasowania (Na Zywo)\";" dcl-fn)  
  (write-line "    : text { key = \"rep_z\"; value=\"...\"; }" dcl-fn)  
  (write-line "    : text { key = \"rep_id\"; value=\"...\"; }" dcl-fn)  
  (write-line "  }" dcl-fn)  

  (if (> c-solids 0)  
    (progn (write-line "  : boxed_radio_row { label = \"Eksport Bryl SOLID\"; key = \"s_m\";" dcl-fn)   
           (write-line "    : radio_button { key = \"1\"; label = \"Tylko P1 (Insertion)\"; value = \"1\"; }" dcl-fn)   
           (write-line "    : radio_button { key = \"4\"; label = \"Wszystkie (P1-P4)\"; } }" dcl-fn)))  

  (write-line (strcat "  : boxed_column { label = \"Duplikaty Geometrii (Wstepnie znaleziono ok. " (itoa dupes) ")\";") dcl-fn)   
  (write-line "    : radio_row { key = \"d_m\";" dcl-fn)   
  (write-line "      : radio_button { key = \"rem\"; label = \"Usun punkty w promieniu ->\"; value = \"1\"; }" dcl-fn)   
  (write-line "      : radio_button { key = \"keep\"; label = \"Eksportuj wszystkie\"; } " dcl-fn) 
  (write-line "    }" dcl-fn)   
  (write-line "    : row { : edit_box { key = \"d_tol\"; label = \"Promien szukania (tolerancja) [m]:\"; edit_width = 8; value = \"0.01\"; } }" dcl-fn)   
  (write-line "  }" dcl-fn)   

  (write-line "  : boxed_column { label = \"Zarzadzanie Numeracja i Konfliktami ID\";" dcl-fn) 
  (write-line "    : toggle { key = \"renum_all\"; label = \"Wymus NOWA numeracje dla WSZYSTKICH pikiet\"; value = \"0\"; }" dcl-fn) 
  (write-line "    : toggle { key = \"fix_dupes\"; label = \"Automatycznie rozwiazuj konflikty (zmieniaj zduplikowane numery)\"; value = \"1\"; }" dcl-fn) 
  (write-line "    : text { label = \"(Puste numery nadal otrzymaja identyfikator z prefiksem z ponizszego pola)\"; }" dcl-fn) 
  (write-line "    : row { : edit_box { key = \"a_p\"; label = \"Prefiks auto:\"; edit_width = 8; value = \"P_\"; } : edit_box { key = \"a_s\"; label = \"Zacznij od nr:\"; edit_width = 8; value = \"1\"; } }" dcl-fn) 
  (write-line "  }" dcl-fn) 

  (write-line "  : boxed_column { label = \"Konfiguracja Radaru i Atrybutow\";" dcl-fn)    
  (write-line (strcat "    : text { label = \"Znalezione Tagi blokow: " tags-str "\"; }") dcl-fn)   
  (write-line "    : row { : edit_box { key = \"b_t\"; label = \"Tag NR:\"; edit_width = 6; value = \"NR, ID\"; } : edit_box { key = \"z_t\"; label = \"Tag Z:\"; edit_width = 6; value = \"H, Z\"; } }" dcl-fn)   
  (write-line "    : row { : edit_box { key = \"t_r\"; label = \"Zasieg radaru tekstow [m]:\"; edit_width = 5; value = \"1.5\"; } : button { key = \"recalc\"; label = \"Odswiez Raport\"; } }" dcl-fn)   
  (write-line "  }" dcl-fn)   

  ;; Format eksportu:
  ;; - TXT: zachowuje dotychczasowy eksport pikiet z numerem,
  ;; - PTS: prosta chmura punktow bez numerow, z liczba punktow w pierwszej linii.
  (write-line "  : boxed_radio_row { label = \"Format eksportu\"; key = \"out_fmt\"; : radio_button { key = \"txt\"; label = \"TXT pikiety\"; value=\"1\";} : radio_button { key = \"pts\"; label = \"PTS chmura punktow\"; } }" dcl-fn)

  ;; Offset Z dziala tylko na eksportowany plik TXT/PTS.
  ;; Nie modyfikuje punktow, blokow ani tekstow w DWG.
  (write-line "  : boxed_column { label = \"Modyfikacja Z przy eksporcie\";" dcl-fn)
  (write-line "    : row { : edit_box { key = \"z_off\"; label = \"Offset Z [m]:\"; edit_width = 8; value = \"0.000\"; } }" dcl-fn)
  (write-line "    : text { label = \"Np. -0.800 obnizy kazda eksportowana rzedna o 0.8 m.\"; }" dcl-fn)
  (write-line "  }" dcl-fn)

  (write-line "  : boxed_radio_row { label = \"Kolejnosc Kolumn\"; key = \"g_m\"; : radio_button { key = \"geo\"; label = \"Geodezja (N,E,H)\"; value=\"1\";} : radio_button { key = \"cad\"; label = \"CAD (E,N,H)\"; } }" dcl-fn)   
  (write-line "  ok_cancel; }" dcl-fn) (close dcl-fn)    

  (setq dcl-id (load_dialog dcl-file)) (new_dialog "Geo10" dcl-id)   

  (run-analysis 1.5)  

  (action_tile "recalc" "(run-analysis (atof (get_tile \"t_r\")))")  
  (action_tile "accept" "(setq geo_mode (get_tile \"g_m\") dupe_mode (get_tile \"d_m\") d_tol_str (get_tile \"d_tol\") solid_mode (if (get_tile \"s_m\") (get_tile \"s_m\") \"1\") auto_pref (get_tile \"a_p\") auto_start_str (get_tile \"a_s\") renum_all (get_tile \"renum_all\") fix_dupes (get_tile \"fix_dupes\") blk_tag (get_tile \"b_t\") z_tag (get_tile \"z_t\") txt_rad (atof (get_tile \"t_r\")) z_offset_str (get_tile \"z_off\") export_format (get_tile \"out_fmt\")) (done_dialog 1)")    

  (setq status (start_dialog)) (unload_dialog dcl-id) (vl-file-delete dcl-file)    
  (if (= status 0) (exit))   

  ;; Przygotowanie zmiennych DCL 
  (setq d_tol (atof d_tol_str)) 
  (setq auto_start (atoi auto_start_str)) 
  (if (= auto_start 0) (setq auto_start 1)) 

  (if (not export_format)
    (setq export_format "txt")
  )

  ;; Offset Z jest transformacja tylko na wyjsciu eksportu.
  ;; Wpisy przyjmujemy z kropka albo przecinkiem.
  (setq z_offset (distof (vl-string-translate "," "." z_offset_str)))
  (if (not z_offset) (setq z_offset 0.0))

  (setq filename (get-native-windows-save-file export_format)) (if (not filename) (exit))   
  (setq b-tags (mapcar 'strcase (parse-tags blk_tag)) z-tags (mapcar 'strcase (parse-tags z_tag)))   

  ;; 3. FINALNE WYCIĄGANIE  
  (setq final-pts '() i 0)  
  (while (< i (sslength ss))   
    (setq ent (ssname ss i) obj (vlax-ename->vla-object ent) type (vla-get-ObjectName obj))  
    (cond  
      ((member type '("AcDbPoint" "AcDbBlockReference")) (setq final-pts (cons (list (car (extract-v22 obj "1")) obj) final-pts)))  

      ;; OKRĄG - finalnie eksportowany jest tylko środek
      ((= type "AcDbCircle") (setq final-pts (cons (list (car (extract-v22 obj "1")) obj) final-pts)))  

      ((= type "AcDbSolid") (foreach p (extract-v22 obj solid_mode) (setq final-pts (cons (list p obj) final-pts))))  
      ((member type '("AcDbLine" "AcDbPolyline" "AcDb2dPolyline" "AcDb3dPolyline" "AcDbArc"))  
       (foreach p (extract-v22 obj "1") (setq final-pts (cons (list p obj) final-pts))))  
    )  
    (setq i (1+ i))  
  )  

  ;; 4. ZAPIS Z INTELIGENTNYM ROZWIĄZYWANIEM KONFLIKTÓW 
  ;; Linie zbieramy najpierw w pamieci, bo format PTS wymaga liczby punktow
  ;; w pierwszej linii pliku.
  (setq count-exp 0 accepted-pts '() used-ids '() export-lines '())   

  (foreach item (reverse final-pts)   
    (setq pt (car item) obj (cadr item) x (car pt) y (cadr pt) z (caddr pt) nr "" f-z nil is-dupe nil)   

    ;; KONTROLA DUPLIKATÓW GEOMETRII (Z tolerancją użytkownika) 
    (if (= dupe_mode "rem")  
      (if (vl-some '(lambda (p) (< (dist-2d pt p) d_tol)) accepted-pts) 
        (setq is-dupe T) 
      ) 
    ) 

    (if (not is-dupe)  
      (progn  
        (setq accepted-pts (cons pt accepted-pts))  
        (if (not z) (setq z 0.0))   

        ;; POBIERANIE DANYCH Z BLOKU 
        (if (= (vla-get-ObjectName obj) "AcDbBlockReference")   
          (foreach att (vlax-invoke obj 'GetAttributes)   
            (setq tstr (strcase (vla-get-TagString att)))   
            (if (and (= renum_all "0") (member tstr b-tags)) (setq nr (vla-get-TextString att)))   
            (if (member tstr z-tags) (setq z (atof (vl-string-translate "," "." (vla-get-TextString att))) f-z T))))   

        ;; POBIERANIE DANYCH Z RADARU 
        (setq m-z 9999.0 m-id 9999.0 c-z nil c-id "")   
        (foreach t-i txt-list   
          (setq dists (get-dist-to-txt pt t-i) d-edge (car dists) d-center (cadr dists))   
          (if (<= d-edge txt_rad)   
            (progn  
              ;; POBIERZ Z Z TEKSTU TYLKO JEŚLI PUNKT JEST "PŁASKI" (Z = 0)
              (if (and (= (nth 3 t-i) "Z") (not f-z) (< d-center m-z) (<= (abs z) 0.001))   
                  (setq m-z d-center c-z (distof (vl-string-trim " mM\r\n\t" (vl-string-translate "," "." (nth 2 t-i))))))  

              ;; POBIERZ ID Z TEKSTU
              (if (and (= renum_all "0") (= (nth 3 t-i) "ID") (= nr "") (< d-center m-id))   
                  (setq m-id d-center c-id (nth 2 t-i)))  
            )  
          )  
        )   

        ;; Zastosuj wyłapane Z z tekstu
        (if c-z (setq z c-z))  

        ;; Offset Z stosujemy dopiero po wszystkich metodach odczytu rzednej.
        ;; Zmieniamy tylko wartosc zapisywana do TXT, nie geometrie w rysunku.
        (setq z (+ z z_offset))

        (if (= nr "") (setq nr c-id)) 

        ;; KONTROLA ID (Brak numeru, wymuszenie nowej numeracji lub konflikt) 
        (setq needs_new_id nil) 
        (if (= nr "") (setq needs_new_id T)) 
        (if (= renum_all "1") (setq needs_new_id T)) 
        (if (and (= fix_dupes "1") (member nr used-ids)) (setq needs_new_id T)) 

        (if needs_new_id 
          (progn 
            (setq nr (strcat auto_pref (itoa auto_start)) auto_start (1+ auto_start)) 
            ;; Pętla upewniająca się, że nowy numer nie koliduje 
            (while (member nr used-ids) 
              (setq nr (strcat auto_pref (itoa auto_start)) auto_start (1+ auto_start)) 
            ) 
          ) 
        ) 
        (setq used-ids (cons nr used-ids)) ; Zapisanie wykorzystanego numeru do pamięci 

        (if (= export_format "pts")
          ;; PTS: prosta chmura punktow bez numeru.
          ;; Kolejnosc wspolrzednych nadal respektuje wybor Geodezja/CAD.
          (if (= geo_mode "geo")
            (setq export-line (strcat (format-coord y) " " (format-coord x) " " (format-coord z)))
            (setq export-line (strcat (format-coord x) " " (format-coord y) " " (format-coord z)))
          )

          ;; TXT: dotychczasowy eksport pikiet z numerem.
          (if (= geo_mode "geo")
            (setq export-line (strcat nr " " (format-coord y) " " (format-coord x) " " (format-coord z)))
            (setq export-line (strcat nr " " (format-coord x) " " (format-coord y) " " (format-coord z)))
          )
        )

        (setq export-lines (cons export-line export-lines))
        (setq count-exp (1+ count-exp))   
      )  
    )  
  )   

  (setq export-lines (reverse export-lines))
  (setq f (open filename "w"))

  (if (= export_format "pts")
    (write-line (itoa count-exp) f)
  )

  (foreach export-line export-lines
    (write-line export-line f)
  )

  (close f)   
  (setq f nil)  
  (alert
    (strcat
      "Sukces!\nZapisano: "
      (itoa count-exp)
      (if (= export_format "pts")
        " punktow PTS."
        " pikiet TXT."
      )
      "\nUkład: "
      detected-sys
      "\nFormat eksportu: "
      (strcase export_format)
      "\nOffset Z eksportu: "
      (rtos z_offset 2 3)
      " m"
    )
  )   
  (princ)   
)   

(princ "\nKomenda: EKSPORT_PIKIET_V22") (princ)