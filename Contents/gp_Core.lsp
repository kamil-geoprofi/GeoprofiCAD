(vl-load-com)

;; ======================================================
;; POMOCNICZE: PARSOWANIE TAGÓW I KONFIGURACJA
;; ======================================================

(defun geocad-parse-tags (str / res tmp)
  (setq res '() tmp "")
  (if (and str (/= str ""))
    (foreach ch (vl-string->list (strcase str))
      (if (member ch '(44 59 32 9)) 
        (if (/= tmp "") (progn (setq res (cons tmp res)) (setq tmp "")))
        (setq tmp (strcat tmp (chr ch)))
      )
    )
  )
  (if (/= tmp "") (setq res (cons tmp res)))
  (reverse res)
)

(defun geocad-get-cfg (klucz domyslny / val)
  (setq val (vl-registry-read "HKEY_CURRENT_USER\\Software\\GeoCadSkrypty" klucz))
  (if (not val) (progn (vl-registry-write "HKEY_CURRENT_USER\\Software\\GeoCadSkrypty" klucz domyslny) domyslny) val)
)

;; ======================================================
;; MÓZG: GENERATOR NUMERACJI I RADAR
;; ======================================================

(defun GP:PobierzNastepnyNumer ( / pref nr max-draw ss i obj att-val)
  (setq pref (geocad-get-cfg "PiktPrefix" ""))
  
  ;; 1. Pobierz numer z pamięci LDATA
  (setq nr (vlax-ldata-get "GeoLicznik" pref))
  (if (not nr) (setq nr 1))

  ;; 2. SZYBKI SKAN RYSUNKU: Szukamy najwyższego istniejącego numeru z tym przedrostkiem
  (setq max-draw 0)
  (if (setq ss (ssget "_X" (list '(0 . "INSERT") '(2 . "Pikieta_Geo"))))
    (repeat (setq i (sslength ss))
      (setq obj (vlax-ename->vla-object (ssname ss (setq i (1- i)))))
      (foreach att (vlax-invoke obj 'GetAttributes)
        (if (= (vla-get-TagString att) "NR")
          (progn
            (setq att-val (vla-get-TextString att))
            ;; Jeśli numer zawiera nasz przedrostek, wycinamy go i sprawdzamy cyfrę
            (if (vl-string-search pref att-val)
              (setq max-draw (max max-draw (atoi (vl-string-subst "" pref att-val))))
            )
          )
        )
      )
    )
  )

  ;; 3. Synchronizacja: weź większą wartość
  (if (> (1+ max-draw) nr) 
      (setq nr (1+ max-draw))
      ;; opcjonalnie: jeśli chcesz automatycznie "cofać" licznik po usunięciu:
      (if (< (1+ max-draw) nr) (setq nr (1+ max-draw)))
  )

  ;; 4. Zapis i zwrot
  (vlax-ldata-put "GeoLicznik" pref (1+ nr))
  (itoa nr)
)

(defun get-pt-from-obj (obj / type pt-list val z-tags)   
  (setq type (vla-get-ObjectName obj))
  (setq z-tags (geocad-parse-tags (geocad-get-cfg "ZTags" "H,Z,RZEDNA")))
  
  (cond   
    ((= type "AcDbPoint")   
     (setq pt-list (vlax-safearray->list (vlax-variant-value (vla-get-Coordinates obj))))  
    )  
    ((member type '("AcDbBlockReference" "AcDbText" "AcDbMText"))  
     (setq pt-list (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint obj))))  
     (if (= type "AcDbBlockReference")    
       (foreach att (vlax-invoke obj 'GetAttributes)    
         (if (member (strcase (vla-get-TagString att)) z-tags)  
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

;; ======================================================
;; FABRYKA: WSTAWIANIE PIKIET
;; ======================================================

(defun geocad-stworz-blok-pikieta ()  
  (if (not (tblsearch "BLOCK" "Pikieta_Geo"))  
    (progn  
      (entmake (list '(0 . "BLOCK") '(2 . "Pikieta_Geo") '(70 . 2) '(10 0.0 0.0 0.0) (cons 8 "0")))  
      (entmake (list '(0 . "POINT") '(10 0.0 0.0 0.0) (cons 8 "0")))  
      (entmake (list '(0 . "ATTDEF") '(10 1.0 0.5 0.0) '(1 . "---") '(2 . "NR") '(3 . "Nr") (cons 40 1.0) '(70 . 0) (cons 8 "0")))  
      (entmake (list '(0 . "ATTDEF") '(10 1.0 -0.5 0.0) '(1 . "0.00") '(2 . "H") '(3 . "H") (cons 40 1.0) '(70 . 0) (cons 8 "0")))  
      (entmake (list '(0 . "ENDBLK") (cons 8 "0")))  
    )  
  )  
  (princ)  
)

(defun geocad-wstaw-pikiete-full (doc space pt-list nr-str show-z / txt-h z-prec prefix kolor styl display pikt_pref pelny-nr px py pz z-str dX dY lay-obj lay-pt lay-nr lay-h pt-3d blkRef vis-nr vis-h) 
  
  ;; AUTO-NUMERACJA: Jeśli pusty, prosimy licznik o nowy
  (if (or (not nr-str) (= nr-str ""))
    (setq nr-str (GP:PobierzNastepnyNumer))
  )

  (setq txt-h  (atof (geocad-get-cfg "TxtH" "1.0")) 
        z-prec (atoi (geocad-get-cfg "Prec" "2")) 
        prefix (geocad-get-cfg "Prefix" "POMIAR") 
        kolor  (atoi (geocad-get-cfg "Color" "3")) 
        styl   (geocad-get-cfg "Styl" "Blok") 
        display (geocad-get-cfg "Display" "Oba")
        pikt_pref (geocad-get-cfg "PiktPrefix" ""))

  (setq pelny-nr (strcat pikt_pref nr-str))
  (setq vis-nr (if (member display '("Oba" "Numer")) :vlax-false :vlax-true)) 
  (setq vis-h  (if (member display '("Oba" "Rzedna")) :vlax-false :vlax-true)) 

  (setq px (car pt-list) py (cadr pt-list) pz (caddr pt-list)) 
  (setq dX (* txt-h 1.2) dY (* txt-h 0.7)) 
  (setq pt-3d (vlax-3d-point pt-list)) 
  (setq z-str (rtos pz 2 z-prec)) 

  (setq lay-obj (vla-get-Layers doc) lay-pt (strcat prefix "_PIKIETY") lay-nr (strcat prefix "_ETYKIETA_NR") lay-h (strcat prefix "_ETYKIETA_H")) 
  (vla-put-color (vla-add lay-obj lay-pt) kolor)     
  (vla-put-color (vla-add lay-obj lay-nr) kolor)    
  (vla-put-color (vla-add lay-obj lay-h) kolor) 

  (if (= styl "Tekst") 
    (progn 
      (entmakex (list '(0 . "POINT") (cons 10 pt-list) (cons 8 lay-pt))) 
      (if (= vis-nr :vlax-false) (entmakex (list '(0 . "TEXT") (cons 10 (list (+ px dX) (+ py dY) pz)) (cons 40 txt-h) (cons 1 pelny-nr) (cons 8 lay-nr)))) 
      (if (= vis-h :vlax-false) (entmakex (list '(0 . "TEXT") (cons 10 (list (+ px dX) (- py dY) pz)) (cons 40 txt-h) (cons 1 z-str) (cons 8 lay-h)))) 
    ) 
    (progn 
      (geocad-stworz-blok-pikieta) 
      (setq blkRef (vla-InsertBlock space pt-3d "Pikieta_Geo" 1.0 1.0 1.0 0.0)) 
      (vla-put-Layer blkRef lay-pt) 
      (foreach att (vlax-invoke blkRef 'GetAttributes) 
        (vla-put-Height att txt-h) 
        (cond 
          ((= (vla-get-TagString att) "NR") 
            (vla-put-TextString att pelny-nr) 
            (vla-put-InsertionPoint att (vlax-3d-point (list (+ px dX) (+ py dY) pz))) 
            (vla-put-Invisible att vis-nr) 
            (vla-put-Layer att lay-nr) 
          ) 
          ((member (vla-get-TagString att) '("H" "Z" "RZEDNA")) 
            (vla-put-TextString att z-str) 
            (vla-put-InsertionPoint att (vlax-3d-point (list (+ px dX) (- py dY) pz))) 
            (vla-put-Invisible att vis-h) 
            (vla-put-Layer att lay-h) 
          ) 
        ) 
      ) 
    ) 
  ) 
  (princ) 
) 

;; ======================================================
;; SKANER I AKTUALIZATOR ISTNIEJĄCYCH PIKIET
;; ======================================================

(defun geocad-get-existing-prefixes ( / ss i obj lay pref lst) 
  (setq lst '()) 
  (setq ss (ssget "X" '((0 . "INSERT") (2 . "Pikieta_Geo")))) 
  (if ss 
    (progn 
      (setq i 0) 
      (while (< i (sslength ss)) 
        (setq obj (vlax-ename->vla-object (ssname ss i))) 
        (setq lay (vla-get-Layer obj)) 
        (if (vl-string-search "_PIKIETY" lay) 
          (setq pref (substr lay 1 (vl-string-search "_PIKIETY" lay))) 
          (setq pref lay) 
        ) 
        (if (not (member pref lst)) (setq lst (append lst (list pref)))) 
        (setq i (1+ i)) 
      ) 
    ) 
  ) 
  lst 
) 

(defun geocad-update-existing (doc target_prefix kolor-str txt-h-str z-prec-str display / ss i ent obj pt px py pz lay-pt lay-nr lay-h lay-obj kolor txt-h z-prec dX dY vis-nr vis-h) 
  (setq kolor (atoi kolor-str) txt-h (atof txt-h-str) z-prec (atoi z-prec-str)) 
  (setq dX (* txt-h 1.2) dY (* txt-h 0.7)) 
   
  (setq vis-nr (if (member display '("Oba" "Numer")) :vlax-false :vlax-true)) 
  (setq vis-h  (if (member display '("Oba" "Rzedna")) :vlax-false :vlax-true)) 

  (if (= target_prefix "--- WSZYSTKIE W RYSUNKU ---") 
    (setq ss (ssget "X" '((0 . "INSERT") (2 . "Pikieta_Geo")))) 
    (setq ss (ssget "X" (list '(0 . "INSERT") '(2 . "Pikieta_Geo") (cons 8 (strcat target_prefix "_PIKIETY"))))) 
  ) 

  (if ss 
    (progn 
      (vla-StartUndoMark doc) 
      (setq i 0 lay-obj (vla-get-Layers doc)) 
      (while (< i (sslength ss)) 
        (setq ent (ssname ss i) obj (vlax-ename->vla-object ent)) 
        (setq pt (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint obj)))) 
        (setq px (car pt) py (cadr pt) pz (caddr pt)) 
         
        (setq lay-pt (vla-get-Layer obj)) 
        (setq lay-nr (vl-string-subst "_ETYKIETA_NR" "_PIKIETY" lay-pt)) 
        (setq lay-h (vl-string-subst "_ETYKIETA_H" "_PIKIETY" lay-pt)) 

        (vla-put-color (vla-add lay-obj lay-pt) kolor) 
        (vla-put-color (vla-add lay-obj lay-nr) kolor) 
        (vla-put-color (vla-add lay-obj lay-h) kolor) 

        (foreach att (vlax-invoke obj 'GetAttributes) 
          (vla-put-Height att txt-h) 
          (cond 
            ((= (vla-get-TagString att) "NR") 
             (vla-put-InsertionPoint att (vlax-3d-point (list (+ px dX) (+ py dY) pz))) 
             (vla-put-Invisible att vis-nr) 
            ) 
            ((member (vla-get-TagString att) '("H" "Z" "RZEDNA")) 
             (vla-put-TextString att (rtos pz 2 z-prec)) 
             (vla-put-InsertionPoint att (vlax-3d-point (list (+ px dX) (- py dY) pz))) 
             (vla-put-Invisible att vis-h) 
            ) 
          ) 
        ) 
        (setq i (1+ i)) 
      ) 
      (vla-EndUndoMark doc) 
      (princ (strcat "\n[SUKCES] Zaktualizowano " (itoa i) " pikiet dla grupy: " target_prefix)) 
    ) 
    (princ "\n[INFO] Nie znaleziono zadnych blokow do aktualizacji.") 
  ) 
) 

;; ======================================================
;; INTERFEJS: GEO_SETUP
;; ======================================================

(defun c:GEO_SETUP ( / txt-h z-prec prefix pikt_pref z_tags kolor styl display dcl-file dcl-fn dcl-id status col-idx styl-idx disp-idx doc prefix_list target_idx target_prefix) 
   
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))) 
  (setq txt-h (geocad-get-cfg "TxtH" "1.0") 
        z-prec (geocad-get-cfg "Prec" "2") 
        prefix (geocad-get-cfg "Prefix" "POMIAR") 
        pikt_pref (geocad-get-cfg "PiktPrefix" "") 
        z_tags (geocad-get-cfg "ZTags" "H, Z, RZEDNA")
        kolor (geocad-get-cfg "Color" "3") 
        styl (geocad-get-cfg "Styl" "Blok") 
        display (geocad-get-cfg "Display" "Oba")) 

  (setq prefix_list (geocad-get-existing-prefixes)) 
  (setq prefix_list (cons "--- WSZYSTKIE W RYSUNKU ---" prefix_list)) 

  (setq dcl-file (vl-filename-mktemp "geosetup.dcl") dcl-fn (open dcl-file "w")) 
  (write-line "GeoSetup : dialog { label = \"Ustawienia Globalne Pikiet (Mozg)\";" dcl-fn) 
  (write-line "  : boxed_column { label = \"Parametry wizualne (Ogolne)\";" dcl-fn) 
  (write-line "    : popup_list { key = \"styl_rys\"; label = \"Styl na mapie:\"; list = \"Inteligentny Blok\\nZwykly Punkt + Tekst\"; }" dcl-fn) 
  (write-line "    : popup_list { key = \"display_mode\"; label = \"Widocznosc:\"; list = \"Oba (Nr + H)\\nTylko Numer\\nTylko Rzedna (H)\\nNic (Sam symbol)\"; }" dcl-fn) 
  (write-line "    : edit_box { key = \"txt_h\"; label = \"Wysokosc tekstu:\"; edit_width = 8; }" dcl-fn) 
  (write-line "    : edit_box { key = \"z_prec\"; label = \"Miejsca po przecinku (Z):\"; edit_width = 8; }" dcl-fn) 
  (write-line "    : edit_box { key = \"prefix\"; label = \"Przedrostek WARSTWY (np. POMIAR):\"; edit_width = 20; }" dcl-fn) 
  (write-line "    : edit_box { key = \"pikt_pref\"; label = \"Przedrostek NUMERU (np. woda_):\"; edit_width = 20; }" dcl-fn) 
  (write-line "    : edit_box { key = \"z_tags\"; label = \"Tagi rzednych (np. H, Z, WYS):\"; edit_width = 20; }" dcl-fn)
  (write-line "    : popup_list { key = \"kolor\"; label = \"Kolor podstawowy:\"; list = \"1 - Czerwony\\n2 - Zolty\\n3 - Zielony\\n4 - Cyjan\\n5 - Niebieski\\n6 - Magenta\\n7 - Czarny/Bialy\"; }" dcl-fn) 
  (write-line "  }" dcl-fn) 
   
  (write-line "  : boxed_column { label = \"Zarzadzanie istniejacymi blokami\";" dcl-fn) 
  (write-line "    : popup_list { key = \"exist_layers\"; label = \"Wybierz grupe do zmiany:\"; }" dcl-fn) 
  (write-line "    : button { key = \"btn_update\"; label = \"Aktualizuj wybrana grupe powyzszymi parametrami\"; }" dcl-fn) 
  (write-line "  }" dcl-fn) 
  (write-line "  ok_cancel;" dcl-fn) 
  (write-line "}" dcl-fn) 
  (close dcl-fn) 

  (setq dcl-id (load_dialog dcl-file)) 
  (if (not (new_dialog "GeoSetup" dcl-id)) (progn (alert "Blad ladowania okna DCL.") (exit))) 

  (start_list "exist_layers") (mapcar 'add_list prefix_list) (end_list) 

  (set_tile "txt_h" txt-h) (set_tile "z_prec" z-prec) (set_tile "prefix" prefix) 
  (set_tile "pikt_pref" pikt_pref) (set_tile "z_tags" z_tags)
  (setq col-idx (atoi kolor)) (if (and (>= col-idx 1) (<= col-idx 7)) (set_tile "kolor" (itoa (1- col-idx))) (set_tile "kolor" "2")) 
  (if (= styl "Tekst") (set_tile "styl_rys" "1") (set_tile "styl_rys" "0")) 
  (cond ((= display "Oba") (set_tile "display_mode" "0")) ((= display "Numer") (set_tile "display_mode" "1")) ((= display "Rzedna") (set_tile "display_mode" "2")) ((= display "Brak") (set_tile "display_mode" "3"))) 

  (action_tile "btn_update" "(setq txt-h (get_tile \"txt_h\") z-prec (get_tile \"z_prec\") prefix (get_tile \"prefix\") pikt_pref (get_tile \"pikt_pref\") z_tags (get_tile \"z_tags\") kolor (itoa (1+ (atoi (get_tile \"kolor\")))) styl-idx (get_tile \"styl_rys\") disp-idx (get_tile \"display_mode\") target_idx (atoi (get_tile \"exist_layers\"))) (done_dialog 2)") 
  (action_tile "accept" "(setq txt-h (get_tile \"txt_h\") z-prec (get_tile \"z_prec\") prefix (get_tile \"prefix\") pikt_pref (get_tile \"pikt_pref\") z_tags (get_tile \"z_tags\") kolor (itoa (1+ (atoi (get_tile \"kolor\")))) styl-idx (get_tile \"styl_rys\") disp-idx (get_tile \"display_mode\")) (done_dialog 1)") 
  (action_tile "cancel" "(done_dialog 0)") 

  (setq status (start_dialog)) (unload_dialog dcl-id) (vl-file-delete dcl-file) 

  (if (or (= status 1) (= status 2)) 
    (progn 
      (setq styl (if (= styl-idx "1") "Tekst" "Blok")) 
      (cond ((= disp-idx "0") (setq display "Oba")) ((= disp-idx "1") (setq display "Numer")) ((= disp-idx "2") (setq display "Rzedna")) ((= disp-idx "3") (setq display "Brak"))) 
       
      (vl-registry-write "HKEY_CURRENT_USER\\Software\\GeoCadSkrypty" "Styl" styl) 
      (vl-registry-write "HKEY_CURRENT_USER\\Software\\GeoCadSkrypty" "Display" display) 
      (vl-registry-write "HKEY_CURRENT_USER\\Software\\GeoCadSkrypty" "TxtH" txt-h) 
      (vl-registry-write "HKEY_CURRENT_USER\\Software\\GeoCadSkrypty" "Prec" z-prec) 
      (vl-registry-write "HKEY_CURRENT_USER\\Software\\GeoCadSkrypty" "Prefix" prefix) 
      (vl-registry-write "HKEY_CURRENT_USER\\Software\\GeoCadSkrypty" "PiktPrefix" pikt_pref) 
      (vl-registry-write "HKEY_CURRENT_USER\\Software\\GeoCadSkrypty" "ZTags" z_tags) 
      (vl-registry-write "HKEY_CURRENT_USER\\Software\\GeoCadSkrypty" "Color" kolor) 
       
      (if (= status 1) (princ "\n[OK] Zapisano standard dla nowych pikiet.")) 
       
      (if (= status 2) 
        (progn 
          (setq target_prefix (nth target_idx prefix_list)) 
          (geocad-update-existing doc target_prefix kolor txt-h z-prec display) 
        ) 
      ) 
    ) 
    (princ "\n[Anulowano]") 
  ) 
  (princ) 
) 

(princ "\nZaladowano biblioteke: gp_Core.lsp. Wpisz GEO_SETUP aby skonfigurowac.")  
(princ)