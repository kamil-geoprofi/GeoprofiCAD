(vl-load-com)
(load "gp_Config.lsp" "\nBLAD: Nie znaleziono pliku gp_Config.lsp!")

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
  (setq val (vl-registry-read *geocad-registry-path* klucz))

  (if (not val)
    (progn
      (vl-registry-write *geocad-registry-path* klucz domyslny)
      domyslny
    )
    val
  )
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
      (entmake
        (list
          '(0 . "BLOCK")
          '(2 . "Pikieta_Geo")
          '(70 . 2)
          '(10 0.0 0.0 0.0)
          (cons 8 "0")
        )
      )

      (entmake
        (list
          '(0 . "POINT")
          '(10 0.0 0.0 0.0)
          (cons 8 "0")
        )
      )

      (entmake
        (list
          '(0 . "ATTDEF")
          '(10 1.0 0.5 0.0)
          '(1 . "---")
          '(2 . "NR")
          '(3 . "Nr")
          (cons 40 1.0)
          '(70 . 0)
          (cons 8 "0")
        )
      )

      (entmake
        (list
          '(0 . "ATTDEF")
          '(10 1.0 -0.5 0.0)
          '(1 . "0.00")
          '(2 . "H")
          '(3 . "H")
          (cons 40 1.0)
          '(70 . 0)
          (cons 8 "0")
        )
      )

      (entmake
        (list
          '(0 . "ENDBLK")
          (cons 8 "0")
        )
      )
    )
  )

  (princ)
)


(defun geocad-ctx-get (key ctx)
  (cdr (assoc key ctx))
)


(defun geocad-ctx-set (key val ctx / pair)
  (setq pair (assoc key ctx))

  (if pair
    (subst (cons key val) pair ctx)
    (cons (cons key val) ctx)
  )
)


(defun geocad-ensure-layer (doc layname color / layers lay res)
  ;; Tworzy warstwe, jezeli nie istnieje.
  ;; Jezeli istnieje, wlacza ja, odmraza, odblokowuje i ustawia kolor.
  (setq layers (vla-get-Layers doc))

  (setq res
    (vl-catch-all-apply
      'vla-Item
      (list layers layname)
    )
  )

  (if (vl-catch-all-error-p res)
    (setq lay (vla-Add layers layname))
    (setq lay res)
  )

  (vl-catch-all-apply 'vla-put-Color (list lay color))
  (vl-catch-all-apply 'vla-put-LayerOn (list lay :vlax-true))
  (vl-catch-all-apply 'vla-put-Freeze (list lay :vlax-false))
  (vl-catch-all-apply 'vla-put-Lock (list lay :vlax-false))

  layname
)


(defun geocad-pikieta-prepare-context
  (
    doc
    /
    txt-h z-prec prefix kolor styl display pikt-pref
    lay-pt lay-nr lay-h
    vis-nr vis-h dX dY
  )

  ;; Ustawienia czytamy raz na serie pikiet.
  (setq txt-h (atof (geocad-get-cfg "TxtH" "1.0")))
  (setq z-prec (atoi (geocad-get-cfg "Prec" "2")))
  (setq prefix (geocad-trim-string (geocad-get-cfg "Prefix" "POMIAR")))
  (setq kolor (atoi (geocad-get-cfg "Color" "3")))
  (setq styl (geocad-get-cfg "Styl" "Blok"))
  (setq display (geocad-get-cfg "Display" "Oba"))
  (setq pikt-pref (geocad-trim-string (geocad-get-cfg "PiktPrefix" "")))

  (if (= prefix "")
    (setq prefix "POMIAR")
  )

  (setq lay-pt
    (geocad-layer-name prefix *geocad-layer-type-points*)
  )

  (setq lay-nr
    (geocad-layer-name prefix *geocad-layer-type-label-nr*)
  )

  (setq lay-h
    (geocad-layer-name prefix *geocad-layer-type-label-h*)
  )

  ;; Warstwy przygotowujemy raz.
  (geocad-ensure-layer doc lay-pt kolor)
  (geocad-ensure-layer doc lay-nr kolor)
  (geocad-ensure-layer doc lay-h kolor)

  ;; Blok tez przygotowujemy raz, a nie przy kazdej pikiecie.
  (if (= styl "Blok")
    (geocad-stworz-blok-pikieta)
  )

  (setq vis-nr
    (if (member display '("Oba" "Numer"))
      :vlax-false
      :vlax-true
    )
  )

  (setq vis-h
    (if (member display '("Oba" "Rzedna"))
      :vlax-false
      :vlax-true
    )
  )

  (setq dX (* txt-h 1.2))
  (setq dY (* txt-h 0.7))

  (list
    (cons 'txt-h txt-h)
    (cons 'z-prec z-prec)
    (cons 'prefix prefix)
    (cons 'kolor kolor)
    (cons 'styl styl)
    (cons 'display display)
    (cons 'pikt-pref pikt-pref)
    (cons 'lay-pt lay-pt)
    (cons 'lay-nr lay-nr)
    (cons 'lay-h lay-h)
    (cons 'vis-nr vis-nr)
    (cons 'vis-h vis-h)
    (cons 'dX dX)
    (cons 'dY dY)
  )
)


(defun geocad-wstaw-pikiete-with-context
  (
    doc space pt-list nr-str show-z ctx
    /
    txt-h z-prec styl pikt-pref
    lay-pt lay-nr lay-h
    vis-nr vis-h dX dY
    pelny-nr px py pz z-str pt-3d blkRef
  )

  ;; show-z zostaje w sygnaturze dla kompatybilnosci.
  ;; Obecna stara funkcja tez realnie opierala widocznosc H na ustawieniu Display.
  (setq txt-h (geocad-ctx-get 'txt-h ctx))
  (setq z-prec (geocad-ctx-get 'z-prec ctx))
  (setq styl (geocad-ctx-get 'styl ctx))
  (setq pikt-pref (geocad-ctx-get 'pikt-pref ctx))

  (setq lay-pt (geocad-ctx-get 'lay-pt ctx))
  (setq lay-nr (geocad-ctx-get 'lay-nr ctx))
  (setq lay-h (geocad-ctx-get 'lay-h ctx))

  (setq vis-nr (geocad-ctx-get 'vis-nr ctx))
  (setq vis-h (geocad-ctx-get 'vis-h ctx))

  (setq dX (geocad-ctx-get 'dX ctx))
  (setq dY (geocad-ctx-get 'dY ctx))

  (if (not nr-str)
    (setq nr-str "")
  )

  (setq nr-str (vl-princ-to-string nr-str))
  (setq pelny-nr (strcat pikt-pref nr-str))

  (setq px (car pt-list))
  (setq py (cadr pt-list))
  (setq pz (caddr pt-list))

  (if (not pz)
    (setq pz 0.0)
  )

  (setq pt-list (list px py pz))
  (setq pt-3d (vlax-3d-point pt-list))
  (setq z-str (rtos pz 2 z-prec))

  (if (= styl "Tekst")
    (progn
      (entmakex
        (list
          '(0 . "POINT")
          (cons 10 pt-list)
          (cons 8 lay-pt)
        )
      )

      (if (= vis-nr :vlax-false)
        (entmakex
          (list
            '(0 . "TEXT")
            (cons 10 (list (+ px dX) (+ py dY) pz))
            (cons 40 txt-h)
            (cons 1 pelny-nr)
            (cons 8 lay-nr)
          )
        )
      )

      (if (= vis-h :vlax-false)
        (entmakex
          (list
            '(0 . "TEXT")
            (cons 10 (list (+ px dX) (- py dY) pz))
            (cons 40 txt-h)
            (cons 1 z-str)
            (cons 8 lay-h)
          )
        )
      )
    )

    (progn
      (setq blkRef
        (vla-InsertBlock
          space
          pt-3d
          "Pikieta_Geo"
          1.0
          1.0
          1.0
          0.0
        )
      )

      (vla-put-Layer blkRef lay-pt)

      (foreach att (vlax-invoke blkRef 'GetAttributes)
        (vla-put-Height att txt-h)

        (cond
          ((= (vla-get-TagString att) "NR")
            (vla-put-TextString att pelny-nr)
            (vla-put-InsertionPoint
              att
              (vlax-3d-point (list (+ px dX) (+ py dY) pz))
            )
            (vla-put-Invisible att vis-nr)
            (vla-put-Layer att lay-nr)
          )

          ((member (vla-get-TagString att) '("H" "Z" "RZEDNA"))
            (vla-put-TextString att z-str)
            (vla-put-InsertionPoint
              att
              (vlax-3d-point (list (+ px dX) (- py dY) pz))
            )
            (vla-put-Invisible att vis-h)
            (vla-put-Layer att lay-h)
          )
        )
      )
    )
  )

  (princ)
)


(defun geocad-pikieta-batch-start (doc / ctx)
  ;; Start sesji masowego wstawiania pikiet.
  ;; Numer automatyczny pobieramy leniwie dopiero przy pierwszym insercie auto.
  (setq ctx (geocad-pikieta-prepare-context doc))

  (list
    (cons 'ctx ctx)
    (cons 'next-nr nil)
    (cons 'auto-used nil)
  )
)


(defun geocad-pikieta-batch-insert
  (
    batch space pt-list nr-str show-z
    /
    ctx next-nr actual-nr
  )

  (setq ctx (geocad-ctx-get 'ctx batch))

  (if (or (not nr-str) (= nr-str ""))
    (progn
      ;; Pierwszy numer pobieramy tylko raz.
      ;; GP:PobierzNastepnyNumer od razu zapisuje kolejny numer,
      ;; a batch-end na koncu nadpisze go finalnym stanem po calej serii.
      (setq next-nr (geocad-ctx-get 'next-nr batch))

      (if (not next-nr)
        (setq next-nr (atoi (GP:PobierzNastepnyNumer)))
      )

      (setq actual-nr (itoa next-nr))
      (setq next-nr (1+ next-nr))

      (setq batch (geocad-ctx-set 'next-nr next-nr batch))
      (setq batch (geocad-ctx-set 'auto-used T batch))
    )

    (setq actual-nr (vl-princ-to-string nr-str))
  )

  (geocad-wstaw-pikiete-with-context
    nil
    space
    pt-list
    actual-nr
    show-z
    ctx
  )

  batch
)


(defun geocad-pikieta-batch-end (batch / ctx next-nr pikt-pref)
  ;; Zapisuje finalny licznik tylko wtedy, gdy w sesji uzyto auto-numeracji.
  (if (and batch (geocad-ctx-get 'auto-used batch))
    (progn
      (setq ctx (geocad-ctx-get 'ctx batch))
      (setq next-nr (geocad-ctx-get 'next-nr batch))
      (setq pikt-pref (geocad-ctx-get 'pikt-pref ctx))

      (if next-nr
        (vlax-ldata-put "GeoLicznik" pikt-pref next-nr)
      )

      (setq batch (geocad-ctx-set 'auto-used nil batch))
    )
  )

  batch
)


(defun geocad-wstaw-pikiete-full (doc space pt-list nr-str show-z / batch)
  ;; Kompatybilny wrapper dla starych wywolan.
  ;; Dla pojedynczej pikiety zachowuje stare API.
  ;; Dla nowych komend masowych lepiej uzywac batch-start/insert/end.
  (setq batch (geocad-pikieta-batch-start doc))
  (setq batch
    (geocad-pikieta-batch-insert
      batch
      space
      pt-list
      nr-str
      show-z
    )
  )
  (setq batch (geocad-pikieta-batch-end batch))

  (princ)
)

;; ======================================================
;; SKANER I AKTUALIZATOR ISTNIEJĄCYCH PIKIET
;; ======================================================

(defun geocad-get-existing-prefixes ( / ss i obj lay pref lst layers lay-obj)
  (setq lst '())

  ;; ------------------------------------------------------
  ;; 1. Stara logika: skan blokow Pikieta_Geo.
  ;; Zachowujemy to, bo aktualizator grup historycznie bazowal na blokach.
  ;; ------------------------------------------------------
  (setq ss (ssget "X" '((0 . "INSERT") (2 . "Pikieta_Geo"))))

  (if ss
    (progn
      (setq i 0)

      (while (< i (sslength ss))
        (setq obj (vlax-ename->vla-object (ssname ss i)))
        (setq lay (vla-get-Layer obj))

        (setq pref (geocad-managed-layer-prefix-from-name lay))

        ;; Fallback dla starych / nietypowych rysunkow:
        ;; jezeli blok Pikieta_Geo siedzi na warstwie bez standardowego sufiksu,
        ;; zachowujemy stare zachowanie i traktujemy nazwe warstwy jako grupe.
        (if (not pref)
          (setq pref lay)
        )

        (setq lst (geocad-add-unique-string pref lst))
        (setq i (1+ i))
      )
    )
  )

  ;; ------------------------------------------------------
  ;; 2. Nowa logika: skan tabeli warstw.
  ;; Dzieki temu lista wykryje tez grupy, ktore maja warstwy,
  ;; ale aktualnie nie maja blokow Pikieta_Geo.
  ;; ------------------------------------------------------
  (setq layers (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object))))

  (vlax-for lay-obj layers
    (setq lay (vla-get-Name lay-obj))
    (setq pref (geocad-managed-layer-prefix-from-name lay))

    (if pref
      (setq lst (geocad-add-unique-string pref lst))
    )
  )

  (vl-sort lst '<)
)

(defun geocad-layer-object-if-exists (layers layname / res)
  (if (and layers layname (/= layname ""))
    (progn
      (setq res
        (vl-catch-all-apply
          'vla-Item
          (list layers layname)
        )
      )

      (if (vl-catch-all-error-p res)
        nil
        res
      )
    )
    nil
  )
)


(defun geocad-update-layer-color-if-exists (layers layname kolor / lay)
  (setq lay (geocad-layer-object-if-exists layers layname))

  (if lay
    (vl-catch-all-apply
      'vla-put-Color
      (list lay kolor)
    )
  )
)


(defun geocad-update-managed-layer-colors (doc prefix kolor / layers)
  ;; Aktualizuje kolor wszystkich istniejacych warstw danej grupy.
  ;; Nie tworzy pustych warstw, jezeli ich nie ma.
  (if (and doc prefix (/= prefix ""))
    (progn
      (setq layers (vla-get-Layers doc))

      (geocad-update-layer-color-if-exists
        layers
        (geocad-layer-name prefix *geocad-layer-type-points*)
        kolor
      )

      (geocad-update-layer-color-if-exists
        layers
        (geocad-layer-name prefix *geocad-layer-type-label-nr*)
        kolor
      )

      (geocad-update-layer-color-if-exists
        layers
        (geocad-layer-name prefix *geocad-layer-type-label-h*)
        kolor
      )

      (geocad-update-layer-color-if-exists
        layers
        (geocad-layer-name prefix *geocad-layer-type-polyline-multi*)
        kolor
      )
    )
  )
)

(defun geocad-update-existing
  (
    doc target_prefix kolor-str txt-h-str z-prec-str display
    /
    ss i ent obj pt px py pz
    lay-pt lay-nr lay-h lay-obj
    kolor txt-h z-prec dX dY vis-nr vis-h
    pref
  )

  (setq kolor (atoi kolor-str)
        txt-h (atof txt-h-str)
        z-prec (atoi z-prec-str)
  )

  (setq dX (* txt-h 1.2)
        dY (* txt-h 0.7)
  )

  (setq vis-nr
    (if (member display '("Oba" "Numer"))
      :vlax-false
      :vlax-true
    )
  )

  (setq vis-h
    (if (member display '("Oba" "Rzedna"))
      :vlax-false
      :vlax-true
    )
  )

  ;; ------------------------------------------------------
  ;; Aktualizacja kolorow wszystkich istniejacych warstw grupy,
  ;; w tym warstwy polilinii z NIWELACJA_MULTI.
  ;; Dziala nawet wtedy, gdy grupa ma tylko polilinie i nie ma blokow.
  ;; ------------------------------------------------------
  (if (= target_prefix "--- WSZYSTKIE W RYSUNKU ---")
    (foreach pref (geocad-get-existing-prefixes)
      (geocad-update-managed-layer-colors doc pref kolor)
    )
    (geocad-update-managed-layer-colors doc target_prefix kolor)
  )

  ;; ------------------------------------------------------
  ;; Wybór bloków do aktualizacji
  ;; ------------------------------------------------------
  (if (= target_prefix "--- WSZYSTKIE W RYSUNKU ---")
    (setq ss
      (ssget "X" '((0 . "INSERT") (2 . "Pikieta_Geo")))
    )
    (setq ss
      (ssget
        "X"
        (list
          '(0 . "INSERT")
          '(2 . "Pikieta_Geo")
          (cons
            8
            (geocad-layer-name target_prefix *geocad-layer-type-points*)
          )
        )
      )
    )
  )

  (if ss
    (progn
      (vla-StartUndoMark doc)

      (setq i 0)
      (setq lay-obj (vla-get-Layers doc))

      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))

        (setq pt
          (vlax-safearray->list
            (vlax-variant-value (vla-get-InsertionPoint obj))
          )
        )

        (setq px (car pt)
              py (cadr pt)
              pz (caddr pt)
        )

        ;; ------------------------------------------------------
        ;; Ustalenie grupy/prefiksu z aktualnej warstwy bloku.
        ;; Dla standardowych warstw:
        ;; POMIAR_PIKIETY -> POMIAR
        ;; ------------------------------------------------------
        (setq lay-pt (vla-get-Layer obj))
        (setq pref (geocad-managed-layer-prefix-from-name lay-pt))

        ;; Fallback:
        ;; - przy aktualizacji konkretnej grupy uzywamy target_prefix,
        ;; - przy "wszystkie" zostawiamy oryginalna warstwe punktu.
        (if pref
          (progn
            (setq lay-pt (geocad-layer-name pref *geocad-layer-type-points*))
            (setq lay-nr (geocad-layer-name pref *geocad-layer-type-label-nr*))
            (setq lay-h  (geocad-layer-name pref *geocad-layer-type-label-h*))
          )
          (progn
            (if (= target_prefix "--- WSZYSTKIE W RYSUNKU ---")
              (progn
                (setq lay-pt (vla-get-Layer obj))
                (setq lay-nr lay-pt)
                (setq lay-h lay-pt)
              )
              (progn
                (setq lay-pt (geocad-layer-name target_prefix *geocad-layer-type-points*))
                (setq lay-nr (geocad-layer-name target_prefix *geocad-layer-type-label-nr*))
                (setq lay-h  (geocad-layer-name target_prefix *geocad-layer-type-label-h*))
              )
            )
          )
        )

        ;; Upewniamy sie, ze warstwy istnieja i maja aktualny kolor.
        (vla-put-color (vla-add lay-obj lay-pt) kolor)
        (vla-put-color (vla-add lay-obj lay-nr) kolor)
        (vla-put-color (vla-add lay-obj lay-h) kolor)

        ;; Blok zostaje na warstwie punktow swojej grupy.
        (vla-put-Layer obj lay-pt)

        (foreach att (vlax-invoke obj 'GetAttributes)
          (vla-put-Height att txt-h)

          (cond
            ((= (vla-get-TagString att) "NR")
              (vla-put-InsertionPoint
                att
                (vlax-3d-point (list (+ px dX) (+ py dY) pz))
              )
              (vla-put-Invisible att vis-nr)
              (vla-put-Layer att lay-nr)
            )

            ((member (vla-get-TagString att) '("H" "Z" "RZEDNA"))
              (vla-put-TextString att (rtos pz 2 z-prec))
              (vla-put-InsertionPoint
                att
                (vlax-3d-point (list (+ px dX) (- py dY) pz))
              )
              (vla-put-Invisible att vis-h)
              (vla-put-Layer att lay-h)
            )
          )
        )

        (setq i (1+ i))
      )

      (vla-EndUndoMark doc)

      (princ
        (strcat
          "\n[SUKCES] Zaktualizowano "
          (itoa i)
          " pikiet dla grupy: "
          target_prefix
        )
      )
    )
    (princ "\n[INFO] Nie znaleziono blokow do aktualizacji. Zaktualizowano kolor istniejacych warstw grupy, jezeli istnialy.")
  )
)

;; ======================================================
;; INTERFEJS: GEO_SETUP
;; ======================================================

(defun c:GEO_SETUP
  (
    /
    txt-h z-prec prefix pikt_pref z_tags kolor styl display
    dcl-file dcl-fn dcl-id status
    col-idx styl-idx disp-idx
    doc
    prefix_groups
    prefix_list target_idx target_prefix
    prefix_select_list prefix_select_idx
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))) 
  (setq txt-h (geocad-get-cfg "TxtH" "1.0") 
        z-prec (geocad-get-cfg "Prec" "2") 
        prefix (geocad-get-cfg "Prefix" "POMIAR") 
        pikt_pref (geocad-get-cfg "PiktPrefix" "") 
        z_tags (geocad-get-cfg "ZTags" "H, Z, RZEDNA")
        kolor (geocad-get-cfg "Color" "3") 
        styl (geocad-get-cfg "Styl" "Blok") 
        display (geocad-get-cfg "Display" "Oba")) 

  (setq prefix (geocad-trim-string prefix))

  (if (= prefix "")
    (setq prefix "POMIAR")
  )

  ;; Lista realnych grup z rysunku + aktualnie zapisany prefix.
  (setq prefix_groups (geocad-get-existing-prefixes))

  ;; Aktualnie zapisany Prefix tez pokazujemy jako grupe,
  ;; nawet jezeli jeszcze nie ma dla niego warstw ani blokow.
  (setq prefix_groups (geocad-add-unique-string prefix prefix_groups))

  ;; Lista do wyboru aktywnego prefixu dla nowych pikiet.
  ;; Pierwsza pozycja zostawia mozliwosc recznego wpisania nowej grupy.
  (setq prefix_select_list
    (cons "--- wpisz recznie / nowa grupa ---" prefix_groups)
  )

  ;; Lista do aktualizacji istniejacych grup.
  (setq prefix_list
    (cons "--- WSZYSTKIE W RYSUNKU ---" prefix_groups)
  )

  (setq dcl-file (vl-filename-mktemp "geosetup.dcl") dcl-fn (open dcl-file "w")) 
  (write-line "GeoSetup : dialog { label = \"Ustawienia Globalne Pikiet (Mozg)\";" dcl-fn) 
  (write-line "  : boxed_column { label = \"Parametry wizualne (Ogolne)\";" dcl-fn) 
  (write-line "    : popup_list { key = \"styl_rys\"; label = \"Styl na mapie:\"; list = \"Inteligentny Blok\\nZwykly Punkt + Tekst\"; }" dcl-fn) 
  (write-line "    : popup_list { key = \"display_mode\"; label = \"Widocznosc:\"; list = \"Oba (Nr + H)\\nTylko Numer\\nTylko Rzedna (H)\\nNic (Sam symbol)\"; }" dcl-fn) 
  (write-line "    : edit_box { key = \"txt_h\"; label = \"Wysokosc tekstu:\"; edit_width = 8; }" dcl-fn) 
  (write-line "    : edit_box { key = \"z_prec\"; label = \"Miejsca po przecinku (Z):\"; edit_width = 8; }" dcl-fn) 
  (write-line "    : popup_list { key = \"prefix_select\"; label = \"Istniejaca grupa warstw:\"; }" dcl-fn)
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

  (start_list "prefix_select")
  (mapcar 'add_list prefix_select_list)
  (end_list)

  (start_list "exist_layers")
  (mapcar 'add_list prefix_list)
  (end_list)

  (set_tile "txt_h" txt-h)
  (set_tile "z_prec" z-prec)
  (set_tile "prefix" prefix)
  (set_tile "pikt_pref" pikt_pref)
  (set_tile "z_tags" z_tags)

  ;; Ustaw popup wyboru grupy na aktualny Prefix.
  ;; Jezeli Prefix nie istnieje na liscie, zostaje pozycja 0.
  (setq prefix_select_idx 0)

  (if (member prefix prefix_select_list)
    (progn
      (while
        (and
          (< prefix_select_idx (length prefix_select_list))
          (/= (nth prefix_select_idx prefix_select_list) prefix)
        )
        (setq prefix_select_idx (1+ prefix_select_idx))
      )
    )
  )

  (set_tile "prefix_select" (itoa prefix_select_idx))

  (setq col-idx (atoi kolor))
  (if (and (>= col-idx 1) (<= col-idx 7))
    (set_tile "kolor" (itoa (1- col-idx)))
    (set_tile "kolor" "2")
  )

  (if (= styl "Tekst")
    (set_tile "styl_rys" "1")
    (set_tile "styl_rys" "0")
  )

  (cond
    ((= display "Oba") (set_tile "display_mode" "0"))
    ((= display "Numer") (set_tile "display_mode" "1"))
    ((= display "Rzedna") (set_tile "display_mode" "2"))
    ((= display "Brak") (set_tile "display_mode" "3"))
  )

  ;; Wybor z listy tylko uzupelnia pole Prefix.
  ;; Reczne wpisywanie nowej grupy nadal zostaje mozliwe.
  (action_tile
    "prefix_select"
    "(setq prefix_select_idx (atoi (get_tile \"prefix_select\"))) (if (> prefix_select_idx 0) (set_tile \"prefix\" (nth prefix_select_idx prefix_select_list)))"
  )

  (action_tile "btn_update" "(setq txt-h (get_tile \"txt_h\") z-prec (get_tile \"z_prec\") prefix (get_tile \"prefix\") pikt_pref (get_tile \"pikt_pref\") z_tags (get_tile \"z_tags\") kolor (itoa (1+ (atoi (get_tile \"kolor\")))) styl-idx (get_tile \"styl_rys\") disp-idx (get_tile \"display_mode\") target_idx (atoi (get_tile \"exist_layers\"))) (done_dialog 2)") 
  (action_tile "accept" "(setq txt-h (get_tile \"txt_h\") z-prec (get_tile \"z_prec\") prefix (get_tile \"prefix\") pikt_pref (get_tile \"pikt_pref\") z_tags (get_tile \"z_tags\") kolor (itoa (1+ (atoi (get_tile \"kolor\")))) styl-idx (get_tile \"styl_rys\") disp-idx (get_tile \"display_mode\")) (done_dialog 1)") 
  (action_tile "cancel" "(done_dialog 0)") 

  (setq status (start_dialog)) (unload_dialog dcl-id) (vl-file-delete dcl-file) 

  (if (or (= status 1) (= status 2)) 
    (progn 
      (setq styl (if (= styl-idx "1") "Tekst" "Blok")) 
      (cond ((= disp-idx "0") (setq display "Oba")) ((= disp-idx "1") (setq display "Numer")) ((= disp-idx "2") (setq display "Rzedna")) ((= disp-idx "3") (setq display "Brak"))) 
       
      (setq prefix (geocad-trim-string prefix))
      (setq pikt_pref (geocad-trim-string pikt_pref))

      (if (= prefix "")
        (setq prefix "POMIAR")
      )

      (vl-registry-write *geocad-registry-path* "Styl" styl)
      (vl-registry-write *geocad-registry-path* "Display" display)
      (vl-registry-write *geocad-registry-path* "TxtH" txt-h)
      (vl-registry-write *geocad-registry-path* "Prec" z-prec)
      (vl-registry-write *geocad-registry-path* "Prefix" prefix)
      (vl-registry-write *geocad-registry-path* "PiktPrefix" pikt_pref)
      (vl-registry-write *geocad-registry-path* "ZTags" z_tags)
      (vl-registry-write *geocad-registry-path* "Color" kolor)
       
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