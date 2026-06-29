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

(defun geocad-setup-ldata-get (key / res)
  (setq res
    (vl-catch-all-apply
      'vlax-ldata-get
      (list *geocad-ldata-setup-dict* key)
    )
  )

  (if (vl-catch-all-error-p res)
    nil
    res
  )
)


(defun geocad-setup-ldata-put (key value)
  (vl-catch-all-apply
    'vlax-ldata-put
    (list *geocad-ldata-setup-dict* key value)
  )

  value
)


(defun geocad-get-global-cfg (klucz domyslny / val)
  (setq val (vl-registry-read *geocad-registry-path* klucz))

  (if (not val)
    (progn
      (vl-registry-write *geocad-registry-path* klucz domyslny)
      domyslny
    )
    val
  )
)


(defun geocad-get-cfg (klucz domyslny / val)
  ;; Kolejnosc:
  ;; 1. pamiec konkretnego DWG,
  ;; 2. wartosc domyslna.
  ;;
  ;; Nie czytamy juz rejestru Windows jako fallbacku,
  ;; bo nowy rysunek nie powinien dziedziczyc ustawien
  ;; ze starego projektu.
  (setq val (geocad-setup-ldata-get klucz))

  (if val
    val
    domyslny
  )
)


(defun geocad-set-cfg (klucz value)
  ;; Zapisujemy tylko do DWG.
  ;;
  ;; DWG = pamiec projektu.
  ;; Rejestr Windows nie jest juz uzywany jako automatyczny fallback,
  ;; zeby nowe rysunki startowaly czysto od wartosci domyslnych.
  (geocad-setup-ldata-put klucz value)
  value
)

;; ======================================================
;; MÓZG: GENERATOR NUMERACJI I RADAR
;; ======================================================

(setq *geocad-new-pikt-prefix-label* "--- wpisz recznie / nowy prefix numeracji ---")


(defun geocad-normalize-pikt-prefix (val)
  ;; Prefix numeracji pikiety to nie prefix warstwy.
  ;; Tutaj tylko trimujemy tekst, nie zdejmujemy sufiksow warstw.
  (geocad-trim-string val)
)


(defun geocad-pikt-prefix-token (pikt-pref / pref)
  ;; Token do klucza LDATA.
  ;; Pusty prefix jest dozwolony, ale jako klucz potrzebuje stabilnej nazwy.
  (setq pref (geocad-normalize-pikt-prefix pikt-pref))

  (if (= pref "")
    "__BRAK_PREFIXU__"
    pref
  )
)


(defun geocad-pikt-prefixes-key (group-prefix)
  (strcat
    "Group."
    (geocad-normalize-layer-prefix group-prefix)
    ".KnownPiktPrefixes"
  )
)


(defun geocad-pikt-counter-key (group-prefix pikt-pref)
  (strcat
    "Group."
    (geocad-normalize-layer-prefix group-prefix)
    ".PiktCounter."
    (geocad-pikt-prefix-token pikt-pref)
    ".Next"
  )
)


(defun geocad-add-unique-pikt-prefix
  (pikt-pref lst / pref)
  (setq pref (geocad-normalize-pikt-prefix pikt-pref))

  ;; Nie zapisujemy pustego prefixu na liste.
  ;; Pusty prefix nadal dziala jako reczny/brak prefixu.
  (if
    (and
      (/= pref "")
      (not (member pref lst))
    )
    (append lst (list pref))
    lst
  )
)


(defun geocad-get-saved-pikt-prefixes-for-group
  (group-prefix / raw result pref)
  (setq result '())
  (setq raw
    (geocad-setup-ldata-get
      (geocad-pikt-prefixes-key group-prefix)
    )
  )

  (if
    (and
      raw
      (listp raw)
    )
    (foreach pref raw
      (setq result
        (geocad-add-unique-pikt-prefix pref result)
      )
    )
  )

  result
)


(defun geocad-save-known-pikt-prefix-for-group
  (group-prefix pikt-pref / group pref lst)
  (setq group (geocad-normalize-layer-prefix group-prefix))
  (setq pref (geocad-normalize-pikt-prefix pikt-pref))

  (if
    (and
      (/= group "")
      (/= pref "")
    )
    (progn
      (setq lst (geocad-get-saved-pikt-prefixes-for-group group))
      (setq lst (geocad-add-unique-pikt-prefix pref lst))

      (geocad-setup-ldata-put
        (geocad-pikt-prefixes-key group)
        lst
      )
    )
  )

  pref
)


(defun geocad-char-code-digit-p (code)
  (and
    (>= code 48)
    (<= code 57)
  )
)


(defun geocad-split-pikieta-number
  (txt / s i code pref num-str)
  ;; Dzieli numer pikiety na:
  ;; - prefix numeracji,
  ;; - koncowy numer.
  ;;
  ;; Przyklady:
  ;; dr_15  -> ("dr_" 15)
  ;; os_007 -> ("os_" 7)
  ;; 123    -> ("" 123)
  ;; ABC    -> nil
  (setq s (geocad-trim-string txt))

  (if (= s "")
    nil
    (progn
      (setq i (strlen s))

      (while
        (and
          (> i 0)
          (geocad-char-code-digit-p
            (ascii (substr s i 1))
          )
        )
        (setq i (1- i))
      )

      ;; Jezeli i jest rowne dlugosci tekstu, to znaczy,
      ;; ze na koncu nie bylo zadnych cyfr.
      (if (= i (strlen s))
        nil
        (progn
          (if (= i 0)
            (setq pref "")
            (setq pref (substr s 1 i))
          )

          (setq num-str (substr s (1+ i)))

          (if (= num-str "")
            nil
            (list pref (atoi num-str))
          )
        )
      )
    )
  )
)


(defun geocad-block-attr-text
  (obj tag / result)
  (setq result nil)

  (if obj
    (foreach att (vlax-invoke obj 'GetAttributes)
      (if (= (strcase (vla-get-TagString att)) (strcase tag))
        (setq result (vla-get-TextString att))
      )
    )
  )

  result
)


(defun geocad-scan-pikt-prefixes-from-group
  (group-prefix / group lay ss i obj nr parsed result)
  ;; Skanuje realne bloki Pikieta_Geo w danej grupie roboczej
  ;; i wyciaga prefixy numeracji z atrybutu NR.
  (setq result '())
  (setq group (geocad-normalize-layer-prefix group-prefix))

  (if (/= group "")
    (progn
      (setq lay
        (geocad-layer-name group *geocad-layer-type-points*)
      )

      (setq ss
        (ssget
          "_X"
          (list
            '(0 . "INSERT")
            '(2 . "Pikieta_Geo")
            (cons 8 lay)
          )
        )
      )

      (if ss
        (progn
          (setq i 0)

          (while (< i (sslength ss))
            (setq obj (vlax-ename->vla-object (ssname ss i)))
            (setq nr (geocad-block-attr-text obj "NR"))
            (setq parsed (geocad-split-pikieta-number nr))

            (if parsed
              (setq result
                (geocad-add-unique-pikt-prefix
                  (car parsed)
                  result
                )
              )
            )

            (setq i (1+ i))
          )
        )
      )
    )
  )

  (vl-sort result '<)
)


(defun geocad-get-known-pikt-prefixes-for-group
  (group-prefix current-pikt-pref / result pref)
  ;; Lista prefixow numeracji dla grupy:
  ;; - zapisane w pamieci DWG,
  ;; - wykryte z realnych pikiet w rysunku,
  ;; - aktualnie wpisany prefix.
  (setq result '())

  (foreach pref (geocad-get-saved-pikt-prefixes-for-group group-prefix)
    (setq result
      (geocad-add-unique-pikt-prefix pref result)
    )
  )

  (foreach pref (geocad-scan-pikt-prefixes-from-group group-prefix)
    (setq result
      (geocad-add-unique-pikt-prefix pref result)
    )
  )

  (setq result
    (geocad-add-unique-pikt-prefix current-pikt-pref result)
  )

  (vl-sort result '<)
)


(defun geocad-max-number-in-group-for-pikt-prefix
  (group-prefix pikt-pref / group pref lay ss i obj nr parsed max-num)
  ;; Szuka najwyzszego numeru dla:
  ;; - konkretnej grupy roboczej,
  ;; - konkretnego prefixu numeracji.
  (setq group (geocad-normalize-layer-prefix group-prefix))
  (setq pref (geocad-normalize-pikt-prefix pikt-pref))
  (setq max-num 0)

  (if (/= group "")
    (progn
      (setq lay
        (geocad-layer-name group *geocad-layer-type-points*)
      )

      (setq ss
        (ssget
          "_X"
          (list
            '(0 . "INSERT")
            '(2 . "Pikieta_Geo")
            (cons 8 lay)
          )
        )
      )

      (if ss
        (progn
          (setq i 0)

          (while (< i (sslength ss))
            (setq obj (vlax-ename->vla-object (ssname ss i)))
            (setq nr (geocad-block-attr-text obj "NR"))
            (setq parsed (geocad-split-pikieta-number nr))

            (if
              (and
                parsed
                (= (car parsed) pref)
              )
              (setq max-num
                (max max-num (cadr parsed))
              )
            )

            (setq i (1+ i))
          )
        )
      )
    )
  )

  max-num
)


(defun geocad-get-pikt-counter
  (group-prefix pikt-pref / val)
  (setq val
    (geocad-setup-ldata-get
      (geocad-pikt-counter-key group-prefix pikt-pref)
    )
  )

  (cond
    ((numberp val) val)
    (val (atoi (vl-princ-to-string val)))
    (T 1)
  )
)


(defun geocad-set-pikt-counter
  (group-prefix pikt-pref next-number)
  (geocad-setup-ldata-put
    (geocad-pikt-counter-key group-prefix pikt-pref)
    next-number
  )

  next-number
)


(defun geocad-next-number-for-group-pikt-prefix
  (group-prefix pikt-pref / saved max-draw next-by-draw next-number)
  ;; Najbezpieczniejszy model:
  ;; nastepny numer = max(licznik z pamieci, najwyzszy numer z rysunku + 1)
  ;;
  ;; Nie cofamy licznika automatycznie po usunieciu pikiet,
  ;; zeby nie ryzykowac duplikatow.
  (setq saved (geocad-get-pikt-counter group-prefix pikt-pref))
  (setq max-draw
    (geocad-max-number-in-group-for-pikt-prefix
      group-prefix
      pikt-pref
    )
  )

  (setq next-by-draw (1+ max-draw))
  (setq next-number (max saved next-by-draw))

  next-number
)


(defun geocad-pikt-prefix-display-name (pikt-pref / pref)
  (setq pref (geocad-normalize-pikt-prefix pikt-pref))

  (if (= pref "")
    "(bez prefixu)"
    pref
  )
)


(defun geocad-pikt-prefix-display-label
  (group-prefix pikt-pref / pref next-number)
  (setq pref (geocad-normalize-pikt-prefix pikt-pref))
  (setq next-number
    (geocad-next-number-for-group-pikt-prefix group-prefix pref)
  )

  (strcat
    (geocad-pikt-prefix-display-name pref)
    "  |  nastepny: "
    (itoa next-number)
  )
)


(defun geocad-build-pikt-prefix-display-list
  (group-prefix pikt-prefixes / result pref)
  (setq result '())

  (foreach pref pikt-prefixes
    (setq result
      (append
        result
        (list
          (geocad-pikt-prefix-display-label group-prefix pref)
        )
      )
    )
  )

  result
)


(defun geocad-setup-refresh-pikt-prefix-list
  (group-prefix current-pikt-pref / prefixes select-prefixes display idx)
  ;; Odswieza popup prefixow numeracji w GEO_SETUP.
  ;; Zwraca liste:
  ;; (prefixy-wewnetrzne teksty-widoczne aktualny-index)
  (setq group-prefix (geocad-normalize-layer-prefix group-prefix))
  (setq current-pikt-pref (geocad-normalize-pikt-prefix current-pikt-pref))

  (setq prefixes
    (geocad-get-known-pikt-prefixes-for-group
      group-prefix
      current-pikt-pref
    )
  )

  (setq select-prefixes
    (cons "" prefixes)
  )

  (setq display
    (cons
      *geocad-new-pikt-prefix-label*
      (geocad-build-pikt-prefix-display-list group-prefix prefixes)
    )
  )

  (start_list "pikt_pref_select")
  (mapcar 'add_list display)
  (end_list)

  (setq idx
    (geocad-index-of-string current-pikt-pref select-prefixes)
  )

  (if (not idx)
    (setq idx 0)
  )

  (set_tile "pikt_pref_select" (itoa idx))

  (list select-prefixes display idx)
)


(defun GP:PobierzNastepnyNumer
  (/ group pref nr)
  ;; Licznik jest teraz per:
  ;; - grupa robocza warstw,
  ;; - prefix numeracji pikiety.
  ;;
  ;; Np.:
  ;; DROGI + os_ -> os_1, os_2, os_3
  ;; DROGI + wp_ -> wp_1, wp_2, wp_3
  ;; KANAL + os_ -> os_1, os_2, os_3
  (setq group
    (geocad-normalize-layer-prefix
      (geocad-get-cfg "Prefix" "POMIAR")
    )
  )

  (if (= group "")
    (setq group "POMIAR")
  )

  (setq pref
    (geocad-normalize-pikt-prefix
      (geocad-get-cfg "PiktPrefix" "")
    )
  )

  ;; Nowy prefix numeracji od razu trafia do pamieci grupy.
  (geocad-save-known-pikt-prefix-for-group group pref)

  (setq nr
    (geocad-next-number-for-group-pikt-prefix group pref)
  )

  ;; Od razu zapisujemy kolejny numer.
  ;; Przy batchu finalny licznik i tak zostanie nadpisany na koncu serii.
  (geocad-set-pikt-counter group pref (1+ nr))

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
  (setq prefix (geocad-normalize-layer-prefix (geocad-get-cfg "Prefix" "POMIAR")))
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


(defun geocad-pikieta-batch-end (batch / ctx next-nr group pikt-pref)
  ;; Zapisuje finalny licznik tylko wtedy, gdy w sesji uzyto auto-numeracji.
  ;; Licznik zapisujemy per:
  ;; - grupa robocza,
  ;; - prefix numeracji pikiety.
  (if (and batch (geocad-ctx-get 'auto-used batch))
    (progn
      (setq ctx (geocad-ctx-get 'ctx batch))
      (setq next-nr (geocad-ctx-get 'next-nr batch))
      (setq group (geocad-ctx-get 'prefix ctx))
      (setq pikt-pref (geocad-ctx-get 'pikt-pref ctx))

      (if next-nr
        (progn
          (geocad-save-known-pikt-prefix-for-group group pikt-pref)
          (geocad-set-pikt-counter group pikt-pref next-nr)
        )
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
;; SKANER, PAMIEC GRUP ROBOCZYCH I AKTUALIZATOR PIKIET
;; ======================================================

(setq *geocad-new-group-label* "--- wpisz recznie / nowa grupa ---")
(setq *geocad-all-groups-label* "--- WSZYSTKIE W RYSUNKU ---")


(defun geocad-index-of-string
  (val lst / idx result)
  (setq idx 0)
  (setq result nil)

  (while
    (and
      lst
      (not result)
    )
    (if (= val (car lst))
      (setq result idx)
      (progn
        (setq idx (1+ idx))
        (setq lst (cdr lst))
      )
    )
  )

  result
)


(defun geocad-safe-atoi (val default / n)
  (setq n (atoi (if val val "")))

  (if (= n 0)
    default
    n
  )
)


(defun geocad-group-cfg-key
  (prefix key)
  (strcat
    "Group."
    (geocad-normalize-layer-prefix prefix)
    "."
    key
  )
)


(defun geocad-group-cfg-read
  (prefix key default / val)
  (setq val
    (geocad-setup-ldata-get
      (geocad-group-cfg-key prefix key)
    )
  )

  (if val val default)
)


(defun geocad-group-cfg-write
  (prefix key value)
  (if
    (and
      prefix
      (/= (geocad-normalize-layer-prefix prefix) "")
    )
    (geocad-setup-ldata-put
      (geocad-group-cfg-key prefix key)
      value
    )
  )

  value
)


(defun geocad-get-saved-prefixes
  (/ raw result pref)
  (setq result '())
  (setq raw (geocad-setup-ldata-get "KnownPrefixes"))

  (if
    (and
      raw
      (listp raw)
    )
    (foreach pref raw
      (setq result (geocad-add-unique-prefix pref result))
    )
  )

  result
)


(defun geocad-save-known-prefix
  (prefix / pref lst)
  (setq pref (geocad-normalize-layer-prefix prefix))

  (if (/= pref "")
    (progn
      (setq lst (geocad-get-saved-prefixes))
      (setq lst (geocad-add-unique-prefix pref lst))
      (geocad-setup-ldata-put "KnownPrefixes" lst)
    )
  )

  pref
)


(defun geocad-save-group-settings
  (prefix kolor pikt_pref styl display txt-h z-prec z_tags / pref)
  (setq pref (geocad-normalize-layer-prefix prefix))

  (if (/= pref "")
    (progn
      (geocad-save-known-prefix pref)

      (geocad-group-cfg-write pref "Color" kolor)
      (geocad-group-cfg-write pref "PiktPrefix" pikt_pref)
      (geocad-group-cfg-write pref "Styl" styl)
      (geocad-group-cfg-write pref "Display" display)
      (geocad-group-cfg-write pref "TxtH" txt-h)
      (geocad-group-cfg-write pref "Prec" z-prec)
      (geocad-group-cfg-write pref "ZTags" z_tags)

      ;; Prefix numeracji tez zapisujemy w pamieci tej grupy.
      ;; Dzieki temu pojawi sie w GEO_SETUP nawet przed utworzeniem pikiety.
      (geocad-save-known-pikt-prefix-for-group pref pikt_pref)
    )
  )

  pref
)


(defun geocad-layer-object-if-exists
  (layers layname / res)
  (if
    (and
      layers
      layname
      (/= layname "")
    )
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


(defun geocad-layer-color-if-exists
  (doc layname / layers lay)
  (setq layers (vla-get-Layers doc))
  (setq lay (geocad-layer-object-if-exists layers layname))

  (if lay
    (itoa (vla-get-Color lay))
    nil
  )
)


(defun geocad-group-layer-color
  (doc prefix fallback / color)
  ;; Kolor domyslny grupy:
  ;; 1. warstwa PIKIETY,
  ;; 2. warstwa ETYKIETA_NR,
  ;; 3. warstwa ETYKIETA_H,
  ;; 4. warstwa POLYLINES_FROM_MULTI,
  ;; 5. fallback.
  (setq prefix (geocad-normalize-layer-prefix prefix))

  (setq color
    (geocad-layer-color-if-exists
      doc
      (geocad-layer-name prefix *geocad-layer-type-points*)
    )
  )

  (if (not color)
    (setq color
      (geocad-layer-color-if-exists
        doc
        (geocad-layer-name prefix *geocad-layer-type-label-nr*)
      )
    )
  )

  (if (not color)
    (setq color
      (geocad-layer-color-if-exists
        doc
        (geocad-layer-name prefix *geocad-layer-type-label-h*)
      )
    )
  )

  (if (not color)
    (setq color
      (geocad-layer-color-if-exists
        doc
        (geocad-layer-name prefix *geocad-layer-type-polyline-multi*)
      )
    )
  )

  (if color color fallback)
)


(defun geocad-count-objects-on-layer
  (layname / ss)
  (if
    (and
      layname
      (/= layname "")
      (tblsearch "LAYER" layname)
    )
    (progn
      (setq ss
        (ssget
          "_X"
          (list (cons 8 layname))
        )
      )

      (if ss
        (sslength ss)
        0
      )
    )
    0
  )
)


(defun geocad-count-objects-in-group
  (prefix / pref count)
  (setq pref (geocad-normalize-layer-prefix prefix))
  (setq count 0)

  (if (/= pref "")
    (progn
      (setq count
        (+
          count
          (geocad-count-objects-on-layer
            (geocad-layer-name pref *geocad-layer-type-points*)
          )
        )
      )

      (setq count
        (+
          count
          (geocad-count-objects-on-layer
            (geocad-layer-name pref *geocad-layer-type-label-nr*)
          )
        )
      )

      (setq count
        (+
          count
          (geocad-count-objects-on-layer
            (geocad-layer-name pref *geocad-layer-type-label-h*)
          )
        )
      )

      (setq count
        (+
          count
          (geocad-count-objects-on-layer
            (geocad-layer-name pref *geocad-layer-type-polyline-multi*)
          )
        )
      )
    )
  )

  count
)


(defun geocad-prefix-display-label
  (prefix / pref count)
  (setq pref (geocad-normalize-layer-prefix prefix))
  (setq count (geocad-count-objects-in-group pref))

  (strcat
    pref
    "  |  obiekty: "
    (itoa count)
  )
)


(defun geocad-build-prefix-display-list
  (prefixes / result pref)
  (setq result '())

  (foreach pref prefixes
    (setq result
      (append
        result
        (list (geocad-prefix-display-label pref))
      )
    )
  )

  result
)


(defun geocad-get-prefixes-from-layers
  (doc / layers lay-obj lay pref lst)
  (setq lst '())
  (setq layers (vla-get-Layers doc))

  (vlax-for lay-obj layers
    (setq lay (vla-get-Name lay-obj))
    (setq pref (geocad-managed-layer-prefix-from-name lay))

    (if pref
      (setq lst (geocad-add-unique-prefix pref lst))
    )
  )

  lst
)


(defun geocad-get-existing-prefixes
  (/ doc lst saved pref current)
  ;; Lista grup roboczych:
  ;; - grupy wykryte z realnych warstw GeoprofiCAD,
  ;; - grupy zapisane w pamieci konkretnego DWG,
  ;; - aktualnie aktywna grupa.
  ;;
  ;; Wszystko jest normalizowane do czystego prefixu.
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq lst '())

  (foreach pref (geocad-get-prefixes-from-layers doc)
    (setq lst (geocad-add-unique-prefix pref lst))
  )

  (setq saved (geocad-get-saved-prefixes))

  (foreach pref saved
    (setq lst (geocad-add-unique-prefix pref lst))
  )

  (setq current
    (geocad-normalize-layer-prefix
      (geocad-get-cfg "Prefix" "POMIAR")
    )
  )

  (if (= current "")
    (setq current "POMIAR")
  )

  (setq lst (geocad-add-unique-prefix current lst))

  (vl-sort lst '<)
)


(defun geocad-update-layer-color-if-exists
  (layers layname kolor / lay)
  (setq lay (geocad-layer-object-if-exists layers layname))

  (if lay
    (vl-catch-all-apply
      'vla-put-Color
      (list lay kolor)
    )
  )
)


(defun geocad-update-managed-layer-colors
  (doc prefix kolor / layers pref)
  ;; Aktualizuje kolor wszystkich istniejacych warstw danej grupy.
  ;; Nie tworzy pustych warstw, jezeli ich nie ma.
  (setq pref (geocad-normalize-layer-prefix prefix))

  (if
    (and
      doc
      pref
      (/= pref "")
    )
    (progn
      (setq layers (vla-get-Layers doc))

      (geocad-update-layer-color-if-exists
        layers
        (geocad-layer-name pref *geocad-layer-type-points*)
        kolor
      )

      (geocad-update-layer-color-if-exists
        layers
        (geocad-layer-name pref *geocad-layer-type-label-nr*)
        kolor
      )

      (geocad-update-layer-color-if-exists
        layers
        (geocad-layer-name pref *geocad-layer-type-label-h*)
        kolor
      )

      (geocad-update-layer-color-if-exists
        layers
        (geocad-layer-name pref *geocad-layer-type-polyline-multi*)
        kolor
      )
    )
  )
)


(defun geocad-popup-color-index
  (kolor / n)
  (setq n (atoi kolor))

  (if
    (and
      (>= n 1)
      (<= n 7)
    )
    (itoa (1- n))
    "2"
  )
)


(defun geocad-popup-styl-index
  (styl)
  (if (= styl "Tekst")
    "1"
    "0"
  )
)


(defun geocad-popup-display-index
  (display)
  (cond
    ((= display "Oba") "0")
    ((= display "Numer") "1")
    ((= display "Rzedna") "2")
    ((= display "Brak") "3")
    (T "0")
  )
)


(defun geocad-display-from-popup-index
  (idx)
  (cond
    ((= idx "0") "Oba")
    ((= idx "1") "Numer")
    ((= idx "2") "Rzedna")
    ((= idx "3") "Brak")
    (T "Oba")
  )
)


(defun geocad-setup-apply-group-to-dialog
  (prefix / doc pref fallback-color kolor pikt-pref styl display txt-h z-prec z-tags)
  ;; Wywoluje sie po wyborze grupy roboczej w popupie.
  ;;
  ;; Jezeli grupa ma pamiec w DWG, przywraca jej ustawienia.
  ;; Jezeli nie ma pamieci, bierze:
  ;; - kolor z istniejacych warstw grupy,
  ;; - reszte z aktualnych/globalnych ustawien.
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq pref (geocad-normalize-layer-prefix prefix))

  (if (/= pref "")
    (progn
      (setq fallback-color
        (itoa
          (1+
            (atoi
              (get_tile "kolor")
            )
          )
        )
      )

      (setq kolor
        (geocad-group-cfg-read
          pref
          "Color"
          (geocad-group-layer-color doc pref fallback-color)
        )
      )

      (setq pikt-pref
        (geocad-group-cfg-read
          pref
          "PiktPrefix"
          ""
        )
      )

      (setq styl
        (geocad-group-cfg-read
          pref
          "Styl"
          (if (= (get_tile "styl_rys") "1") "Tekst" "Blok")
        )
      )

      (setq display
        (geocad-group-cfg-read
          pref
          "Display"
          (geocad-display-from-popup-index (get_tile "display_mode"))
        )
      )

      (setq txt-h
        (geocad-group-cfg-read
          pref
          "TxtH"
          (get_tile "txt_h")
        )
      )

      (setq z-prec
        (geocad-group-cfg-read
          pref
          "Prec"
          (get_tile "z_prec")
        )
      )

      (setq z-tags
        (geocad-group-cfg-read
          pref
          "ZTags"
          (get_tile "z_tags")
        )
      )

      (set_tile "prefix" pref)
      (set_tile "pikt_pref" pikt-pref)
      (set_tile "txt_h" txt-h)
      (set_tile "z_prec" z-prec)
      (set_tile "z_tags" z-tags)
      (set_tile "kolor" (geocad-popup-color-index kolor))
      (set_tile "styl_rys" (geocad-popup-styl-index styl))
      (set_tile "display_mode" (geocad-popup-display-index display))
    )
  )
)


(defun geocad-update-existing
  (
    doc target_prefix kolor-str txt-h-str z-prec-str display
    /
    ss i ent obj pt px py pz
    lay-pt lay-nr lay-h
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
  ;; Aktualizacja kolorow warstw grupy.
  ;; ------------------------------------------------------
  (if (= target_prefix *geocad-all-groups-label*)
    (foreach pref (geocad-get-existing-prefixes)
      (geocad-update-managed-layer-colors doc pref kolor)
    )
    (geocad-update-managed-layer-colors doc target_prefix kolor)
  )

  ;; ------------------------------------------------------
  ;; Wybor blokow do aktualizacji.
  ;; ------------------------------------------------------
  (if (= target_prefix *geocad-all-groups-label*)
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
        ;; Ustalenie grupy z aktualnej warstwy bloku.
        ;; ------------------------------------------------------
        (setq lay-pt (vla-get-Layer obj))
        (setq pref (geocad-managed-layer-prefix-from-name lay-pt))

        (if pref
          (progn
            (setq lay-pt (geocad-layer-name pref *geocad-layer-type-points*))
            (setq lay-nr (geocad-layer-name pref *geocad-layer-type-label-nr*))
            (setq lay-h  (geocad-layer-name pref *geocad-layer-type-label-h*))
          )
          (progn
            (if (= target_prefix *geocad-all-groups-label*)
              (progn
                ;; Legacy fallback:
                ;; jezeli stary blok nie siedzi na standardowej warstwie,
                ;; zostaje na swojej warstwie.
                (setq lay-pt (vla-get-Layer obj))
                (setq lay-nr lay-pt)
                (setq lay-h lay-pt)
              )
              (progn
                (setq pref (geocad-normalize-layer-prefix target_prefix))
                (setq lay-pt (geocad-layer-name pref *geocad-layer-type-points*))
                (setq lay-nr (geocad-layer-name pref *geocad-layer-type-label-nr*))
                (setq lay-h  (geocad-layer-name pref *geocad-layer-type-label-h*))
              )
            )
          )
        )

        ;; Warstwy tworzymy bezpiecznie tylko tutaj, bo aktualizujemy
        ;; realne bloki i musimy miec gdzie przeniesc atrybuty.
        (geocad-ensure-layer doc lay-pt kolor)
        (geocad-ensure-layer doc lay-nr kolor)
        (geocad-ensure-layer doc lay-h kolor)

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
          " blokow pikiet dla grupy: "
          target_prefix
        )
      )
    )
    (princ
      "\n[INFO] Nie znaleziono blokow do aktualizacji. Zaktualizowano kolor istniejacych warstw grupy, jezeli istnialy."
    )
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
    prefix_select_prefixes prefix_select_display prefix_select_idx
    pikt_prefix_bundle
    pikt_prefix_select_prefixes pikt_prefix_select_display pikt_prefix_select_idx
    target_prefix
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; ------------------------------------------------------
  ;; Odczyt ustawien:
  ;; 1. DWG,
  ;; 2. rejestr,
  ;; 3. default.
  ;; ------------------------------------------------------
  (setq txt-h (geocad-get-cfg "TxtH" "1.0")
        z-prec (geocad-get-cfg "Prec" "2")
        prefix (geocad-get-cfg "Prefix" "POMIAR")
        pikt_pref (geocad-get-cfg "PiktPrefix" "")
        z_tags (geocad-get-cfg "ZTags" "H, Z, RZEDNA")
        kolor (geocad-get-cfg "Color" "3")
        styl (geocad-get-cfg "Styl" "Blok")
        display (geocad-get-cfg "Display" "Oba")
  )

  (setq prefix (geocad-normalize-layer-prefix prefix))
  (setq pikt_pref (geocad-normalize-pikt-prefix pikt_pref))

  (if (= prefix "")
    (setq prefix "POMIAR")
  )

  ;; ------------------------------------------------------
  ;; Lista grup roboczych:
  ;; - z warstw,
  ;; - z pamieci DWG,
  ;; - aktualny prefix.
  ;; ------------------------------------------------------
  (setq prefix_groups (geocad-get-existing-prefixes))
  (setq prefix_groups (geocad-add-unique-prefix prefix prefix_groups))
  (setq prefix_groups (vl-sort prefix_groups '<))

  ;; Prefixy logiczne dla wyboru aktywnej grupy.
  ;; Pierwsza pozycja oznacza reczny wpis nowej grupy.
  (setq prefix_select_prefixes
    (cons "" prefix_groups)
  )

  ;; Teksty widoczne w popupie grup.
  ;; Tu pokazujemy liczbe obiektow.
  (setq prefix_select_display
    (cons
      *geocad-new-group-label*
      (geocad-build-prefix-display-list prefix_groups)
    )
  )

  ;; ------------------------------------------------------
  ;; DCL
  ;; ------------------------------------------------------
  (setq dcl-file (vl-filename-mktemp "geosetup.dcl"))
  (setq dcl-fn (open dcl-file "w"))

  (write-line "GeoSetup : dialog { label = \"GEO_SETUP - Grupa robocza GeoprofiCAD\";" dcl-fn)

  (write-line "  : boxed_column { label = \"Grupa robocza\";" dcl-fn)
  (write-line "    : popup_list { key = \"prefix_select\"; label = \"Wybierz grupe:\"; width = 48; }" dcl-fn)
  (write-line "    : edit_box { key = \"prefix\"; label = \"Prefix grupy (np. DROGI):\"; edit_width = 24; }" dcl-fn)
  (write-line "  }" dcl-fn)

  (write-line "  : boxed_column { label = \"Parametry tej grupy\";" dcl-fn)
  (write-line "    : popup_list { key = \"styl_rys\"; label = \"Styl na mapie:\"; list = \"Inteligentny Blok\\nZwykly Punkt + Tekst\"; }" dcl-fn)
  (write-line "    : popup_list { key = \"display_mode\"; label = \"Widocznosc:\"; list = \"Oba (Nr + H)\\nTylko Numer\\nTylko Rzedna (H)\\nNic (Sam symbol)\"; }" dcl-fn)
  (write-line "    : edit_box { key = \"txt_h\"; label = \"Wysokosc tekstu:\"; edit_width = 8; }" dcl-fn)
  (write-line "    : edit_box { key = \"z_prec\"; label = \"Miejsca po przecinku (Z):\"; edit_width = 8; }" dcl-fn)
  (write-line "    : popup_list { key = \"pikt_pref_select\"; label = \"Wybierz prefix numeracji:\"; width = 48; }" dcl-fn)
  (write-line "    : edit_box { key = \"pikt_pref\"; label = \"Prefix numeru pikiety (np. dr_):\"; edit_width = 20; }" dcl-fn)
  (write-line "    : edit_box { key = \"z_tags\"; label = \"Tagi rzednych (np. H, Z, WYS):\"; edit_width = 20; }" dcl-fn)
  (write-line "    : popup_list { key = \"kolor\"; label = \"Kolor podstawowy:\"; list = \"1 - Czerwony\\n2 - Zolty\\n3 - Zielony\\n4 - Cyjan\\n5 - Niebieski\\n6 - Magenta\\n7 - Czarny/Bialy\"; }" dcl-fn)
  (write-line "  }" dcl-fn)

  (write-line "  : boxed_column { label = \"Akcje\";" dcl-fn)
  (write-line "    : text { label = \"Zapisz - zapisuje ustawienia tej grupy dla nowych pikiet.\"; }" dcl-fn)
  (write-line "    : text { label = \"Zapisz i aktualizuj - zapisuje ustawienia i poprawia istniejace obiekty tej grupy.\"; }" dcl-fn)
  (write-line "    : text { label = \"Wszystkie grupy - stosuje aktualne parametry hurtowo do wszystkich grup w rysunku.\"; }" dcl-fn)
  (write-line "  }" dcl-fn)

  (write-line "  : row { alignment = centered;" dcl-fn)
  (write-line "    : button { key = \"save_only\"; label = \"Zapisz\"; is_default = true; }" dcl-fn)
  (write-line "    : button { key = \"save_update\"; label = \"Zapisz i aktualizuj te grupe\"; }" dcl-fn)
  (write-line "  }" dcl-fn)

  (write-line "  : row { alignment = centered;" dcl-fn)
  (write-line "    : button { key = \"save_update_all\"; label = \"Zapisz i aktualizuj wszystkie grupy\"; }" dcl-fn)
  (write-line "    : button { key = \"cancel\"; label = \"Anuluj\"; is_cancel = true; }" dcl-fn)
  (write-line "  }" dcl-fn)

  (write-line "}" dcl-fn)

  (close dcl-fn)

  (setq dcl-id (load_dialog dcl-file))

  (if
    (not (new_dialog "GeoSetup" dcl-id))
    (progn
      (alert "Blad ladowania okna DCL.")
      (exit)
    )
  )

  ;; ------------------------------------------------------
  ;; Wypelnienie listy grup.
  ;; ------------------------------------------------------
  (start_list "prefix_select")
  (mapcar 'add_list prefix_select_display)
  (end_list)

  ;; ------------------------------------------------------
  ;; Wypelnienie pol.
  ;; ------------------------------------------------------
  (set_tile "txt_h" txt-h)
  (set_tile "z_prec" z-prec)
  (set_tile "prefix" prefix)
  (set_tile "pikt_pref" pikt_pref)
  (set_tile "z_tags" z_tags)

  (setq prefix_select_idx
    (geocad-index-of-string prefix prefix_select_prefixes)
  )

  (if (not prefix_select_idx)
    (setq prefix_select_idx 0)
  )

  (set_tile "prefix_select" (itoa prefix_select_idx))

  ;; ------------------------------------------------------
  ;; Wypelnienie listy prefixow numeracji dla aktualnej grupy.
  ;; ------------------------------------------------------
  (setq pikt_prefix_bundle
    (geocad-setup-refresh-pikt-prefix-list prefix pikt_pref)
  )
  (setq pikt_prefix_select_prefixes (car pikt_prefix_bundle))
  (setq pikt_prefix_select_display (cadr pikt_prefix_bundle))
  (setq pikt_prefix_select_idx (caddr pikt_prefix_bundle))

  (setq col-idx (atoi kolor))

  (if
    (and
      (>= col-idx 1)
      (<= col-idx 7)
    )
    (set_tile "kolor" (itoa (1- col-idx)))
    (set_tile "kolor" "2")
  )

  (set_tile "styl_rys" (geocad-popup-styl-index styl))
  (set_tile "display_mode" (geocad-popup-display-index display))

  ;; ------------------------------------------------------
  ;; Akcje DCL.
  ;; ------------------------------------------------------

  ;; Wybor grupy:
  ;; - popup pokazuje opis z liczba obiektow,
  ;; - wewnetrznie uzywamy czystego prefixu z prefix_select_prefixes.
  ;; Po wyborze grupy pola automatycznie laduja jej pamiec z DWG.
  ;; Potem odswiezamy liste prefixow numeracji dla tej grupy.
  (action_tile
    "prefix_select"
    "(setq prefix_select_idx (atoi (get_tile \"prefix_select\"))) (if (> prefix_select_idx 0) (progn (geocad-setup-apply-group-to-dialog (nth prefix_select_idx prefix_select_prefixes)) (setq pikt_prefix_bundle (geocad-setup-refresh-pikt-prefix-list (get_tile \"prefix\") (get_tile \"pikt_pref\"))) (setq pikt_prefix_select_prefixes (car pikt_prefix_bundle)) (setq pikt_prefix_select_display (cadr pikt_prefix_bundle)) (setq pikt_prefix_select_idx (caddr pikt_prefix_bundle))))"
  )

  ;; Jezeli recznie zmienisz prefix grupy, popup grup wraca na tryb nowej/recznej grupy.
  ;; Dodatkowo lista prefixow numeracji odswieza sie dla wpisanej grupy.
  (action_tile
    "prefix"
    "(setq prefix_select_idx 0) (set_tile \"prefix_select\" \"0\") (set_tile \"pikt_pref\" \"\") (setq pikt_prefix_bundle (geocad-setup-refresh-pikt-prefix-list (get_tile \"prefix\") \"\")) (setq pikt_prefix_select_prefixes (car pikt_prefix_bundle)) (setq pikt_prefix_select_display (cadr pikt_prefix_bundle)) (setq pikt_prefix_select_idx (caddr pikt_prefix_bundle))"
  )

  ;; Wybor prefixu numeracji:
  ;; popup pokazuje prefix + nastepny numer,
  ;; ale do pola wpisujemy sam czysty prefix numeracji.
  (action_tile
    "pikt_pref_select"
    "(setq pikt_prefix_select_idx (atoi (get_tile \"pikt_pref_select\"))) (if (> pikt_prefix_select_idx 0) (set_tile \"pikt_pref\" (nth pikt_prefix_select_idx pikt_prefix_select_prefixes)))"
  )

  ;; Jezeli recznie wpiszesz nowy prefix numeracji,
  ;; popup wraca na tryb nowego/recznego prefixu.
  (action_tile
    "pikt_pref"
    "(setq pikt_prefix_select_idx 0) (set_tile \"pikt_pref_select\" \"0\")"
  )

  ;; Status 1:
  ;; tylko zapis ustawien aktywnej grupy.
  (action_tile
    "save_only"
    "(setq txt-h (get_tile \"txt_h\") z-prec (get_tile \"z_prec\") prefix (get_tile \"prefix\") pikt_pref (get_tile \"pikt_pref\") z_tags (get_tile \"z_tags\") kolor (itoa (1+ (atoi (get_tile \"kolor\")))) styl-idx (get_tile \"styl_rys\") disp-idx (get_tile \"display_mode\")) (done_dialog 1)"
  )

  ;; Status 2:
  ;; zapis ustawien + aktualizacja obiektow tej samej grupy.
  (action_tile
    "save_update"
    "(setq txt-h (get_tile \"txt_h\") z-prec (get_tile \"z_prec\") prefix (get_tile \"prefix\") pikt_pref (get_tile \"pikt_pref\") z_tags (get_tile \"z_tags\") kolor (itoa (1+ (atoi (get_tile \"kolor\")))) styl-idx (get_tile \"styl_rys\") disp-idx (get_tile \"display_mode\")) (done_dialog 2)"
  )

  ;; Status 3:
  ;; zapis ustawien aktywnej grupy + hurtowa aktualizacja wszystkich grup.
  (action_tile
    "save_update_all"
    "(setq txt-h (get_tile \"txt_h\") z-prec (get_tile \"z_prec\") prefix (get_tile \"prefix\") pikt_pref (get_tile \"pikt_pref\") z_tags (get_tile \"z_tags\") kolor (itoa (1+ (atoi (get_tile \"kolor\")))) styl-idx (get_tile \"styl_rys\") disp-idx (get_tile \"display_mode\")) (done_dialog 3)"
  )

  (action_tile "cancel" "(done_dialog 0)")

  (setq status (start_dialog))
  (unload_dialog dcl-id)
  (vl-file-delete dcl-file)

  ;; ------------------------------------------------------
  ;; Zapis po akcji.
  ;;
  ;; status 0 = Anuluj
  ;; status 1 = Zapisz
  ;; status 2 = Zapisz i aktualizuj te grupe
  ;; status 3 = Zapisz i aktualizuj wszystkie grupy
  ;; ------------------------------------------------------
  (if
    (member status '(1 2 3))
    (progn
      (setq styl
        (if (= styl-idx "1")
          "Tekst"
          "Blok"
        )
      )

      (setq display
        (geocad-display-from-popup-index disp-idx)
      )

      ;; Prefix grupy jest zawsze czysta nazwa grupy.
      ;; Jezeli ktos wpisze DROGI_PIKIETY, zapisze sie DROGI.
      (setq prefix (geocad-normalize-layer-prefix prefix))
      (setq pikt_pref (geocad-normalize-pikt-prefix pikt_pref))
      (setq z_tags (geocad-trim-string z_tags))

      (if (= prefix "")
        (setq prefix "POMIAR")
      )

      ;; ------------------------------------------------------
      ;; Zapis aktywnej grupy:
      ;; - do DWG,
      ;; - do rejestru jako fallback globalny.
      ;; ------------------------------------------------------
      (geocad-set-cfg "Styl" styl)
      (geocad-set-cfg "Display" display)
      (geocad-set-cfg "TxtH" txt-h)
      (geocad-set-cfg "Prec" z-prec)
      (geocad-set-cfg "Prefix" prefix)
      (geocad-set-cfg "PiktPrefix" pikt_pref)
      (geocad-set-cfg "ZTags" z_tags)
      (geocad-set-cfg "Color" kolor)

      ;; ------------------------------------------------------
      ;; Zapis pamieci aktualnej grupy w tym DWG.
      ;; geocad-save-group-settings zapisuje tez prefix numeracji
      ;; do listy znanych prefixow tej grupy.
      ;; ------------------------------------------------------
      (geocad-save-group-settings
        prefix
        kolor
        pikt_pref
        styl
        display
        txt-h
        z-prec
        z_tags
      )

      ;; ------------------------------------------------------
      ;; Status 1: tylko zapis.
      ;; ------------------------------------------------------
      (if (= status 1)
        (princ
          (strcat
            "\n[OK] Zapisano ustawienia grupy roboczej: "
            prefix
            ". Nowe pikiety beda tworzone na warstwach tej grupy."
          )
        )
      )

      ;; ------------------------------------------------------
      ;; Status 2:
      ;; zapis + aktualizacja obiektow tej samej grupy.
      ;; ------------------------------------------------------
      (if (= status 2)
        (progn
          (setq target_prefix prefix)

          (geocad-update-existing
            doc
            target_prefix
            kolor
            txt-h
            z-prec
            display
          )

          (princ
            (strcat
              "\n[OK] Zapisano i zaktualizowano grupe robocza: "
              target_prefix
              "."
            )
          )
        )
      )

      ;; ------------------------------------------------------
      ;; Status 3:
      ;; hurtowo zapisuje te same parametry jako pamiec wszystkich
      ;; istniejacych grup i aktualizuje wszystkie bloki w rysunku.
      ;; ------------------------------------------------------
      (if (= status 3)
        (progn
          (foreach target_prefix prefix_groups
            (geocad-save-group-settings
              target_prefix
              kolor
              pikt_pref
              styl
              display
              txt-h
              z-prec
              z_tags
            )
          )

          (geocad-update-existing
            doc
            *geocad-all-groups-label*
            kolor
            txt-h
            z-prec
            display
          )

          (princ
            "\n[OK] Zapisano i zaktualizowano wszystkie grupy w rysunku."
          )
        )
      )
    )
    (princ "\n[Anulowano] Nie zapisano zmian.")
  )

  (princ)
)

(princ "\nZaladowano biblioteke: gp_Core.lsp. Wpisz GEO_SETUP aby skonfigurowac.")
(princ)