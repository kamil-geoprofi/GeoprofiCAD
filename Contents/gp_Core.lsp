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
  ;; Jezeli uzytkownik wstawia pikiety bez wchodzenia w GEO_SETUP,
  ;; nadal inicjalizujemy pamiec DWG z realnego rysunku.
  (geocad-ensure-dwg-setup-initialized doc)
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
  ;; Zapisuje parametry aktywnej grupy roboczej.
  ;;
  ;; UWAGA:
  ;; z_tags zostaje w sygnaturze tylko dla kompatybilnosci
  ;; ze starszymi wywolaniami funkcji.
  ;; Tagi rzednych NIE sa juz parametrem grupy roboczej.
  ;; Sa ustawieniem DWG/projektu uzywanym przy rozpoznawaniu Z
  ;; z obcych blokow/obiektow.

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

(defun geocad-best-prefix-from-existing-layers
  (doc / prefixes pref count best best-count)
  ;; Dla starego DWG bez pamieci projektu wybieramy grupe,
  ;; ktora ma najwiecej obiektow na standardowych warstwach GeoprofiCAD.
  (setq prefixes (geocad-get-prefixes-from-layers doc))
  (setq best "")
  (setq best-count -1)

  (foreach pref prefixes
    (setq count (geocad-count-objects-in-group pref))

    (if (> count best-count)
      (progn
        (setq best pref)
        (setq best-count count)
      )
    )
  )

  best
)


(defun geocad-count-pikt-prefix-in-group
  (group-prefix pikt-pref / group pref lay ss i obj nr parsed count)
  ;; Liczy realne bloki Pikieta_Geo w danej grupie,
  ;; ktore maja konkretny prefix numeracji.
  (setq group (geocad-normalize-layer-prefix group-prefix))
  (setq pref (geocad-normalize-pikt-prefix pikt-pref))
  (setq count 0)

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
              (setq count (1+ count))
            )

            (setq i (1+ i))
          )
        )
      )
    )
  )

  count
)


(defun geocad-best-pikt-prefix-for-group
  (group-prefix / prefixes pref count best best-count)
  ;; Dla grupy bez zapamietanego prefixu numeracji wybieramy
  ;; prefix najczesciej wystepujacy w realnych pikietach tej grupy.
  (setq prefixes
    (geocad-get-known-pikt-prefixes-for-group group-prefix "")
  )

  (setq best "")
  (setq best-count -1)

  (foreach pref prefixes
    (setq count
      (geocad-count-pikt-prefix-in-group group-prefix pref)
    )

    (if (> count best-count)
      (progn
        (setq best pref)
        (setq best-count count)
      )
    )
  )

  best
)


(defun geocad-ensure-dwg-setup-initialized
  (doc / saved-prefix inferred-prefix saved-pikt inferred-pikt)
  ;; Inicjalizacja pamieci per DWG.
  ;;
  ;; Nowy pusty rysunek:
  ;; - dostanie POMIAR.
  ;;
  ;; Stary rysunek z warstwami GeoprofiCAD, ale bez LDATA:
  ;; - dostanie aktywna grupe wywnioskowana z istniejacych warstw,
  ;; - dostanie prefix numeracji wywnioskowany z pikiet tej grupy.
  ;;
  ;; Nie uzywamy rejestru Windows.
  (setq saved-prefix (geocad-setup-ldata-get "Prefix"))

  (if saved-prefix
    (setq saved-prefix (geocad-normalize-layer-prefix saved-prefix))
    (setq saved-prefix "")
  )

  (if (= saved-prefix "")
    (progn
      (setq inferred-prefix
        (geocad-best-prefix-from-existing-layers doc)
      )

      (if (= inferred-prefix "")
        (setq inferred-prefix "POMIAR")
      )

      (setq inferred-pikt
        (geocad-group-cfg-read
          inferred-prefix
          "PiktPrefix"
          (geocad-best-pikt-prefix-for-group inferred-prefix)
        )
      )

      ;; Aktywne ustawienia konkretnego DWG.
      (geocad-setup-ldata-put "Prefix" inferred-prefix)
      (geocad-setup-ldata-put "PiktPrefix" inferred-pikt)

      ;; Pamiec grupy w tym DWG.
      (if (/= inferred-pikt "")
        (geocad-save-known-pikt-prefix-for-group inferred-prefix inferred-pikt)
      )
    )
  )
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

(defun geocad-safe-delete-object
  (obj)
  (if obj
    (vl-catch-all-apply
      'vla-Delete
      (list obj)
    )
  )

  nil
)


(defun geocad-object-point-list
  (obj / obj-name)
  (if obj
    (progn
      (setq obj-name (vla-get-ObjectName obj))

      (cond
        ((= obj-name "AcDbPoint")
          (vlax-safearray->list
            (vlax-variant-value
              (vla-get-Coordinates obj)
            )
          )
        )

        (T
          (vlax-safearray->list
            (vlax-variant-value
              (vla-get-InsertionPoint obj)
            )
          )
        )
      )
    )
    nil
  )
)


(defun geocad-set-object-visible
  (obj visible)
  ;; Dziala dla TEXT/POINT/INSERT, jezeli obiekt wspiera wlasciwosc Visible.
  ;; Jezeli dany obiekt jej nie wspiera, ignorujemy blad.
  (if obj
    (vl-catch-all-apply
      'vla-put-Visible
      (list
        obj
        (if visible :vlax-true :vlax-false)
      )
    )
  )

  obj
)


(defun geocad-text-string-or-empty
  (obj / val)
  (setq val "")

  (if obj
    (progn
      (setq val
        (vl-catch-all-apply
          'vla-get-TextString
          (list obj)
        )
      )

      (if (vl-catch-all-error-p val)
        (setq val "")
      )
    )
  )

  (if val val "")
)


(defun geocad-make-text-entity
  (pt txt-h text-value layname visible / ent obj)
  (if (not text-value)
    (setq text-value "")
  )

  (setq ent
    (entmakex
      (list
        '(0 . "TEXT")
        (cons 10 pt)
        (cons 40 txt-h)
        (cons 1 text-value)
        (cons 8 layname)
      )
    )
  )

  (if ent
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (geocad-set-object-visible obj visible)
    )
  )

  ent
)


(defun geocad-make-point-entity
  (pt layname)
  (entmakex
    (list
      '(0 . "POINT")
      (cons 10 pt)
      (cons 8 layname)
    )
  )
)


(defun geocad-find-nearest-text-object
  (layname target-pt tol / ss i obj pt d best best-d)
  ;; Szuka najblizszego TEXT na podanej warstwie.
  ;; Uzywane do parowania tekstow NR/H z punktem pikiety.
  (setq best nil)
  (setq best-d nil)

  (if
    (and
      layname
      (/= layname "")
      target-pt
      (tblsearch "LAYER" layname)
    )
    (progn
      (setq ss
        (ssget
          "_X"
          (list
            '(0 . "TEXT")
            (cons 8 layname)
          )
        )
      )

      (if ss
        (progn
          (setq i 0)

          (while (< i (sslength ss))
            (setq obj (vlax-ename->vla-object (ssname ss i)))
            (setq pt (geocad-object-point-list obj))

            (if pt
              (progn
                (setq d (distance pt target-pt))

                (if
                  (and
                    (<= d tol)
                    (or
                      (not best-d)
                      (< d best-d)
                    )
                  )
                  (progn
                    (setq best obj)
                    (setq best-d d)
                  )
                )
              )
            )

            (setq i (1+ i))
          )
        )
      )
    )
  )

  best
)


(defun geocad-update-text-object
  (obj pt txt-h layname text-value visible)
  (if obj
    (progn
      (vl-catch-all-apply
        'vla-put-Layer
        (list obj layname)
      )

      (vl-catch-all-apply
        'vla-put-Height
        (list obj txt-h)
      )

      (if text-value
        (vl-catch-all-apply
          'vla-put-TextString
          (list obj text-value)
        )
      )

      (vl-catch-all-apply
        'vla-put-InsertionPoint
        (list obj (vlax-3d-point pt))
      )

      (geocad-set-object-visible obj visible)
    )
  )

  obj
)


(defun geocad-create-text-pikieta
  (
    pt-list nr-str txt-h z-prec display
    lay-pt lay-nr lay-h
    /
    px py pz dX dY z-str show-nr show-h
  )
  ;; Tworzy wariant tekstowy pikiety:
  ;; POINT + TEXT NR + TEXT H.
  ;;
  ;; Nowo tworzone pikiety tekstowe zawsze dostaja oba TEXT-y.
  ;; Widocznosc sterujemy przez Visible, zeby pozniej dalo sie je odzyskac
  ;; przy zmianie trybu widocznosci.

  (if
    (or
      (not nr-str)
      (= nr-str "")
    )
    (setq nr-str "---")
  )

  (setq px (car pt-list))
  (setq py (cadr pt-list))
  (setq pz (caddr pt-list))

  (if (not pz)
    (setq pz 0.0)
  )

  (setq pt-list (list px py pz))
  (setq dX (* txt-h 1.2))
  (setq dY (* txt-h 0.7))
  (setq z-str (rtos pz 2 z-prec))

  (setq show-nr
    (if (member display '("Oba" "Numer"))
      T
      nil
    )
  )

  (setq show-h
    (if (member display '("Oba" "Rzedna"))
      T
      nil
    )
  )

  (geocad-make-point-entity pt-list lay-pt)

  (geocad-make-text-entity
    (list (+ px dX) (+ py dY) pz)
    txt-h
    nr-str
    lay-nr
    show-nr
  )

  (geocad-make-text-entity
    (list (+ px dX) (- py dY) pz)
    txt-h
    z-str
    lay-h
    show-h
  )

  T
)


(defun geocad-insert-pikieta-block-from-data
  (
    doc pt-list nr-str txt-h z-prec display
    lay-pt lay-nr lay-h
    /
    space px py pz dX dY z-str pt-3d blkRef vis-nr vis-h tag
  )
  ;; Tworzy blok Pikieta_Geo z danych odczytanych z wariantu tekstowego.

  (if
    (or
      (not nr-str)
      (= nr-str "")
    )
    (setq nr-str "---")
  )

  (geocad-stworz-blok-pikieta)

  (setq space (vla-get-ModelSpace doc))

  (setq px (car pt-list))
  (setq py (cadr pt-list))
  (setq pz (caddr pt-list))

  (if (not pz)
    (setq pz 0.0)
  )

  (setq pt-list (list px py pz))
  (setq pt-3d (vlax-3d-point pt-list))
  (setq dX (* txt-h 1.2))
  (setq dY (* txt-h 0.7))
  (setq z-str (rtos pz 2 z-prec))

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
    (setq tag (strcase (vla-get-TagString att)))

    (cond
      ((= tag "NR")
        (vla-put-TextString att nr-str)
        (vla-put-InsertionPoint
          att
          (vlax-3d-point (list (+ px dX) (+ py dY) pz))
        )
        (vla-put-Invisible att vis-nr)
        (vla-put-Layer att lay-nr)
      )

      ((member tag '("H" "Z" "RZEDNA"))
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

  blkRef
)


(defun geocad-convert-blocks-to-text
  (
    doc prefix kolor txt-h z-prec display
    /
    pref lay-pt lay-nr lay-h ss i obj pt nr count
  )
  ;; Konwersja:
  ;; INSERT Pikieta_Geo -> POINT + TEXT + TEXT.

  (setq pref (geocad-normalize-layer-prefix prefix))
  (setq count 0)

  (if (/= pref "")
    (progn
      (setq lay-pt (geocad-layer-name pref *geocad-layer-type-points*))
      (setq lay-nr (geocad-layer-name pref *geocad-layer-type-label-nr*))
      (setq lay-h  (geocad-layer-name pref *geocad-layer-type-label-h*))

      (geocad-ensure-layer doc lay-pt kolor)
      (geocad-ensure-layer doc lay-nr kolor)
      (geocad-ensure-layer doc lay-h kolor)

      (setq ss
        (ssget
          "_X"
          (list
            '(0 . "INSERT")
            '(2 . "Pikieta_Geo")
            (cons 8 lay-pt)
          )
        )
      )

      (if ss
        (progn
          (setq i 0)

          (while (< i (sslength ss))
            (setq obj (vlax-ename->vla-object (ssname ss i)))
            (setq pt (geocad-object-point-list obj))
            (setq nr (geocad-block-attr-text obj "NR"))

            (if pt
              (progn
                (geocad-create-text-pikieta
                  pt
                  nr
                  txt-h
                  z-prec
                  display
                  lay-pt
                  lay-nr
                  lay-h
                )

                (geocad-safe-delete-object obj)
                (setq count (1+ count))
              )
            )

            (setq i (1+ i))
          )
        )
      )
    )
  )

  count
)


(defun geocad-convert-text-to-blocks
  (
    doc prefix kolor txt-h z-prec display
    /
    pref lay-pt lay-nr lay-h ss i pt-obj pt
    px py pz dX dY tol nr-obj h-obj nr-str count
  )
  ;; Konwersja:
  ;; POINT + TEXT + TEXT -> INSERT Pikieta_Geo.
  ;;
  ;; Uwaga:
  ;; Jezeli stara tekstowa pikieta nie ma tekstu numeru,
  ;; numeru nie da sie odtworzyc. Wtedy blok dostanie NR = "---".

  (setq pref (geocad-normalize-layer-prefix prefix))
  (setq count 0)

  (if (/= pref "")
    (progn
      (setq lay-pt (geocad-layer-name pref *geocad-layer-type-points*))
      (setq lay-nr (geocad-layer-name pref *geocad-layer-type-label-nr*))
      (setq lay-h  (geocad-layer-name pref *geocad-layer-type-label-h*))

      (geocad-ensure-layer doc lay-pt kolor)
      (geocad-ensure-layer doc lay-nr kolor)
      (geocad-ensure-layer doc lay-h kolor)

      (geocad-stworz-blok-pikieta)

      (setq ss
        (ssget
          "_X"
          (list
            '(0 . "POINT")
            (cons 8 lay-pt)
          )
        )
      )

      (if ss
        (progn
          (setq i 0)

          (while (< i (sslength ss))
            (setq pt-obj (vlax-ename->vla-object (ssname ss i)))
            (setq pt (geocad-object-point-list pt-obj))

            (if pt
              (progn
                (setq px (car pt))
                (setq py (cadr pt))
                (setq pz (caddr pt))

                (if (not pz)
                  (setq pz 0.0)
                )

                (setq pt (list px py pz))
                (setq dX (* txt-h 1.2))
                (setq dY (* txt-h 0.7))

                ;; Celowo dosc szeroka tolerancja, bo teksty mogly byc
                ;; utworzone przy innej wysokosci tekstu.
                (setq tol (max 1.0 (* txt-h 8.0)))

                (setq nr-obj
                  (geocad-find-nearest-text-object
                    lay-nr
                    (list (+ px dX) (+ py dY) pz)
                    tol
                  )
                )

                (setq h-obj
                  (geocad-find-nearest-text-object
                    lay-h
                    (list (+ px dX) (- py dY) pz)
                    tol
                  )
                )

                (setq nr-str (geocad-text-string-or-empty nr-obj))

                (if (= nr-str "")
                  (setq nr-str "---")
                )

                (geocad-insert-pikieta-block-from-data
                  doc
                  pt
                  nr-str
                  txt-h
                  z-prec
                  display
                  lay-pt
                  lay-nr
                  lay-h
                )

                (geocad-safe-delete-object nr-obj)
                (geocad-safe-delete-object h-obj)
                (geocad-safe-delete-object pt-obj)

                (setq count (1+ count))
              )
            )

            (setq i (1+ i))
          )
        )
      )
    )
  )

  count
)


(defun geocad-update-text-style-existing
  (
    doc prefix kolor txt-h z-prec display
    /
    pref lay-pt lay-nr lay-h ss i pt-obj pt
    px py pz dX dY tol nr-obj h-obj nr-pt h-pt
    show-nr show-h z-str count nr-str
  )
  ;; Aktualizuje istniejace pikiety tekstowe:
  ;; POINT + TEXT NR + TEXT H.
  ;;
  ;; Nie konwertuje stylu. Tylko poprawia:
  ;; - warstwy,
  ;; - kolor warstw,
  ;; - wysokosc tekstu,
  ;; - pozycje tekstow,
  ;; - widocznosc,
  ;; - tekst rzednej.

  (setq pref (geocad-normalize-layer-prefix prefix))
  (setq count 0)

  (if (/= pref "")
    (progn
      (setq lay-pt (geocad-layer-name pref *geocad-layer-type-points*))
      (setq lay-nr (geocad-layer-name pref *geocad-layer-type-label-nr*))
      (setq lay-h  (geocad-layer-name pref *geocad-layer-type-label-h*))

      (geocad-ensure-layer doc lay-pt kolor)
      (geocad-ensure-layer doc lay-nr kolor)
      (geocad-ensure-layer doc lay-h kolor)

      (geocad-update-managed-layer-colors doc pref kolor)

      (setq show-nr
        (if (member display '("Oba" "Numer"))
          T
          nil
        )
      )

      (setq show-h
        (if (member display '("Oba" "Rzedna"))
          T
          nil
        )
      )

      (setq ss
        (ssget
          "_X"
          (list
            '(0 . "POINT")
            (cons 8 lay-pt)
          )
        )
      )

      (if ss
        (progn
          (setq i 0)

          (while (< i (sslength ss))
            (setq pt-obj (vlax-ename->vla-object (ssname ss i)))
            (setq pt (geocad-object-point-list pt-obj))

            (if pt
              (progn
                (setq px (car pt))
                (setq py (cadr pt))
                (setq pz (caddr pt))

                (if (not pz)
                  (setq pz 0.0)
                )

                (setq pt (list px py pz))
                (setq dX (* txt-h 1.2))
                (setq dY (* txt-h 0.7))
                (setq tol (max 1.0 (* txt-h 8.0)))

                (setq nr-pt (list (+ px dX) (+ py dY) pz))
                (setq h-pt  (list (+ px dX) (- py dY) pz))
                (setq z-str (rtos pz 2 z-prec))

                (vl-catch-all-apply
                  'vla-put-Layer
                  (list pt-obj lay-pt)
                )

                (setq nr-obj
                  (geocad-find-nearest-text-object
                    lay-nr
                    nr-pt
                    tol
                  )
                )

                (setq h-obj
                  (geocad-find-nearest-text-object
                    lay-h
                    h-pt
                    tol
                  )
                )

                (if nr-obj
                  (progn
                    (setq nr-str (geocad-text-string-or-empty nr-obj))

                    (if (= nr-str "")
                      (setq nr-str "---")
                    )

                    (geocad-update-text-object
                      nr-obj
                      nr-pt
                      txt-h
                      lay-nr
                      nr-str
                      show-nr
                    )
                  )
                  (if show-nr
                    (geocad-make-text-entity
                      nr-pt
                      txt-h
                      "---"
                      lay-nr
                      T
                    )
                  )
                )

                (if h-obj
                  (geocad-update-text-object
                    h-obj
                    h-pt
                    txt-h
                    lay-h
                    z-str
                    show-h
                  )
                  (geocad-make-text-entity
                    h-pt
                    txt-h
                    z-str
                    lay-h
                    show-h
                  )
                )

                (setq count (1+ count))
              )
            )

            (setq i (1+ i))
          )
        )
      )
    )
  )

  count
)


(defun geocad-setup-apply-current-group-params
  (
    doc prefix kolor-str txt-h-str z-prec-str styl display
    /
    pref kolor txt-h z-prec
  )
  ;; Glowny auto-apply dla GEO_SETUP.
  ;;
  ;; Po kazdej realnej zmianie parametru:
  ;; - zapis parametru robi funkcja autosave,
  ;; - ta funkcja stosuje aktualny profil do obiektow aktywnej grupy.
  ;;
  ;; Styl = Tekst:
  ;; - bloki sa konwertowane do POINT + TEXT,
  ;; - istniejace tekstowe pikiety sa aktualizowane.
  ;;
  ;; Styl = Blok:
  ;; - tekstowe pikiety sa konwertowane do blokow,
  ;; - bloki sa aktualizowane przez istniejace geocad-update-existing.

  (setq pref (geocad-normalize-layer-prefix prefix))

  (if (= pref "")
    (setq pref "POMIAR")
  )

  (setq kolor (atoi kolor-str))
  (setq txt-h (atof txt-h-str))
  (setq z-prec (atoi z-prec-str))

  (if (= styl "Tekst")
    (progn
      (geocad-convert-blocks-to-text
        doc
        pref
        kolor
        txt-h
        z-prec
        display
      )

      (geocad-update-text-style-existing
        doc
        pref
        kolor
        txt-h
        z-prec
        display
      )
    )
    (progn
      (geocad-convert-text-to-blocks
        doc
        pref
        kolor
        txt-h
        z-prec
        display
      )

      (geocad-update-existing
        doc
        pref
        kolor-str
        txt-h-str
        z-prec-str
        display
      )
    )
  )

  T
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
  ;; Jezeli DWG nie ma jeszcze pamieci GeoprofiCAD,
  ;; inicjalizujemy ja z realnych warstw/pikiet w tym rysunku.
  (geocad-ensure-dwg-setup-initialized doc)
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
          (geocad-best-pikt-prefix-for-group pref)
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

(setq *geocad-new-group-label* "--- UTWORZ NOWA GRUPE ---")
(setq *geocad-no-pikt-prefix-label* "(bez prefixu)")
(setq *geocad-add-pikt-prefix-label* "--- DODAJ NOWY PREFIX ---")
(setq *geocad-add-pikt-prefix-marker* "__GEOCAD_ADD_PIKT_PREFIX__")


(defun geocad-setup-pikt-number-preview
  (group-prefix pikt-pref / pref nr)
  (setq pref (geocad-normalize-pikt-prefix pikt-pref))
  (setq nr
    (geocad-next-number-for-group-pikt-prefix
      group-prefix
      pref
    )
  )

  (if (= pref "")
    (itoa nr)
    (strcat pref (itoa nr))
  )
)


(defun geocad-setup-next-number-status
  (group-prefix pikt-pref)
  (strcat
    "Nastepny numer: "
    (geocad-setup-pikt-number-preview group-prefix pikt-pref)
  )
)


(defun geocad-setup-pikt-prefix-display-label
  (group-prefix pikt-pref / pref)
  (setq pref (geocad-normalize-pikt-prefix pikt-pref))

  (strcat
    (if (= pref "")
      *geocad-no-pikt-prefix-label*
      pref
    )
    "  |  nastepny: "
    (geocad-setup-pikt-number-preview group-prefix pref)
  )
)


(defun geocad-setup-param-prefix-label
  (pikt-pref / pref)
  (setq pref (geocad-normalize-pikt-prefix pikt-pref))

  (if (= pref "")
    *geocad-no-pikt-prefix-label*
    pref
  )
)


(defun geocad-setup-update-param-context
  (group-prefix pikt-pref / group)
  ;; Parametry wygladu sa per GRUPA ROBOCZA.
  ;; Prefix numeracji nie jest czescia profilu parametrow,
  ;; dlatego nie pokazujemy go w tej sekcji.

  (setq group (geocad-normalize-layer-prefix group-prefix))

  (if (= group "")
    (setq group "POMIAR")
  )

  (set_tile "param_context_group" (strcat "Grupa: " group))

  group
)


(defun geocad-setup-clear-param-statuses ()
  ;; Czyscimy statusy tylko przy otwarciu okna albo zmianie grupy.
  ;; Nie wolno czyscic ich przy zmianie innego parametru.
  ;; Nie wolno czyscic ich przy zmianie prefixu numeracji.

  (set_tile "styl_status" " ")
  (set_tile "display_status" " ")
  (set_tile "txt_h_status" " ")
  (set_tile "z_prec_status" " ")
  (set_tile "kolor_status" " ")
  (set_tile "z_tags_status" " ")
)


(defun geocad-setup-color-label
  (color-str / n)
  (setq n (atoi color-str))

  (cond
    ((= n 1) "1 - Czerwony")
    ((= n 2) "2 - Zolty")
    ((= n 3) "3 - Zielony")
    ((= n 4) "4 - Cyjan")
    ((= n 5) "5 - Niebieski")
    ((= n 6) "6 - Magenta")
    ((= n 7) "7 - Czarny/Bialy")
    (T color-str)
  )
)


(defun geocad-setup-style-label
  (styl)
  (if (= styl "Tekst")
    "Zwykly Punkt + Tekst"
    "Inteligentny Blok"
  )
)


(defun geocad-setup-display-label
  (display)
  (cond
    ((= display "Oba") "Oba (Nr + H)")
    ((= display "Numer") "Tylko Numer")
    ((= display "Rzedna") "Tylko Rzedna (H)")
    ((= display "Brak") "Nic (Sam symbol)")
    (T display)
  )
)


(defun geocad-setup-normalize-real-text
  (txt)
  (geocad-trim-string
    (vl-string-translate "," "." txt)
  )
)


(defun geocad-setup-positive-real-from-text
  (txt / s val)
  (setq s (geocad-setup-normalize-real-text txt))
  (setq val (distof s 2))

  (if
    (and
      val
      (> val 0.0)
    )
    val
    nil
  )
)


(defun geocad-setup-digits-only-p
  (txt / s i ok code)
  (setq s (geocad-trim-string txt))
  (setq ok (/= s ""))
  (setq i 1)

  (while
    (and
      ok
      (<= i (strlen s))
    )
    (setq code (ascii (substr s i 1)))

    (if
      (not
        (and
          (>= code 48)
          (<= code 57)
        )
      )
      (setq ok nil)
    )

    (setq i (1+ i))
  )

  ok
)


(defun geocad-setup-save-group-param
  (group-prefix key value / group)
  ;; Zapis pojedynczego parametru grupy.
  ;; Uzywane przez auto-zapis w GEO_SETUP.

  (setq group (geocad-normalize-layer-prefix group-prefix))

  (if (= group "")
    (setq group "POMIAR")
  )

  ;; Aktywny kontekst DWG.
  (geocad-set-cfg key value)

  ;; Pamiec konkretnej grupy.
  (geocad-save-known-prefix group)
  (geocad-group-cfg-write group key value)

  value
)

(defun geocad-setup-save-z-tags
  (old-z-tags / raw norm)
  ;; Tagi rzednych nie sa parametrem grupy.
  ;; To ustawienie DWG/projektu uzywane przy rozpoznawaniu Z
  ;; z obcych blokow/obiektow.

  (if
    (or
      (not old-z-tags)
      (= old-z-tags "")
    )
    (setq old-z-tags "H,Z,RZEDNA")
  )

  (setq raw (get_tile "z_tags"))
  (setq norm (geocad-trim-string raw))

  (if (= norm "")
    (progn
      (set_tile "z_tags" old-z-tags)
      (set_tile "z_tags_status" "BLAD - lista tagow rzednej nie moze byc pusta. Przywrocono poprzednia wartosc.")
      (set_tile "dirty_status" "BLAD - nie zapisano tagow rzednej.")
      old-z-tags
    )
    (progn
      (set_tile "z_tags" norm)

      (if (/= norm old-z-tags)
        (progn
          (geocad-set-cfg "ZTags" norm)
          (set_tile "z_tags_status" (strcat "(Zmieniono " old-z-tags " -> " norm ")"))
          (set_tile "dirty_status" "ZAPISANO tagi rozpoznawania rzednej.")
        )
      )

      norm
    )
  )
)


(defun geocad-setup-try-save-txt-h
  (doc group-prefix old-txt-h kolor z-prec styl display / raw norm val)
  (if
    (or
      (not old-txt-h)
      (= old-txt-h "")
    )
    (setq old-txt-h "1.0")
  )

  (setq raw (get_tile "txt_h"))
  (setq norm (geocad-setup-normalize-real-text raw))
  (setq val (geocad-setup-positive-real-from-text raw))

  (if
    (not val)
    (progn
      (set_tile "txt_h" old-txt-h)
      (set_tile "txt_h_status" "BLAD - wysokosc tekstu musi byc liczba > 0. Przywrocono poprzednia wartosc.")
      (set_tile "dirty_status" "BLAD - nie zapisano wysokosci tekstu.")
      (list nil old-txt-h)
    )
    (progn
      (set_tile "txt_h" norm)

      (if (/= norm old-txt-h)
        (progn
          (geocad-setup-save-group-param group-prefix "TxtH" norm)

          (geocad-setup-apply-current-group-params
            doc
            group-prefix
            kolor
            norm
            z-prec
            styl
            display
          )

          (set_tile "txt_h_status" (strcat "(Zmieniono " old-txt-h " -> " norm ")"))
          (set_tile "dirty_status" (strcat "ZAPISANO wysokosc tekstu i zaktualizowano grupe " group-prefix "."))
        )
      )

      (list T norm)
    )
  )
)


(defun geocad-setup-try-save-z-prec
  (doc group-prefix old-z-prec kolor txt-h styl display / raw norm val)
  (if
    (or
      (not old-z-prec)
      (= old-z-prec "")
    )
    (setq old-z-prec "2")
  )

  (setq raw (get_tile "z_prec"))
  (setq norm (geocad-trim-string raw))

  (if
    (not (geocad-setup-digits-only-p norm))
    (progn
      (set_tile "z_prec" old-z-prec)
      (set_tile "z_prec_status" "BLAD - precyzja Z musi byc liczba calkowita 0-8. Przywrocono poprzednia wartosc.")
      (set_tile "dirty_status" "BLAD - nie zapisano precyzji Z.")
      (list nil old-z-prec)
    )
    (progn
      (setq val (atoi norm))

      (if
        (or
          (< val 0)
          (> val 8)
        )
        (progn
          (set_tile "z_prec" old-z-prec)
          (set_tile "z_prec_status" "BLAD - precyzja Z musi byc w zakresie 0-8. Przywrocono poprzednia wartosc.")
          (set_tile "dirty_status" "BLAD - nie zapisano precyzji Z.")
          (list nil old-z-prec)
        )
        (progn
          (setq norm (itoa val))
          (set_tile "z_prec" norm)

          (if (/= norm old-z-prec)
            (progn
              (geocad-setup-save-group-param group-prefix "Prec" norm)

              (geocad-setup-apply-current-group-params
                doc
                group-prefix
                kolor
                txt-h
                norm
                styl
                display
              )

              (set_tile "z_prec_status" (strcat "(Zmieniono " old-z-prec " -> " norm ")"))
              (set_tile "dirty_status" (strcat "ZAPISANO precyzje Z i zaktualizowano grupe " group-prefix "."))
            )
          )

          (list T norm)
        )
      )
    )
  )
)


(defun geocad-setup-autosave-style
  (doc group-prefix old-styl kolor txt-h z-prec display / idx new-styl)
  (setq idx (get_tile "styl_rys"))

  (setq new-styl
    (if (= idx "1")
      "Tekst"
      "Blok"
    )
  )

  (if (/= new-styl old-styl)
    (progn
      (geocad-setup-save-group-param group-prefix "Styl" new-styl)

      (geocad-setup-apply-current-group-params
        doc
        group-prefix
        kolor
        txt-h
        z-prec
        new-styl
        display
      )

      (set_tile
        "styl_status"
        (strcat
          "(Zmieniono "
          (geocad-setup-style-label old-styl)
          " -> "
          (geocad-setup-style-label new-styl)
          ")"
        )
      )

      (set_tile "dirty_status" (strcat "ZAPISANO styl i przekonwertowano grupe " group-prefix "."))
    )
  )

  new-styl
)


(defun geocad-setup-autosave-display
  (doc group-prefix old-display kolor txt-h z-prec styl / idx new-display)
  (setq idx (get_tile "display_mode"))
  (setq new-display (geocad-display-from-popup-index idx))

  (if (/= new-display old-display)
    (progn
      (geocad-setup-save-group-param group-prefix "Display" new-display)

      (geocad-setup-apply-current-group-params
        doc
        group-prefix
        kolor
        txt-h
        z-prec
        styl
        new-display
      )

      (set_tile
        "display_status"
        (strcat
          "(Zmieniono "
          (geocad-setup-display-label old-display)
          " -> "
          (geocad-setup-display-label new-display)
          ")"
        )
      )

      (set_tile "dirty_status" (strcat "ZAPISANO widocznosc i zaktualizowano grupe " group-prefix "."))
    )
  )

  new-display
)


(defun geocad-setup-autosave-color
  (doc group-prefix old-color txt-h z-prec styl display / idx new-color)
  (setq idx (atoi (get_tile "kolor")))
  (setq new-color (itoa (1+ idx)))

  (if (/= new-color old-color)
    (progn
      (geocad-setup-save-group-param group-prefix "Color" new-color)

      (geocad-setup-apply-current-group-params
        doc
        group-prefix
        new-color
        txt-h
        z-prec
        styl
        display
      )

      (set_tile
        "kolor_status"
        (strcat
          "(Zmieniono "
          (geocad-setup-color-label old-color)
          " -> "
          (geocad-setup-color-label new-color)
          ")"
        )
      )

      (set_tile "dirty_status" (strcat "ZAPISANO kolor i zaktualizowano grupe " group-prefix "."))
    )
  )

  new-color
)


(defun geocad-setup-refresh-pikt-prefix-popup
  (group-prefix current-pikt-pref / group pref prefixes select-prefixes display idx)
  ;; Popup prefixow numeracji ma 3 logiczne czesci:
  ;; 1. (bez prefixu),
  ;; 2. istniejace prefixy,
  ;; 3. --- DODAJ NOWY PREFIX ---.
  ;;
  ;; Zwraca:
  ;; (select-prefixes display idx)

  (setq group (geocad-normalize-layer-prefix group-prefix))
  (setq pref (geocad-normalize-pikt-prefix current-pikt-pref))

  (if (= group "")
    (setq group "POMIAR")
  )

  (setq prefixes
    (geocad-get-known-pikt-prefixes-for-group
      group
      pref
    )
  )

  (setq select-prefixes
    (append
      (list "")
      prefixes
      (list *geocad-add-pikt-prefix-marker*)
    )
  )

  (setq display
    (append
      (list
        (geocad-setup-pikt-prefix-display-label group "")
      )
      (geocad-build-pikt-prefix-display-list group prefixes)
      (list *geocad-add-pikt-prefix-label*)
    )
  )

  (start_list "pikt_pref_select" 3)
  (mapcar 'add_list display)
  (end_list)

  (setq idx
    (geocad-index-of-string pref select-prefixes)
  )

  (if (not idx)
    (setq idx 0)
  )

  (set_tile "pikt_pref_select" (itoa idx))
  (set_tile "next_number_status" (geocad-setup-next-number-status group pref))

  (list select-prefixes display idx)
)


(defun geocad-setup-pikt-prefix-exists-for-group
  (group-prefix pikt-pref / group pref prefixes)
  (setq group (geocad-normalize-layer-prefix group-prefix))
  (setq pref (geocad-normalize-pikt-prefix pikt-pref))

  (if
    (or
      (= group "")
      (= pref "")
    )
    nil
    (progn
      (setq prefixes
        (geocad-get-known-pikt-prefixes-for-group
          group
          ""
        )
      )

      (if (member pref prefixes)
        T
        nil
      )
    )
  )
)


(defun geocad-setup-activate-pikt-prefix
  (group-prefix pikt-pref / group pref)
  ;; Aktywuje prefix numeracji od razu.
  ;; To nie jest zmiana parametrow wizualnych grupy,
  ;; wiec nie wymaga zadnego przycisku zapisu.

  (setq group (geocad-normalize-layer-prefix group-prefix))
  (setq pref (geocad-normalize-pikt-prefix pikt-pref))

  (if (= group "")
    (setq group "POMIAR")
  )

  ;; Aktywny kontekst DWG.
  (geocad-set-cfg "Prefix" group)
  (geocad-set-cfg "PiktPrefix" pref)

  ;; Pamiec grupy.
  ;; Pusty prefix zapisujemy jako aktywny PiktPrefix grupy,
  ;; ale nie dopisujemy go do listy znanych prefixow.
  (geocad-group-cfg-write group "PiktPrefix" pref)

  (if (/= pref "")
    (geocad-save-known-pikt-prefix-for-group group pref)
  )

  pref
)


(defun geocad-setup-save-active-values
  (values / prefix pikt-pref kolor txt-h z-prec styl display)
  ;; Zapisuje aktywny kontekst DWG.
  ;; Nie dotyka ZTags, bo Tagi rzednych nie sa parametrem grupy.

  (setq prefix (nth 0 values))
  (setq pikt-pref (nth 1 values))
  (setq kolor (nth 2 values))
  (setq txt-h (nth 3 values))
  (setq z-prec (nth 4 values))
  (setq styl (nth 5 values))
  (setq display (nth 6 values))

  (geocad-set-cfg "Styl" styl)
  (geocad-set-cfg "Display" display)
  (geocad-set-cfg "TxtH" txt-h)
  (geocad-set-cfg "Prec" z-prec)
  (geocad-set-cfg "Prefix" prefix)
  (geocad-set-cfg "PiktPrefix" pikt-pref)
  (geocad-set-cfg "Color" kolor)

  values
)


(defun geocad-setup-load-group-to-main-dialog
  (prefix / doc pref fallback-color kolor pikt-pref styl display txt-h z-prec values)
  ;; Laduje parametry grupy do glownego dialogu.
  ;; Nie laduje ZTags, bo Tagi rzednych nie sa parametrem grupy.

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (geocad-ensure-dwg-setup-initialized doc)

  (setq pref (geocad-normalize-layer-prefix prefix))

  (if (= pref "")
    (setq pref "POMIAR")
  )

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
      (geocad-best-pikt-prefix-for-group pref)
    )
  )

  (setq pikt-pref (geocad-normalize-pikt-prefix pikt-pref))

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

  (set_tile "txt_h" txt-h)
  (set_tile "z_prec" z-prec)
  (set_tile "kolor" (geocad-popup-color-index kolor))
  (set_tile "styl_rys" (geocad-popup-styl-index styl))
  (set_tile "display_mode" (geocad-popup-display-index display))

  (setq values
    (list
      pref
      pikt-pref
      kolor
      txt-h
      z-prec
      styl
      display
    )
  )

  values
)


(defun geocad-setup-create-new-group
  (prefix pikt-pref / pref pikt txt-h z-prec kolor styl display z-tags)
  ;; Tworzy nowa grupe robocza i od razu ustawia ja jako aktywna.
  ;;
  ;; Parametry wizualne nowej grupy startuja z aktualnych ustawien DWG.
  ;; Prefix numeracji trafia tu tylko wtedy, jezeli uzytkownik wpisal go
  ;; w dialogu tworzenia nowej grupy.
  ;;
  ;; z-tags pobieramy tylko jako argument kompatybilnosci dla
  ;; geocad-save-group-settings, ale funkcja juz nie zapisuje ZTags
  ;; jako parametru grupy.

  (setq pref (geocad-normalize-layer-prefix prefix))
  (setq pikt (geocad-normalize-pikt-prefix pikt-pref))

  (if (/= pref "")
    (progn
      (setq txt-h (geocad-get-cfg "TxtH" "1.0"))
      (setq z-prec (geocad-get-cfg "Prec" "2"))
      (setq kolor (geocad-get-cfg "Color" "3"))
      (setq styl (geocad-get-cfg "Styl" "Blok"))
      (setq display (geocad-get-cfg "Display" "Oba"))
      (setq z-tags (geocad-get-cfg "ZTags" "H,Z,RZEDNA"))

      ;; Aktywny kontekst DWG.
      (geocad-set-cfg "Prefix" pref)
      (geocad-set-cfg "PiktPrefix" pikt)
      (geocad-set-cfg "Styl" styl)
      (geocad-set-cfg "Display" display)
      (geocad-set-cfg "TxtH" txt-h)
      (geocad-set-cfg "Prec" z-prec)
      (geocad-set-cfg "Color" kolor)

      ;; Pamiec nowej grupy.
      (geocad-save-group-settings
        pref
        kolor
        pikt
        styl
        display
        txt-h
        z-prec
        z-tags
      )

      pref
    )
    nil
  )
)


(defun geocad-setup-show-new-group-dialog
  (/ dcl-file dcl-fn dcl-id status new-prefix new-pikt-pref created-prefix)
  ;; Osobny dialog tworzenia nowej grupy.
  ;;
  ;; Zwraca:
  ;; - prefix utworzonej grupy,
  ;; - nil, jezeli anulowano.

  (setq new-prefix "")
  (setq new-pikt-pref "")
  (setq created-prefix nil)

  (setq dcl-file (vl-filename-mktemp "geosetup_new_group.dcl"))
  (setq dcl-fn (open dcl-file "w"))

  (write-line "GeoNewGroup : dialog { label = \"GEO_SETUP - Nowa grupa robocza\";" dcl-fn)

  (write-line "  : boxed_column { label = \"Nowa grupa\";" dcl-fn)
  (write-line "    : edit_box { key = \"new_prefix\"; label = \"Prefix nowej grupy (np. DROGI):\"; edit_width = 24; }" dcl-fn)
  (write-line "    : edit_box { key = \"new_pikt_pref\"; label = \"Prefix numeracji, opcjonalnie (np. dr_):\"; edit_width = 20; }" dcl-fn)
  (write-line "  }" dcl-fn)

  (write-line "  : boxed_column { label = \"Status\";" dcl-fn)
  (write-line "    : text { key = \"new_status\"; label = \"Wpisz prefix nowej grupy.\"; }" dcl-fn)
  (write-line "  }" dcl-fn)

  (write-line "  : row { alignment = centered;" dcl-fn)
  (write-line "    : button { key = \"create_group\"; label = \"Utworz nowa grupe\"; is_default = true; }" dcl-fn)
  (write-line "    : button { key = \"cancel\"; label = \"Anuluj\"; is_cancel = true; }" dcl-fn)
  (write-line "  }" dcl-fn)

  (write-line "}" dcl-fn)
  (close dcl-fn)

  (setq dcl-id (load_dialog dcl-file))

  (if
    (not (new_dialog "GeoNewGroup" dcl-id))
    (progn
      (alert "Blad ladowania okna DCL nowej grupy.")
      (exit)
    )
  )

  (set_tile "new_prefix" "")
  (set_tile "new_pikt_pref" "")
  (set_tile "new_status" "Wpisz prefix nowej grupy.")
  (mode_tile "new_prefix" 2)

  (action_tile
    "new_prefix"
    "(set_tile \"new_status\" \"Wpisz prefix nowej grupy i kliknij Utworz nowa grupe.\")"
  )

  (action_tile
    "new_pikt_pref"
    "(set_tile \"new_status\" \"Prefix numeracji jest opcjonalny.\")"
  )

  (action_tile
    "create_group"
    "(setq new-prefix (geocad-normalize-layer-prefix (get_tile \"new_prefix\"))) (setq new-pikt-pref (geocad-normalize-pikt-prefix (get_tile \"new_pikt_pref\"))) (cond ((= new-prefix \"\") (set_tile \"new_status\" \"BLAD - wpisz poprawny prefix grupy.\")) ((member new-prefix (geocad-get-existing-prefixes)) (set_tile \"new_status\" \"BLAD - taka grupa juz istnieje. Wybierz ja z listy.\")) (T (done_dialog 1)))"
  )

  (action_tile "cancel" "(done_dialog 0)")

  (setq status (start_dialog))
  (unload_dialog dcl-id)
  (vl-file-delete dcl-file)

  (if (= status 1)
    (progn
      (setq created-prefix
        (geocad-setup-create-new-group
          new-prefix
          new-pikt-pref
        )
      )

      (if created-prefix
        (princ
          (strcat
            "\n[OK] Utworzono i aktywowano grupe robocza: "
            created-prefix
            "."
          )
        )
      )
    )
  )

  created-prefix
)


(defun geocad-setup-show-new-pikt-prefix-dialog
  (group-prefix / group dcl-file dcl-fn dcl-id status new-pikt-pref created-prefix)
  ;; Osobny dialog tworzenia nowego prefixu numeracji dla aktywnej grupy.
  ;;
  ;; Zwraca:
  ;; - prefix utworzony i aktywowany,
  ;; - nil, jezeli anulowano.

  (setq group (geocad-normalize-layer-prefix group-prefix))

  (if (= group "")
    (setq group "POMIAR")
  )

  (setq new-pikt-pref "")
  (setq created-prefix nil)

  (setq dcl-file (vl-filename-mktemp "geosetup_new_pikt_prefix.dcl"))
  (setq dcl-fn (open dcl-file "w"))

  (write-line "GeoNewPiktPrefix : dialog { label = \"GEO_SETUP - Nowy prefix numeracji\";" dcl-fn)

  (write-line "  : boxed_column { label = \"Aktywna grupa\";" dcl-fn)
  (write-line (strcat "    : text { label = \"" group "\"; }") dcl-fn)
  (write-line "  }" dcl-fn)

  (write-line "  : boxed_column { label = \"Nowy prefix numeracji\";" dcl-fn)
  (write-line "    : edit_box { key = \"new_pikt_pref\"; label = \"Prefix numeracji (np. dr_):\"; edit_width = 20; }" dcl-fn)
  (write-line "  }" dcl-fn)

  (write-line "  : boxed_column { label = \"Status\";" dcl-fn)
  (write-line "    : text { key = \"new_status\"; label = \"Wpisz nowy prefix numeracji.\"; }" dcl-fn)
  (write-line "  }" dcl-fn)

  (write-line "  : row { alignment = centered;" dcl-fn)
  (write-line "    : button { key = \"create_pikt_prefix\"; label = \"Dodaj prefix\"; is_default = true; }" dcl-fn)
  (write-line "    : button { key = \"cancel\"; label = \"Anuluj\"; is_cancel = true; }" dcl-fn)
  (write-line "  }" dcl-fn)

  (write-line "}" dcl-fn)
  (close dcl-fn)

  (setq dcl-id (load_dialog dcl-file))

  (if
    (not (new_dialog "GeoNewPiktPrefix" dcl-id))
    (progn
      (alert "Blad ladowania okna DCL nowego prefixu numeracji.")
      (exit)
    )
  )

  (set_tile "new_pikt_pref" "")
  (set_tile "new_status" "Wpisz nowy prefix numeracji.")
  (mode_tile "new_pikt_pref" 2)

  (action_tile
    "new_pikt_pref"
    "(set_tile \"new_status\" \"Wpisz nowy prefix i kliknij Dodaj prefix.\")"
  )

  (action_tile
    "create_pikt_prefix"
    "(setq new-pikt-pref (geocad-normalize-pikt-prefix (get_tile \"new_pikt_pref\"))) (cond ((= new-pikt-pref \"\") (set_tile \"new_status\" \"BLAD - pusty prefix. Dla braku prefixu wybierz opcje (bez prefixu).\")) ((geocad-setup-pikt-prefix-exists-for-group group new-pikt-pref) (set_tile \"new_status\" \"BLAD - taki prefix juz istnieje dla tej grupy.\")) (T (done_dialog 1)))"
  )

  (action_tile "cancel" "(done_dialog 0)")

  (setq status (start_dialog))
  (unload_dialog dcl-id)
  (vl-file-delete dcl-file)

  (if (= status 1)
    (progn
      (geocad-save-known-pikt-prefix-for-group group new-pikt-pref)
      (geocad-setup-activate-pikt-prefix group new-pikt-pref)
      (setq created-prefix new-pikt-pref)

      (princ
        (strcat
          "\n[OK] Dodano i aktywowano prefix numeracji: "
          created-prefix
          " dla grupy "
          group
          "."
        )
      )
    )
  )

  created-prefix
)


(defun geocad-setup-refresh-group-popup
  (prefix / prefix-groups prefix-select-prefixes prefix-select-display prefix-select-idx)
  ;; Odswieza liste grup w otwartym glownym GEO_SETUP.
  ;;
  ;; Zwraca:
  ;; (prefix-groups prefix-select-prefixes prefix-select-display prefix-select-idx)

  (setq prefix (geocad-normalize-layer-prefix prefix))

  (if (= prefix "")
    (setq prefix "POMIAR")
  )

  (setq prefix-groups (geocad-get-existing-prefixes))
  (setq prefix-groups (geocad-add-unique-prefix prefix prefix-groups))
  (setq prefix-groups (vl-sort prefix-groups '<))

  (setq prefix-select-prefixes
    (cons "" prefix-groups)
  )

  (setq prefix-select-display
    (cons
      *geocad-new-group-label*
      (geocad-build-prefix-display-list prefix-groups)
    )
  )

  (start_list "prefix_select" 3)
  (mapcar 'add_list prefix-select-display)
  (end_list)

  (setq prefix-select-idx
    (geocad-index-of-string prefix prefix-select-prefixes)
  )

  (if (not prefix-select-idx)
    (setq prefix-select-idx 0)
  )

  (set_tile "prefix_select" (itoa prefix-select-idx))

  (list
    prefix-groups
    prefix-select-prefixes
    prefix-select-display
    prefix-select-idx
  )
)


(defun geocad-setup-show-main-dialog
  (
    doc
    /
    txt-h z-prec prefix pikt_pref kolor styl display z-tags
    dcl-file dcl-fn dcl-id status
    col-idx styl-idx disp-idx
    prefix_groups
    prefix_select_prefixes prefix_select_display prefix_select_idx
    pikt_prefix_bundle
    pikt_prefix_select_prefixes pikt_prefix_select_display pikt_prefix_select_idx
    selected_pikt_prefix
    group_popup_bundle
    save_result validate_result active_result
    target_prefix
    saved_in_dialog
  )

  ;; ------------------------------------------------------
  ;; Odczyt ustawien:
  ;; 1. pamiec konkretnego DWG,
  ;; 2. default.
  ;; ------------------------------------------------------
  (setq txt-h (geocad-get-cfg "TxtH" "1.0")
        z-prec (geocad-get-cfg "Prec" "2")
        prefix (geocad-get-cfg "Prefix" "POMIAR")
        pikt_pref (geocad-get-cfg "PiktPrefix" "")
        kolor (geocad-get-cfg "Color" "3")
        styl (geocad-get-cfg "Styl" "Blok")
        display (geocad-get-cfg "Display" "Oba")
        z-tags (geocad-get-cfg "ZTags" "H,Z,RZEDNA")
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

  (setq prefix_select_prefixes
    (cons "" prefix_groups)
  )

  (setq prefix_select_display
    (cons
      *geocad-new-group-label*
      (geocad-build-prefix-display-list prefix_groups)
    )
  )

  ;; ------------------------------------------------------
  ;; DCL glownego okna
  ;; ------------------------------------------------------
  (setq dcl-file (vl-filename-mktemp "geosetup.dcl"))
  (setq dcl-fn (open dcl-file "w"))

  (write-line "GeoSetup : dialog { label = \"GEO_SETUP - GeoprofiCAD\";" dcl-fn)

  (write-line "  : boxed_column { label = \"Grupa robocza\";" dcl-fn)
  (write-line "    : popup_list { key = \"prefix_select\"; label = \"Aktywna grupa:\"; width = 52; }" dcl-fn)
  (write-line "  }" dcl-fn)

  (write-line "  : boxed_column { label = \"Prefix numeru pikiety\";" dcl-fn)
  (write-line "    : popup_list { key = \"pikt_pref_select\"; label = \"Aktywny prefix:\"; width = 48; }" dcl-fn)
  (write-line "    : text { key = \"next_number_status\"; label = \"Nastepny numer: -\"; }" dcl-fn)
  (write-line "  }" dcl-fn)

  (write-line "  : boxed_column { label = \"Parametry aktywnej grupy\";" dcl-fn)
  (write-line "    : text { key = \"param_context_group\"; label = \"Grupa: -\"; }" dcl-fn)

  (write-line "    : popup_list { key = \"styl_rys\"; label = \"Styl na mapie:\"; list = \"Inteligentny Blok\\nZwykly Punkt + Tekst\"; }" dcl-fn)
  (write-line "    : text { key = \"styl_status\"; label = \" \"; }" dcl-fn)

  (write-line "    : popup_list { key = \"display_mode\"; label = \"Widocznosc:\"; list = \"Oba (Nr + H)\\nTylko Numer\\nTylko Rzedna (H)\\nNic (Sam symbol)\"; }" dcl-fn)
  (write-line "    : text { key = \"display_status\"; label = \" \"; }" dcl-fn)

  (write-line "    : edit_box { key = \"txt_h\"; label = \"Wysokosc tekstu:\"; edit_width = 8; }" dcl-fn)
  (write-line "    : text { key = \"txt_h_status\"; label = \" \"; }" dcl-fn)

  (write-line "    : edit_box { key = \"z_prec\"; label = \"Miejsca po przecinku (Z):\"; edit_width = 8; }" dcl-fn)
  (write-line "    : text { key = \"z_prec_status\"; label = \" \"; }" dcl-fn)

  (write-line "    : popup_list { key = \"kolor\"; label = \"Kolor podstawowy:\"; list = \"1 - Czerwony\\n2 - Zolty\\n3 - Zielony\\n4 - Cyjan\\n5 - Niebieski\\n6 - Magenta\\n7 - Czarny/Bialy\"; }" dcl-fn)
  (write-line "    : text { key = \"kolor_status\"; label = \" \"; }" dcl-fn)
  (write-line "  }" dcl-fn)

  (write-line "  : boxed_column { label = \"Rozpoznawanie rzednej z obiektow\";" dcl-fn)
  (write-line "    : edit_box { key = \"z_tags\"; label = \"Tagi atrybutow Z:\"; edit_width = 24; }" dcl-fn)
  (write-line "    : text { key = \"z_tags_status\"; label = \" \"; }" dcl-fn)
  (write-line "  }" dcl-fn)

  (write-line "  : boxed_column { label = \"Status\";" dcl-fn)
  (write-line "    : text { key = \"dirty_status\"; label = \"Brak zmian.\"; }" dcl-fn)
  (write-line "  }" dcl-fn)

  (write-line "  : boxed_column { label = \"Zasada dzialania\";" dcl-fn)
  (write-line "    : text { label = \"Zmiana parametru zapisuje ustawienie i od razu aktualizuje aktywna grupe.\"; }" dcl-fn)
  (write-line "    : text { label = \"Zmiana stylu konwertuje istniejace pikiety aktywnej grupy.\"; }" dcl-fn)
  (write-line "  }" dcl-fn)

  (write-line "  : row { alignment = centered;" dcl-fn)
  (write-line "    : button { key = \"cancel\"; label = \"Zamknij\"; is_cancel = true; }" dcl-fn)
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

  (setq prefix_select_idx
    (geocad-index-of-string prefix prefix_select_prefixes)
  )

  (if (not prefix_select_idx)
    (setq prefix_select_idx 0)
  )

  (set_tile "prefix_select" (itoa prefix_select_idx))

  ;; ------------------------------------------------------
  ;; Wypelnienie pol parametrow.
  ;; ------------------------------------------------------
  (set_tile "txt_h" txt-h)
  (set_tile "z_prec" z-prec)
  (set_tile "z_tags" z-tags)

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

  ;; Kontekst parametrow.
  (geocad-setup-update-param-context prefix pikt_pref)
  (geocad-setup-clear-param-statuses)

  ;; ------------------------------------------------------
  ;; Wypelnienie listy prefixow numeracji dla aktualnej grupy.
  ;; ------------------------------------------------------
  (setq pikt_prefix_bundle
    (geocad-setup-refresh-pikt-prefix-popup prefix pikt_pref)
  )
  (setq pikt_prefix_select_prefixes (car pikt_prefix_bundle))
  (setq pikt_prefix_select_display (cadr pikt_prefix_bundle))
  (setq pikt_prefix_select_idx (caddr pikt_prefix_bundle))

  ;; Status poczatkowy.
  (setq saved_in_dialog nil)
  (set_tile
    "dirty_status"
    (strcat
      "AKTYWNA GRUPA: "
      prefix
      ". Aktywny prefix: "
      (geocad-setup-param-prefix-label pikt_pref)
      "."
    )
  )

  ;; ------------------------------------------------------
  ;; Akcje DCL.
  ;; ------------------------------------------------------

  ;; Wybor grupy:
  ;; - indeks 0 otwiera osobny dialog tworzenia nowej grupy,
  ;; - istniejaca grupa aktywuje sie natychmiast.
  (action_tile
    "prefix_select"
    "(setq prefix_select_idx (atoi (get_tile \"prefix_select\"))) (if (= prefix_select_idx 0) (done_dialog 10) (progn (setq prefix (nth prefix_select_idx prefix_select_prefixes)) (setq active_result (geocad-setup-load-group-to-main-dialog prefix)) (setq prefix (nth 0 active_result)) (setq pikt_pref (nth 1 active_result)) (setq kolor (nth 2 active_result)) (setq txt-h (nth 3 active_result)) (setq z-prec (nth 4 active_result)) (setq styl (nth 5 active_result)) (setq display (nth 6 active_result)) (setq active_result (geocad-setup-save-active-values active_result)) (geocad-setup-update-param-context prefix pikt_pref) (geocad-setup-clear-param-statuses) (setq pikt_prefix_bundle (geocad-setup-refresh-pikt-prefix-popup prefix pikt_pref)) (setq pikt_prefix_select_prefixes (car pikt_prefix_bundle)) (setq pikt_prefix_select_display (cadr pikt_prefix_bundle)) (setq pikt_prefix_select_idx (caddr pikt_prefix_bundle)) (setq saved_in_dialog T) (set_tile \"dirty_status\" (strcat \"AKTYWNA GRUPA: \" prefix \". Aktywny prefix: \" (geocad-setup-param-prefix-label pikt_pref) \".\"))))"
  )

  ;; Wybor prefixu numeracji:
  ;; - (bez prefixu) dziala od razu,
  ;; - istniejacy prefix dziala od razu,
  ;; - --- DODAJ NOWY PREFIX --- otwiera osobny dialog.
  ;;
  ;; Prefix numeracji NIE jest profilem parametrow wygladu,
  ;; wiec zmiana prefixu NIE czysci statusow parametrow.
  (action_tile
    "pikt_pref_select"
    "(setq pikt_prefix_select_idx (atoi (get_tile \"pikt_pref_select\"))) (setq selected_pikt_prefix (nth pikt_prefix_select_idx pikt_prefix_select_prefixes)) (if (= selected_pikt_prefix *geocad-add-pikt-prefix-marker*) (done_dialog 11) (progn (setq pikt_pref (geocad-setup-activate-pikt-prefix prefix selected_pikt_prefix)) (setq pikt_prefix_bundle (geocad-setup-refresh-pikt-prefix-popup prefix pikt_pref)) (setq pikt_prefix_select_prefixes (car pikt_prefix_bundle)) (setq pikt_prefix_select_display (cadr pikt_prefix_bundle)) (setq pikt_prefix_select_idx (caddr pikt_prefix_bundle)) (geocad-setup-update-param-context prefix pikt_pref) (setq saved_in_dialog T) (set_tile \"dirty_status\" (strcat \"AKTYWNY PREFIX NUMERACJI: \" (geocad-setup-param-prefix-label pikt_pref) \".\"))))"
  )

    ;; Auto-zapis parametrow grupy.
  ;; Kazda realna zmiana:
  ;; - zapisuje parametr,
  ;; - od razu aktualizuje/konwertuje aktywna grupe.

  (action_tile
    "txt_h"
    "(setq save_result (geocad-setup-try-save-txt-h doc prefix txt-h kolor z-prec styl display)) (setq txt-h (cadr save_result))"
  )

  (action_tile
    "z_prec"
    "(setq save_result (geocad-setup-try-save-z-prec doc prefix z-prec kolor txt-h styl display)) (setq z-prec (cadr save_result))"
  )

  (action_tile
    "styl_rys"
    "(setq styl (geocad-setup-autosave-style doc prefix styl kolor txt-h z-prec display))"
  )

  (action_tile
    "display_mode"
    "(setq display (geocad-setup-autosave-display doc prefix display kolor txt-h z-prec styl))"
  )

  (action_tile
    "kolor"
    "(setq kolor (geocad-setup-autosave-color doc prefix kolor txt-h z-prec styl display))"
  )

  ;; ZTags to ustawienie rozpoznawania rzednej z obcych obiektow.
  ;; Nie aktualizuje pikiet.
  (action_tile
    "z_tags"
    "(setq z-tags (geocad-setup-save-z-tags z-tags))"
  )

  (action_tile "cancel" "(done_dialog 0)")

  (setq status (start_dialog))
  (unload_dialog dcl-id)
  (vl-file-delete dcl-file)

  ;; ------------------------------------------------------
  ;; Status specjalny:
  ;; 10 = otworz osobny dialog tworzenia nowej grupy.
  ;; 11 = otworz osobny dialog tworzenia nowego prefixu numeracji.
  ;; ------------------------------------------------------
  (if
    (not (member status '(10 11)))
    (progn
      ;; ------------------------------------------------------
      ;; Zapis po akcji.
      ;;
      ;; status 0 = Zamknij
      ;; status 2 = Aktualizuj te grupe
      ;; status 3 = Aktualizuj wszystkie grupy
      ;; ------------------------------------------------------
      (if
        (member status '(2 3))
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

          (setq prefix (geocad-normalize-layer-prefix prefix))
          (setq pikt_pref (geocad-normalize-pikt-prefix pikt_pref))

          (if (= prefix "")
            (setq prefix "POMIAR")
          )

          ;; Utrwalamy aktualny stan aktywnej grupy.
          ;; ZTags nie sa juz parametrem grupy, ale przekazujemy z-tags
          ;; dla kompatybilnosci sygnatury geocad-save-group-settings.
          (geocad-set-cfg "Styl" styl)
          (geocad-set-cfg "Display" display)
          (geocad-set-cfg "TxtH" txt-h)
          (geocad-set-cfg "Prec" z-prec)
          (geocad-set-cfg "Prefix" prefix)
          (geocad-set-cfg "PiktPrefix" pikt_pref)
          (geocad-set-cfg "Color" kolor)

          (geocad-save-group-settings
            prefix
            kolor
            pikt_pref
            styl
            display
            txt-h
            z-prec
            z-tags
          )

          ;; ------------------------------------------------------
          ;; Status 2:
          ;; aktualizacja obiektow tej samej grupy.
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
                  "\n[OK] Zaktualizowano grupe robocza: "
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
          ;;
          ;; Uwaga:
          ;; Nie nadpisujemy prefixu numeracji wszystkich grup aktywnym
          ;; pikt_pref. Kazda grupa zachowuje swoj PiktPrefix.
          ;; ------------------------------------------------------
          (if (= status 3)
            (progn
              (foreach target_prefix prefix_groups
                (geocad-save-group-settings
                  target_prefix
                  kolor
                  (geocad-group-cfg-read
                    target_prefix
                    "PiktPrefix"
                    (geocad-best-pikt-prefix-for-group target_prefix)
                  )
                  styl
                  display
                  txt-h
                  z-prec
                  z-tags
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
                "\n[OK] Zaktualizowano wszystkie grupy w rysunku."
              )
            )
          )
        )
        (if saved_in_dialog
          (princ "\n[Zamknieto] Okno zamknieto po wczesniejszej aktywacji/zapisie.")
          (princ "\n[Zamknieto] Zamknieto GEO_SETUP.")
        )
      )
    )
  )

  status
)


(defun c:GEO_SETUP
  (/ doc status created-prefix created-pikt-prefix continue)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; Jezeli DWG nie ma jeszcze pamieci GeoprofiCAD,
  ;; inicjalizujemy ja z realnych warstw/pikiet w tym rysunku.
  (geocad-ensure-dwg-setup-initialized doc)

  (setq continue T)

  (while continue
    (setq status (geocad-setup-show-main-dialog doc))

    (cond
      ;; Uzytkownik wybral "--- UTWORZ NOWA GRUPE ---".
      ((= status 10)
        (setq created-prefix (geocad-setup-show-new-group-dialog))

        ;; Po utworzeniu albo anulowaniu wracamy do glownego GEO_SETUP.
        ;; Jezeli utworzono grupe, jest juz aktywna w pamieci DWG.
        (setq continue T)
      )

      ;; Uzytkownik wybral "--- DODAJ NOWY PREFIX ---".
      ((= status 11)
        (setq created-pikt-prefix
          (geocad-setup-show-new-pikt-prefix-dialog
            (geocad-get-cfg "Prefix" "POMIAR")
          )
        )

        ;; Po dodaniu albo anulowaniu wracamy do glownego GEO_SETUP.
        ;; Jezeli dodano prefix, jest juz aktywny w pamieci DWG.
        (setq continue T)
      )

      ;; Kazdy inny status konczy prace komendy.
      (T
        (setq continue nil)
      )
    )
  )

  (princ)
)

(princ "\nZaladowano biblioteke: gp_Core.lsp. Wpisz GEO_SETUP aby skonfigurowac.")
(princ)