;; ======================================================
;; GEOPROFICAD - NUMBERING
;; ======================================================
;;
;; Prefixy i liczniki numeracji pikiet.
;; Shadow split: definicje sa zgodne z gp_CoreLegacy.lsp
;; i sa ladowane po legacy, bez zmiany publicznego API.
;; ======================================================

(setq *geocad-module-numbering-loaded* T)
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

(princ)
