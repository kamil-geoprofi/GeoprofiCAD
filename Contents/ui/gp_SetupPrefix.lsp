;; ======================================================
;; GEOPROFICAD - SETUP PREFIX UI HELPERS
;; ======================================================
;;
;; Prefixy numeracji w dialogu GEO_SETUP.
;; Shadow split: definicje sa zgodne z gp_CoreLegacy.lsp
;; i sa ladowane po legacy, bez zmiany publicznego API.
;; ======================================================

(setq *geocad-module-setupprefix-loaded* T)

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

  (if (geocad-imported-group-p group)
    (setq pref "")
  )

  (setq prefixes
    (if (geocad-imported-group-p group)
      '()
      (geocad-get-known-pikt-prefixes-for-group
        group
        pref
      )
    )
  )

  (setq select-prefixes
    (if (geocad-imported-group-p group)
      (list "")
      (append
        (list "")
        prefixes
        (list *geocad-add-pikt-prefix-marker*)
      )
    )
  )

  (setq display
    (if (geocad-imported-group-p group)
      (list "imported: nazwy pikiet pochodza z pliku TXT")
      (append
        (list
          (geocad-setup-pikt-prefix-display-label group "")
        )
        (geocad-build-pikt-prefix-display-list group prefixes)
        (list *geocad-add-pikt-prefix-label*)
      )
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
  (if (geocad-imported-group-p group)
    (progn
      (mode_tile "pikt_pref_select" 1)
      (set_tile "next_number_status" "Imported: prefix numeracji wylaczony")
    )
    (progn
      (mode_tile "pikt_pref_select" 0)
      (set_tile "next_number_status" (geocad-setup-next-number-status group pref))
    )
  )

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

  (if (geocad-imported-group-p group)
    (setq pref "")
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

(princ)
