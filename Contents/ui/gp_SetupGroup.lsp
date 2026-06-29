;; ======================================================
;; GEOPROFICAD - SETUP GROUP UI HELPERS
;; ======================================================
;;
;; Ladowanie grupy do GEO_SETUP oraz male dialogi tworzenia
;; nowej grupy i nowego prefixu numeracji.
;; Shadow split: definicje sa zgodne z gp_CoreLegacy.lsp
;; i sa ladowane po legacy, bez zmiany publicznego API.
;; ======================================================

(setq *geocad-module-setupgroup-loaded* T)

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

(princ)
