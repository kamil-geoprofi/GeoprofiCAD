;; ======================================================
;; GEOPROFICAD - SETUP MAIN DIALOG HELPERS
;; ======================================================
;;
;; Helpery glownego okna GEO_SETUP.
;; Shadow split: definicje sa zgodne z gp_CoreLegacy.lsp
;; i sa ladowane po legacy, bez zmiany publicznego API.
;; ======================================================

(setq *geocad-module-setupmain-loaded* T)

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

(princ)
