;; ======================================================
;; GEOPROFICAD - SETUP DIALOG UI
;; ======================================================
;;
;; UI/DCL dla GEO_SETUP.
;; Shadow split: czesc helperow UI jest juz w module UI,
;; reszta geocad-setup-* nadal jest ladowana z gp_CoreLegacy.lsp
;; i bedzie przenoszona stopniowo.
;; ======================================================

(setq *geocad-module-setupdialog-loaded* T)

(setq *geocad-new-group-label* "--- UTWORZ NOWA GRUPE ---")
(setq *geocad-no-pikt-prefix-label* "(bez prefixu)")
(setq *geocad-add-pikt-prefix-label* "--- DODAJ NOWY PREFIX ---")
(setq *geocad-add-pikt-prefix-marker* "__GEOCAD_ADD_PIKT_PREFIX__")

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

(princ)
