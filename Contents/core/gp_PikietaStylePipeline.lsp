;; ======================================================
;; GEOPROFICAD - PIKIETA STYLE PIPELINE
;; ======================================================
;;
;; Finalne nadpisania operacji stylu pikiet.
;; Logika konwersji jest oparta o gp_PikietaData.lsp:
;; odczyt DWG do pamieci -> zapis stylu docelowego -> kasowanie zrodel.
;; ======================================================

(setq *geocad-module-pikietastylepipeline-loaded* T)

(defun geocad-convert-blocks-to-text
  (doc prefix kolor txt-h z-prec display)
  ;; INSERT Pikieta_Geo -> POINT + TEXT + TEXT.
  ;; Blok jest pelnym zrodlem danych, wiec radar tekstow nie jest tu uzywany.
  (geocad-pikieta-convert-blocks-to-text
    doc
    prefix
    kolor
    txt-h
    z-prec
    display
  )
)

(defun geocad-convert-text-to-blocks
  (doc prefix kolor txt-h z-prec display)
  ;; POINT + TEXT + TEXT -> INSERT Pikieta_Geo.
  ;; Radar tekstow jest uzywany tylko przy odczycie wariantu tekstowego.
  (geocad-pikieta-convert-text-to-blocks
    doc
    prefix
    kolor
    txt-h
    z-prec
    display
  )
)

(defun geocad-update-text-style-existing
  (doc prefix kolor txt-h z-prec display)
  ;; Aktualizacja istniejacego wariantu tekstowego bez konwersji.
  (geocad-pikieta-update-text-data
    doc
    prefix
    kolor
    txt-h
    z-prec
    display
  )
)

(defun geocad-setup-apply-current-group-params
  (
    doc prefix kolor-str txt-h-str z-prec-str styl display
    /
    pref kolor txt-h z-prec converted-count
  )
  ;; Auto-apply dla GEO_SETUP.
  ;; Po udanej konwersji nie robimy drugiego pelnego update'u tej samej grupy.

  (setq pref (geocad-normalize-layer-prefix prefix))
  (if (= pref "")
    (setq pref "POMIAR")
  )

  (setq kolor (atoi kolor-str))
  (setq txt-h (atof txt-h-str))
  (setq z-prec (atoi z-prec-str))

  (if (= styl "Tekst")
    (progn
      (setq converted-count
        (geocad-convert-blocks-to-text
          doc
          pref
          kolor
          txt-h
          z-prec
          display
        )
      )

      ;; Jezeli blokow nie bylo, grupa byla juz tekstowa.
      (if (= converted-count 0)
        (geocad-update-text-style-existing
          doc
          pref
          kolor
          txt-h
          z-prec
          display
        )
      )
    )
    (progn
      (setq converted-count
        (geocad-convert-text-to-blocks
          doc
          pref
          kolor
          txt-h
          z-prec
          display
        )
      )

      ;; Jezeli tekstowych pikiet nie bylo, grupa byla juz blokowa.
      (if (= converted-count 0)
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
  )

  T
)

(princ)
