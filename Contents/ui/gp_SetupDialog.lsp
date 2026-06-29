;; ======================================================
;; GEOPROFICAD - SETUP DIALOG UI
;; ======================================================
;;
;; UI/DCL dla GEO_SETUP.
;; Shadow split: punkt wejscia c:GEO_SETUP jest juz w module UI.
;; Helpery geocad-setup-* nadal sa ladowane z gp_CoreLegacy.lsp
;; i beda przenoszone w kolejnym kroku.
;; ======================================================

(setq *geocad-module-setupdialog-loaded* T)

(setq *geocad-new-group-label* "--- UTWORZ NOWA GRUPE ---")
(setq *geocad-no-pikt-prefix-label* "(bez prefixu)")
(setq *geocad-add-pikt-prefix-label* "--- DODAJ NOWY PREFIX ---")
(setq *geocad-add-pikt-prefix-marker* "__GEOCAD_ADD_PIKT_PREFIX__")

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
