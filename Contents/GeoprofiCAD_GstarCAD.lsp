;; GeoprofiCAD - loader for GstarCAD (LISP / Visual LISP)
;; Keep AutoCAD's PackageContents.xml unchanged.
;;
;; Installation:
;; 1. Add the GeoprofiCAD/Contents directory to GstarCAD's Support File
;;    Search Path AND Trusted Locations (OPTIONS > Files).
;; 2. APPLOAD this file; optionally register it in Startup Suite.
;; 3. Load Contents/GeoprofiCAD.cuix separately using CUILOAD if desired.
;;
;; Required support path: gp_Import.lsp, gp_Export.lsp and several tools
;; explicitly call (load "gp_Core.lsp"), so an APPLOAD of this file
;; alone is not enough without the Support File Search Path.

(vl-load-com)

(defun geocad-gstarcad-load
  (/ source base modules core-files missing rel full result ready)
  (if *geocad-gstarcad-loader-loaded*
    (princ "\n[GeoprofiCAD] Moduly juz wczytane w tym rysunku.")
    (progn
      (setq source (findfile "GeoprofiCAD_GstarCAD.lsp"))
      (if (not source)
        (princ
          "\n[GeoprofiCAD] Nie znaleziono loadera przez findfile. Dodaj Contents do Support File Search Path."
        )
        (progn
          (setq base (vl-filename-directory source))

          ;; Load in dependency order, corresponding to the effective
          ;; bottom-to-top order of the AutoCAD PackageContents.xml.
          ;; gp_Core.lsp itself loads the core/ modules listed below.
          (setq modules
            '(
              "gp_Config.lsp"
              "gp_Core.lsp"
              "ui/gp_SetupDialog.lsp"
              "ui/gp_SetupAutosave.lsp"
              "ui/gp_SetupPrefix.lsp"
              "ui/gp_SetupGroup.lsp"
              "ui/gp_SetupMain.lsp"
              "gp_Import.lsp"
              "gp_Export.lsp"
              "core/gp_ExportRadarCompat.lsp"
              "gp_WstawPikiete.lsp"
              "gp_Spadki.lsp"
              "gp_NiwelacjaOsi.lsp"
              "gp_NiwelacjaKrawedzi.lsp"
              "gp_NiwelacjaMulti.lsp"
              "gp_SiatkaPunktow.lsp"
              "gp_SiatkaPionowa.lsp"
              "gp_PomiarSpadku.lsp"
              "gp_UsunPunktyZMulti.lsp"
              "gp_UsunPikietyZakres.lsp"
            )
          )

          ;; Preflight every dependency before evaluating any source.
          (setq core-files
            '(
              "core/gp_CoreRuntime.lsp"
              "core/gp_ProjectMemory.lsp"
              "core/gp_Numbering.lsp"
              "core/gp_CadObjects.lsp"
              "core/gp_Workgroups.lsp"
              "core/gp_PikietaSchema.lsp"
              "core/gp_TextRadar.lsp"
              "core/gp_PikietaWriters.lsp"
              "core/gp_PikietaData.lsp"
              "core/gp_PikietaFactory.lsp"
              "core/gp_PikietaConversion.lsp"
            )
          )

          (setq missing nil)
          (foreach rel (append modules core-files)
            (setq full (strcat base "/" rel))
            (if (not (findfile full))
              (setq missing (cons rel missing))
            )
          )

          (if missing
            (progn
              (princ "\n[GeoprofiCAD] Brak wymaganych plikow:")
              (foreach rel (reverse missing)
                (princ (strcat "\n  - " rel))
              )
              (princ "\nZachowaj pelna strukture katalogu Contents.")
            )
            (progn
              (setq ready T)
              (foreach rel modules
                (if ready
                  (progn
                    (setq full (strcat base "/" rel))
                    (setq result (vl-catch-all-apply 'load (list full)))
                    (if (vl-catch-all-error-p result)
                      (progn
                        (setq ready nil)
                        (princ
                          (strcat
                            "\n[GeoprofiCAD] Blad ladowania " rel ": "
                            (vl-catch-all-error-message result)
                          )
                        )
                      )
                    )
                  )
                )
              )
              (if ready
                (progn
                  (setq *geocad-gstarcad-loader-loaded* T)
                  (princ
                    "\n[GeoprofiCAD] LISP wczytany. Test: GEO_SETUP, WSTAW_PIKIETE, NIWELACJA_OSI."
                  )
                )
                (princ
                  "\n[GeoprofiCAD] Ladowanie przerwane. Sprawdz komunikat wyzej."
                )
              )
            )
          )
        )
      )
    )
  )
  (princ)
)

(geocad-gstarcad-load)
(princ)
