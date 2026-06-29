(vl-load-com)

;; ======================================================
;; GEOPROFICAD - CORE LOADER
;; ======================================================
;;
;; Ten plik zostaje jako stabilny punkt wejscia dla:
;; - PackageContents.xml,
;; - gp_Import.lsp,
;; - gp_WstawPikiete.lsp,
;; - pozostalych modulow, ktore laduja gp_Core.lsp.
;;
;; Realna logika core zostala przeniesiona do folderu core/,
;; zeby gp_Core.lsp nie puchl jako monolit.
;; ======================================================

(defun geocad-load-relative (rel / core-file base path)
  (setq core-file (findfile "gp_Core.lsp"))
  (setq base (if core-file (vl-filename-directory core-file) nil))

  (if base
    (setq path (strcat base "\\" rel))
    (setq path rel)
  )

  (load path (strcat "\nBLAD: Nie znaleziono pliku " rel))
)

;; Ten loader ma gwarantowac kompletne publiczne API core takze wtedy,
;; gdy starszy modul albo uzytkownik wczyta tylko gp_Core.lsp recznie.
;; PackageContents.xml nadal moze ladowac moduly osobno, ale gp_Core.lsp
;; nie moze zostawiac funkcji core w stanie czesciowo dostepnym.
(if (not *geocad-core-loader-loaded*)
  (progn
    (setq *geocad-core-loader-loaded* T)

    ;; gp_Config.lsp zawiera stale i helpery warstw uzywane przez core.
    ;; PackageContents.xml laduje go przed gp_Core.lsp, ale reczne
    ;; (load "gp_Core.lsp") powinno dzialac niezaleznie od APPLOAD.
    (if (not geocad-layer-name)
      (geocad-load-relative "gp_Config.lsp")
    )

    (geocad-load-relative "core\\gp_CoreRuntime.lsp")
    (geocad-load-relative "core\\gp_ProjectMemory.lsp")
    (geocad-load-relative "core\\gp_Numbering.lsp")
    (geocad-load-relative "core\\gp_CadObjects.lsp")
    (geocad-load-relative "core\\gp_Workgroups.lsp")
    (geocad-load-relative "core\\gp_PikietaSchema.lsp")
    (geocad-load-relative "core\\gp_TextRadar.lsp")
    (geocad-load-relative "core\\gp_PikietaWriters.lsp")
    (geocad-load-relative "core\\gp_PikietaData.lsp")
    (geocad-load-relative "core\\gp_PikietaFactory.lsp")
    (geocad-load-relative "core\\gp_PikietaConversion.lsp")
  )
)

(princ)
