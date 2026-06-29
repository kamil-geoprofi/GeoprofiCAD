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

(defun geocad-load-relative (rel / base path)
  (setq base (vl-filename-directory (findfile "gp_Core.lsp")))

  (if base
    (setq path (strcat base "\\" rel))
    (setq path rel)
  )

  (load path (strcat "\nBLAD: Nie znaleziono pliku " rel))
)

;; Na tym etapie runtime zachowuje oryginalne API i zachowanie.
;; Kolejne kroki moga dzielic gp_CoreRuntime.lsp na mniejsze moduly
;; bez zmiany tego publicznego loadera.
(geocad-load-relative "core\\gp_CoreRuntime.lsp")

(princ)
