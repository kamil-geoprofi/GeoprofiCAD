;; ======================================================
;; GEOPROFICAD - EXPORT RADAR COMPAT
;; ======================================================
;;
;; Kompatybilne wrappery dla starych helperow gp_Export.lsp.
;; Plik laduje sie po gp_Export.lsp i przekierowuje stara logike
;; radaru tekstow na wspolne funkcje core/gp_TextRadar.lsp.
;; ======================================================

(setq *geocad-module-exportradarcompat-loaded* T)

(defun dist-2d (p1 p2)
  (geocad-dist-2d p1 p2)
)

(defun categorize-text (txt)
  (geocad-text-radar-categorize txt)
)

(defun get-dist-to-txt (pt t-item)
  (geocad-text-radar-distance pt t-item)
)

(princ)
