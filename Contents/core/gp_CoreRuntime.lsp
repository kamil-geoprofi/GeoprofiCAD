(vl-load-com)

;; ======================================================
;; GEOPROFICAD - CORE RUNTIME ORCHESTRATOR
;; ======================================================
;;
;; Techniczny punkt przejscia dla refaktoru core.
;; Kolejnosc ladowania runtime/legacy/modulow jest obecnie jawnie
;; zapisana w PackageContents.xml.
;;
;; Ten plik celowo nie laduje jeszcze podmodulow samodzielnie,
;; zeby nie dublowac ladowania z manifestu bundle.
;; ======================================================

(setq *geocad-core-split-stage* "shadow-split")
(setq *geocad-core-runtime-loaded* T)

(princ)
