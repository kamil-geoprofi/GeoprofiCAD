(vl-load-com)

;; ======================================================
;; GEOPROFICAD - CORE RUNTIME ORCHESTRATOR
;; ======================================================
;;
;; Techniczny punkt przejscia dla refaktoru core.
;; Kolejnosc ladowania runtime/legacy/modulow jest obecnie jawnie
;; zapisana w PackageContents.xml, bo AutoCAD bundle pewniej
;; obsluguje sciezki modulow z manifestu niz findfile/load.
;;
;; Ten plik celowo nie laduje jeszcze podmodulow samodzielnie.
;; ======================================================

(setq *geocad-core-split-stage* "shadow-split")

(princ)
