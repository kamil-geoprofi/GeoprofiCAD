(vl-load-com)

;; ======================================================
;; GEOPROFICAD - CORE RUNTIME ORCHESTRATOR
;; ======================================================
;;
;; Ten plik zostaje jako techniczny punkt przejscia dla
;; refaktoru core. Docelowo bedzie ladowal male moduly core.
;;
;; Na obecnym etapie pelna, nierozdzielona implementacja
;; jest zachowana w gp_CoreLegacy.lsp i ladowana bezposrednio
;; z PackageContents.xml. Dzieki temu nie zmieniamy zachowania
;; komend podczas czystego podzialu plikow.
;; ======================================================

(princ)
