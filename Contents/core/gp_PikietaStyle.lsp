;; ======================================================
;; GEOPROFICAD - PIKIETA STYLE
;; ======================================================
;;
;; Konwersje Blok/Tekst i aktualizacja istniejacych pikiet.
;; Shadow split: pelna implementacja nadal jest w gp_CoreLegacy.lsp.
;; Ten plik jest ladowany po legacy i docelowo przejmie ponizsze API:
;;
;; - geocad-create-text-pikieta
;; - geocad-insert-pikieta-block-from-data
;; - geocad-convert-blocks-to-text
;; - geocad-convert-text-to-blocks
;; - geocad-update-text-style-existing
;; - geocad-setup-apply-current-group-params
;; - geocad-update-existing
;;
;; Status: mapa API. Definicje funkcji sa jeszcze w gp_CoreLegacy.lsp.
;; ======================================================

(setq *geocad-module-pikietastyle-loaded* T)
(princ)
