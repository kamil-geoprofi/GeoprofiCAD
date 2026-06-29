;; ======================================================
;; GEOPROFICAD - PIKIETA FACTORY
;; ======================================================
;;
;; Tworzenie i batchowe wstawianie pikiet.
;; Shadow split: pelna implementacja nadal jest w gp_CoreLegacy.lsp.
;; Ten plik jest ladowany po legacy i docelowo przejmie ponizsze API:
;;
;; - geocad-stworz-blok-pikieta
;; - geocad-ctx-get
;; - geocad-ctx-set
;; - geocad-pikieta-prepare-context
;; - geocad-wstaw-pikiete-with-context
;; - geocad-pikieta-batch-start
;; - geocad-pikieta-batch-insert
;; - geocad-pikieta-batch-end
;; - geocad-wstaw-pikiete-full
;;
;; Status: mapa API. Definicje funkcji sa jeszcze w gp_CoreLegacy.lsp.
;; ======================================================

(setq *geocad-module-pikietafactory-loaded* T)
(princ)
