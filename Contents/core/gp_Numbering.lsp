;; ======================================================
;; GEOPROFICAD - NUMBERING
;; ======================================================
;;
;; Prefixy i liczniki numeracji pikiet.
;; Shadow split: pelna implementacja nadal jest w gp_CoreLegacy.lsp.
;; Ten plik jest ladowany po legacy i docelowo przejmie ponizsze API:
;;
;; - geocad-normalize-pikt-prefix
;; - geocad-pikt-prefix-token
;; - geocad-pikt-prefixes-key
;; - geocad-pikt-counter-key
;; - geocad-add-unique-pikt-prefix
;; - geocad-get-saved-pikt-prefixes-for-group
;; - geocad-save-known-pikt-prefix-for-group
;; - geocad-split-pikieta-number
;; - geocad-scan-pikt-prefixes-from-group
;; - geocad-get-known-pikt-prefixes-for-group
;; - geocad-max-number-in-group-for-pikt-prefix
;; - geocad-get-pikt-counter
;; - geocad-set-pikt-counter
;; - geocad-next-number-for-group-pikt-prefix
;; - GP:PobierzNastepnyNumer
;;
;; Status: mapa API. Definicje funkcji sa jeszcze w gp_CoreLegacy.lsp.
;; ======================================================

(setq *geocad-module-numbering-loaded* T)
(princ)
