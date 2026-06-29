;; ======================================================
;; GEOPROFICAD - CAD OBJECTS
;; ======================================================
;;
;; Niskopoziomowe helpery AutoCAD/VLAX.
;; Shadow split: pelna implementacja nadal jest w gp_CoreLegacy.lsp.
;; Ten plik jest ladowany po legacy i docelowo przejmie ponizsze API:
;;
;; - geocad-block-attr-text
;; - get-pt-from-obj
;; - geocad-ensure-layer
;; - geocad-safe-delete-object
;; - geocad-object-point-list
;; - geocad-set-object-visible
;; - geocad-text-string-or-empty
;; - geocad-make-text-entity
;; - geocad-make-point-entity
;; - geocad-find-nearest-text-object
;; - geocad-update-text-object
;;
;; Na tym commicie nie zmieniamy definicji funkcji.
;; ======================================================

(princ)
