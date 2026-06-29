;; ======================================================
;; GEOPROFICAD - SETUP MAIN DIALOG HELPERS
;; ======================================================
;;
;; Helpery glownego okna GEO_SETUP.
;; Shadow split: definicje sa zgodne z gp_CoreLegacy.lsp
;; i sa ladowane po legacy, bez zmiany publicznego API.
;; ======================================================

(setq *geocad-module-setupmain-loaded* T)

(defun geocad-setup-refresh-group-popup
  (prefix / prefix-groups prefix-select-prefixes prefix-select-display prefix-select-idx)
  ;; Odswieza liste grup w otwartym glownym GEO_SETUP.
  ;;
  ;; Zwraca:
  ;; (prefix-groups prefix-select-prefixes prefix-select-display prefix-select-idx)

  (setq prefix (geocad-normalize-layer-prefix prefix))

  (if (= prefix "")
    (setq prefix "POMIAR")
  )

  (setq prefix-groups (geocad-get-existing-prefixes))
  (setq prefix-groups (geocad-add-unique-prefix prefix prefix-groups))
  (setq prefix-groups (vl-sort prefix-groups '<))

  (setq prefix-select-prefixes
    (cons "" prefix-groups)
  )

  (setq prefix-select-display
    (cons
      *geocad-new-group-label*
      (geocad-build-prefix-display-list prefix-groups)
    )
  )

  (start_list "prefix_select" 3)
  (mapcar 'add_list prefix-select-display)
  (end_list)

  (setq prefix-select-idx
    (geocad-index-of-string prefix prefix-select-prefixes)
  )

  (if (not prefix-select-idx)
    (setq prefix-select-idx 0)
  )

  (set_tile "prefix_select" (itoa prefix-select-idx))

  (list
    prefix-groups
    prefix-select-prefixes
    prefix-select-display
    prefix-select-idx
  )
)

(princ)
