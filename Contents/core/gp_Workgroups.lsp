;; ======================================================
;; GEOPROFICAD - WORKGROUPS / SETUP STATE
;; ======================================================
;;
;; Pamiec grup roboczych, skaner istniejacych warstw GeoprofiCAD
;; i helpery inicjalizacji ustawien DWG.
;; Wydzielone z gp_CoreLegacy.lsp.
;; ======================================================

(setq *geocad-module-workgroups-loaded* T)

(setq *geocad-new-group-label* "--- wpisz recznie / nowa grupa ---")
(setq *geocad-all-groups-label* "--- WSZYSTKIE W RYSUNKU ---")

(defun geocad-index-of-string (val lst / idx result)
  (setq idx 0)
  (setq result nil)
  (while (and lst (not result))
    (if (= val (car lst))
      (setq result idx)
      (progn
        (setq idx (1+ idx))
        (setq lst (cdr lst))
      )
    )
  )
  result
)

(defun geocad-safe-atoi (val default / n)
  (setq n (atoi (if val val "")))
  (if (= n 0) default n)
)

(defun geocad-group-cfg-key (prefix key)
  (strcat
    "Group."
    (geocad-normalize-layer-prefix prefix)
    "."
    key
  )
)

(defun geocad-group-cfg-read (prefix key default / val)
  (setq val
    (geocad-setup-ldata-get
      (geocad-group-cfg-key prefix key)
    )
  )
  (if val val default)
)


(defun geocad-group-type (prefix)
  (geocad-group-cfg-read prefix "GroupType" "generated")
)

(defun geocad-imported-group-p (prefix)
  (= (strcase (geocad-group-type prefix)) "IMPORTED")
)


(defun geocad-imported-group-unlocked-p (prefix)
  (= (geocad-group-cfg-read prefix "ImportedUnlocked" "0") "1")
)

(defun geocad-set-imported-group-unlocked (prefix flag)
  (geocad-group-cfg-write
    prefix
    "ImportedUnlocked"
    (if flag "1" "0")
  )
)

(defun geocad-group-cfg-write (prefix key value)
  (if
    (and
      prefix
      (/= (geocad-normalize-layer-prefix prefix) "")
    )
    (geocad-setup-ldata-put
      (geocad-group-cfg-key prefix key)
      value
    )
  )
  value
)

(defun geocad-get-saved-prefixes (/ raw result pref)
  (setq result '())
  (setq raw (geocad-setup-ldata-get "KnownPrefixes"))
  (if (and raw (listp raw))
    (foreach pref raw
      (setq result (geocad-add-unique-prefix pref result))
    )
  )
  result
)

(defun geocad-save-known-prefix (prefix / pref lst)
  (setq pref (geocad-normalize-layer-prefix prefix))
  (if (/= pref "")
    (progn
      (setq lst (geocad-get-saved-prefixes))
      (setq lst (geocad-add-unique-prefix pref lst))
      (geocad-setup-ldata-put "KnownPrefixes" lst)
    )
  )
  pref
)

(defun geocad-save-group-settings
  (prefix kolor pikt_pref styl display txt-h z-prec z_tags / pref)
  ;; z_tags zostaje w sygnaturze tylko dla kompatybilnosci.
  ;; Tagi rzednych nie sa parametrem grupy roboczej.
  (setq pref (geocad-normalize-layer-prefix prefix))
  (if (/= pref "")
    (progn
      (geocad-save-known-prefix pref)
      (geocad-group-cfg-write pref "Color" kolor)
      (geocad-group-cfg-write pref "PiktPrefix" pikt_pref)
      (geocad-group-cfg-write pref "Styl" styl)
      (geocad-group-cfg-write pref "Display" display)
      (geocad-group-cfg-write pref "TxtH" txt-h)
      (geocad-group-cfg-write pref "Prec" z-prec)
      (if (not (geocad-group-cfg-read pref "GroupType" nil))
        (geocad-group-cfg-write pref "GroupType" "generated")
      )
      (geocad-save-known-pikt-prefix-for-group pref pikt_pref)
    )
  )
  pref
)

(defun geocad-layer-object-if-exists (layers layname / res)
  (if (and layers layname (/= layname ""))
    (progn
      (setq res
        (vl-catch-all-apply
          'vla-Item
          (list layers layname)
        )
      )
      (if (vl-catch-all-error-p res) nil res)
    )
    nil
  )
)

(defun geocad-layer-color-if-exists (doc layname / layers lay)
  (setq layers (vla-get-Layers doc))
  (setq lay (geocad-layer-object-if-exists layers layname))
  (if lay (itoa (vla-get-Color lay)) nil)
)

(defun geocad-group-layer-color (doc prefix fallback / color)
  (setq prefix (geocad-normalize-layer-prefix prefix))
  (setq color
    (geocad-layer-color-if-exists
      doc
      (geocad-layer-name prefix *geocad-layer-type-points*)
    )
  )
  (if (not color)
    (setq color
      (geocad-layer-color-if-exists
        doc
        (geocad-layer-name prefix *geocad-layer-type-label-nr*)
      )
    )
  )
  (if (not color)
    (setq color
      (geocad-layer-color-if-exists
        doc
        (geocad-layer-name prefix *geocad-layer-type-label-h*)
      )
    )
  )
  (if (not color)
    (setq color
      (geocad-layer-color-if-exists
        doc
        (geocad-layer-name prefix *geocad-layer-type-polyline-multi*)
      )
    )
  )
  (if color color fallback)
)

(defun geocad-count-objects-on-layer (layname / ss)
  (if
    (and layname (/= layname "") (tblsearch "LAYER" layname))
    (progn
      (setq ss (ssget "_X" (list (cons 8 layname))))
      (if ss (sslength ss) 0)
    )
    0
  )
)

(defun geocad-count-objects-in-group (prefix / pref count)
  (setq pref (geocad-normalize-layer-prefix prefix))
  (setq count 0)
  (if (/= pref "")
    (progn
      (setq count (+ count (geocad-count-objects-on-layer (geocad-layer-name pref *geocad-layer-type-points*))))
      (setq count (+ count (geocad-count-objects-on-layer (geocad-layer-name pref *geocad-layer-type-label-nr*))))
      (setq count (+ count (geocad-count-objects-on-layer (geocad-layer-name pref *geocad-layer-type-label-h*))))
      (setq count (+ count (geocad-count-objects-on-layer (geocad-layer-name pref *geocad-layer-type-polyline-multi*))))
    )
  )
  count
)

(defun geocad-prefix-display-label (prefix / pref count)
  (setq pref (geocad-normalize-layer-prefix prefix))
  (setq count (geocad-count-objects-in-group pref))
  (strcat pref "  |  obiekty: " (itoa count))
)

(defun geocad-build-prefix-display-list (prefixes / result pref)
  (setq result '())
  (foreach pref prefixes
    (setq result
      (append result (list (geocad-prefix-display-label pref)))
    )
  )
  result
)

(defun geocad-get-prefixes-from-layers (doc / layers lay-obj lay pref lst)
  (setq lst '())
  (setq layers (vla-get-Layers doc))
  (vlax-for lay-obj layers
    (setq lay (vla-get-Name lay-obj))
    (setq pref (geocad-managed-layer-prefix-from-name lay))
    (if pref
      (setq lst (geocad-add-unique-prefix pref lst))
    )
  )
  lst
)

(defun geocad-get-existing-prefixes (/ doc lst saved pref current)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq lst '())
  (foreach pref (geocad-get-prefixes-from-layers doc)
    (setq lst (geocad-add-unique-prefix pref lst))
  )
  (setq saved (geocad-get-saved-prefixes))
  (foreach pref saved
    (setq lst (geocad-add-unique-prefix pref lst))
  )
  (setq current
    (geocad-normalize-layer-prefix
      (geocad-get-cfg "Prefix" "POMIAR")
    )
  )
  (if (= current "")
    (setq current "POMIAR")
  )
  (setq lst (geocad-add-unique-prefix current lst))
  (vl-sort lst '<)
)

(defun geocad-best-prefix-from-existing-layers (doc / prefixes pref count best best-count)
  (setq prefixes (geocad-get-prefixes-from-layers doc))
  (setq best "")
  (setq best-count -1)
  (foreach pref prefixes
    (setq count (geocad-count-objects-in-group pref))
    (if (> count best-count)
      (progn
        (setq best pref)
        (setq best-count count)
      )
    )
  )
  best
)

(defun geocad-count-pikt-prefix-in-group
  (group-prefix pikt-pref / group pref lay ss i obj nr parsed count)
  (setq group (geocad-normalize-layer-prefix group-prefix))
  (setq pref (geocad-normalize-pikt-prefix pikt-pref))
  (setq count 0)
  (if (/= group "")
    (progn
      (setq lay (geocad-layer-name group *geocad-layer-type-points*))
      (setq ss
        (ssget
          "_X"
          (geocad-pikieta-block-layer-filter lay)
        )
      )
      (if ss
        (progn
          (setq i 0)
          (while (< i (sslength ss))
            (setq obj (vlax-ename->vla-object (ssname ss i)))
            (setq nr (geocad-pikieta-attr-nr-text obj))
            (setq parsed (geocad-split-pikieta-number nr))
            (if (and parsed (= (car parsed) pref))
              (setq count (1+ count))
            )
            (setq i (1+ i))
          )
        )
      )
    )
  )
  count
)

(defun geocad-best-pikt-prefix-for-group (group-prefix / prefixes pref count best best-count)
  (setq prefixes (geocad-get-known-pikt-prefixes-for-group group-prefix ""))
  (setq best "")
  (setq best-count -1)
  (foreach pref prefixes
    (setq count (geocad-count-pikt-prefix-in-group group-prefix pref))
    (if (> count best-count)
      (progn
        (setq best pref)
        (setq best-count count)
      )
    )
  )
  best
)

(defun geocad-ensure-dwg-setup-initialized
  (doc / saved-prefix inferred-prefix saved-pikt inferred-pikt)
  (setq saved-prefix (geocad-setup-ldata-get "Prefix"))
  (if saved-prefix
    (setq saved-prefix (geocad-normalize-layer-prefix saved-prefix))
    (setq saved-prefix "")
  )
  (if (= saved-prefix "")
    (progn
      (setq inferred-prefix (geocad-best-prefix-from-existing-layers doc))
      (if (= inferred-prefix "")
        (setq inferred-prefix "POMIAR")
      )
      (setq inferred-pikt
        (geocad-group-cfg-read
          inferred-prefix
          "PiktPrefix"
          (geocad-best-pikt-prefix-for-group inferred-prefix)
        )
      )
      (geocad-setup-ldata-put "Prefix" inferred-prefix)
      (geocad-setup-ldata-put "PiktPrefix" inferred-pikt)
      (if (/= inferred-pikt "")
        (geocad-save-known-pikt-prefix-for-group inferred-prefix inferred-pikt)
      )
    )
  )
)

(defun geocad-update-layer-color-if-exists (layers layname kolor / lay)
  (setq lay (geocad-layer-object-if-exists layers layname))
  (if lay
    (vl-catch-all-apply
      'vla-put-Color
      (list lay kolor)
    )
  )
)

(defun geocad-update-managed-layer-colors (doc prefix kolor / layers pref)
  (setq pref (geocad-normalize-layer-prefix prefix))
  (if (and doc pref (/= pref ""))
    (progn
      (setq layers (vla-get-Layers doc))
      (geocad-update-layer-color-if-exists layers (geocad-layer-name pref *geocad-layer-type-points*) kolor)
      (geocad-update-layer-color-if-exists layers (geocad-layer-name pref *geocad-layer-type-label-nr*) kolor)
      (geocad-update-layer-color-if-exists layers (geocad-layer-name pref *geocad-layer-type-label-h*) kolor)
      (geocad-update-layer-color-if-exists layers (geocad-layer-name pref *geocad-layer-type-polyline-multi*) kolor)
    )
  )
)

(princ)
