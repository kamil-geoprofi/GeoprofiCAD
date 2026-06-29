;; ======================================================
;; GEOPROFICAD - PIKIETA SCHEMA
;; ======================================================
;;
;; Centralny kontrakt logicznej pikiety GeoprofiCAD.
;; Tutaj trzymamy nazwe bloku, tagi atrybutow i wartosci domyslne.
;; Inne moduly nie powinny wpisywac recznie "Pikieta_Geo", "NR" ani "H".
;; ======================================================

(setq *geocad-module-pikietaschema-loaded* T)

(setq *geocad-pikieta-block-name* "Pikieta_Geo")
(setq *geocad-pikieta-attr-nr* "NR")
(setq *geocad-pikieta-attr-z-main* "H")
(setq *geocad-pikieta-attr-z-tags* '("H" "Z" "RZEDNA"))
(setq *geocad-pikieta-empty-nr* "---")
(setq *geocad-pikieta-default-z-text* "0.00")

(defun geocad-pikieta-tag-normalize (tag)
  (strcase (if tag (vl-princ-to-string tag) ""))
)

(defun geocad-pikieta-nr-tag-p (tag)
  (=
    (geocad-pikieta-tag-normalize tag)
    (geocad-pikieta-tag-normalize *geocad-pikieta-attr-nr*)
  )
)

(defun geocad-pikieta-z-tag-p (tag)
  (member
    (geocad-pikieta-tag-normalize tag)
    (mapcar 'geocad-pikieta-tag-normalize *geocad-pikieta-attr-z-tags*)
  )
)

(defun geocad-pikieta-z-tags-string (/ result tag)
  (setq result "")
  (foreach tag *geocad-pikieta-attr-z-tags*
    (if (= result "")
      (setq result tag)
      (setq result (strcat result "," tag))
    )
  )
  result
)

(defun geocad-pikieta-empty-nr-if-needed (nr)
  (if
    (or
      (not nr)
      (= (vl-princ-to-string nr) "")
    )
    *geocad-pikieta-empty-nr*
    (vl-princ-to-string nr)
  )
)

(defun geocad-pikieta-block-filter ()
  (list
    '(0 . "INSERT")
    (cons 2 *geocad-pikieta-block-name*)
  )
)

(defun geocad-pikieta-block-layer-filter (layname)
  (append
    (geocad-pikieta-block-filter)
    (list (cons 8 layname))
  )
)

(defun geocad-pikieta-attr-text-by-tags (obj tags / result tag)
  (setq result nil)
  (if obj
    (foreach tag tags
      (if (not result)
        (setq result (geocad-block-attr-text obj tag))
      )
    )
  )
  (if result result "")
)

(defun geocad-pikieta-attr-nr-text (obj)
  (geocad-pikieta-empty-nr-if-needed
    (geocad-block-attr-text obj *geocad-pikieta-attr-nr*)
  )
)

(defun geocad-pikieta-attr-z-text (obj)
  (geocad-pikieta-attr-text-by-tags obj *geocad-pikieta-attr-z-tags*)
)

(princ)
