;; ======================================================
;; GEOPROFICAD - PIKIETA DATA PIPELINE
;; ======================================================
;;
;; Wspolny model posredni pikiety.
;; Zasada:
;; 1. odczytaj obiekty DWG do listy PikietaData,
;; 2. utworz docelowy wariant z danych w pamieci,
;; 3. dopiero na koncu usun obiekty zrodlowe.
;;
;; To eliminuje mieszanie odczytu, zapisu i kasowania w jednej petli.
;; ======================================================

(setq *geocad-module-pikietadata-loaded* T)

(defun geocad-pikieta-data-make
  (pt nr z source point-obj nr-obj h-obj block-obj)
  (list
    (cons 'pt pt)
    (cons 'nr nr)
    (cons 'z z)
    (cons 'source source)
    (cons 'point-obj point-obj)
    (cons 'nr-obj nr-obj)
    (cons 'h-obj h-obj)
    (cons 'block-obj block-obj)
  )
)

(defun geocad-pikieta-data-get (item key)
  (cdr (assoc key item))
)

(defun geocad-pikieta-data-safe-z (val fallback / parsed clean)
  (setq parsed nil)
  (if val
    (progn
      (setq clean
        (vl-string-trim
          " mM\r\n\t"
          (vl-string-translate "," "." (vl-princ-to-string val))
        )
      )
      (setq parsed (distof clean))
    )
  )
  (if parsed parsed fallback)
)

(defun geocad-pikieta-data-block-z
  (obj fallback / z-txt z-val)
  (setq z-txt (geocad-block-attr-text obj "H"))
  (if (= z-txt "")
    (setq z-txt (geocad-block-attr-text obj "Z"))
  )
  (if (= z-txt "")
    (setq z-txt (geocad-block-attr-text obj "RZEDNA"))
  )
  (setq z-val (geocad-pikieta-data-safe-z z-txt fallback))
  z-val
)

(defun geocad-pikieta-data-from-block
  (obj / pt px py pz nr z)
  (setq pt (geocad-object-point-list obj))
  (if pt
    (progn
      (setq px (car pt))
      (setq py (cadr pt))
      (setq pz (caddr pt))
      (if (not pz)
        (setq pz 0.0)
      )
      (setq nr (geocad-block-attr-text obj "NR"))
      (if (= nr "")
        (setq nr "---")
      )
      (setq z (geocad-pikieta-data-block-z obj pz))
      (geocad-pikieta-data-make
        (list px py z)
        nr
        z
        "block"
        nil
        nil
        nil
        obj
      )
    )
    nil
  )
)

(defun geocad-pikieta-data-read-blocks
  (prefix / pref lay-pt ss i obj item result)
  (setq result '())
  (setq pref (geocad-normalize-layer-prefix prefix))
  (if (/= pref "")
    (progn
      (setq lay-pt (geocad-layer-name pref *geocad-layer-type-points*))
      (setq ss
        (ssget
          "_X"
          (list
            '(0 . "INSERT")
            '(2 . "Pikieta_Geo")
            (cons 8 lay-pt)
          )
        )
      )
      (if ss
        (progn
          (setq i 0)
          (while (< i (sslength ss))
            (setq obj (vlax-ename->vla-object (ssname ss i)))
            (setq item (geocad-pikieta-data-from-block obj))
            (if item
              (setq result (cons item result))
            )
            (setq i (1+ i))
          )
        )
      )
    )
  )
  (reverse result)
)

(defun geocad-pikieta-data-from-text-point
  (pt-obj nr-items h-items txt-h / pt px py pz dX dY tol nr-item h-item nr-obj h-obj nr z)
  ;; Zwraca liste:
  ;; (data updated-nr-items updated-h-items)
  (setq pt (geocad-object-point-list pt-obj))
  (if pt
    (progn
      (setq px (car pt))
      (setq py (cadr pt))
      (setq pz (caddr pt))
      (if (not pz)
        (setq pz 0.0)
      )

      (setq dX (* txt-h 1.2))
      (setq dY (* txt-h 0.7))
      (setq tol (max 1.0 (* txt-h 8.0)))

      (setq nr-item
        (geocad-text-radar-find-nearest
          (list (+ px dX) (+ py dY) pz)
          nr-items
          tol
          nil
        )
      )

      (setq h-item
        (geocad-text-radar-find-nearest
          (list (+ px dX) (- py dY) pz)
          h-items
          tol
          nil
        )
      )

      (setq nr-obj (geocad-text-radar-item-object nr-item))
      (setq h-obj  (geocad-text-radar-item-object h-item))

      (setq nr (geocad-text-radar-item-text nr-item))
      (if (= nr "")
        (setq nr "---")
      )

      (setq z
        (geocad-pikieta-data-safe-z
          (geocad-text-radar-item-text h-item)
          pz
        )
      )

      (list
        (geocad-pikieta-data-make
          (list px py z)
          nr
          z
          "text"
          pt-obj
          nr-obj
          h-obj
          nil
        )
        (geocad-text-radar-remove-item nr-item nr-items)
        (geocad-text-radar-remove-item h-item h-items)
      )
    )
    (list nil nr-items h-items)
  )
)

(defun geocad-pikieta-data-read-texts
  (prefix txt-h / pref lay-pt lay-nr lay-h ss i pt-obj bundle item result nr-items h-items)
  (setq result '())
  (setq pref (geocad-normalize-layer-prefix prefix))
  (if (/= pref "")
    (progn
      (setq lay-pt (geocad-layer-name pref *geocad-layer-type-points*))
      (setq lay-nr (geocad-layer-name pref *geocad-layer-type-label-nr*))
      (setq lay-h  (geocad-layer-name pref *geocad-layer-type-label-h*))

      ;; Teksty zbieramy raz do pamieci.
      (setq nr-items (geocad-text-radar-collect-layer lay-nr))
      (setq h-items  (geocad-text-radar-collect-layer lay-h))

      (setq ss
        (ssget
          "_X"
          (list
            '(0 . "POINT")
            (cons 8 lay-pt)
          )
        )
      )

      (if ss
        (progn
          (setq i 0)
          (while (< i (sslength ss))
            (setq pt-obj (vlax-ename->vla-object (ssname ss i)))
            (setq bundle
              (geocad-pikieta-data-from-text-point
                pt-obj
                nr-items
                h-items
                txt-h
              )
            )
            (setq item (car bundle))
            (setq nr-items (cadr bundle))
            (setq h-items  (caddr bundle))
            (if item
              (setq result (cons item result))
            )
            (setq i (1+ i))
          )
        )
      )
    )
  )
  (reverse result)
)

(defun geocad-pikieta-data-write-as-text
  (data txt-h z-prec display lay-pt lay-nr lay-h / item pt nr count)
  (setq count 0)
  (foreach item data
    (setq pt (geocad-pikieta-data-get item 'pt))
    (setq nr (geocad-pikieta-data-get item 'nr))
    (if pt
      (progn
        (geocad-create-text-pikieta
          pt
          nr
          txt-h
          z-prec
          display
          lay-pt
          lay-nr
          lay-h
        )
        (setq count (1+ count))
      )
    )
  )
  count
)

(defun geocad-pikieta-data-write-as-blocks
  (doc data txt-h z-prec display lay-pt lay-nr lay-h / item pt nr count)
  (setq count 0)
  (foreach item data
    (setq pt (geocad-pikieta-data-get item 'pt))
    (setq nr (geocad-pikieta-data-get item 'nr))
    (if pt
      (progn
        (geocad-insert-pikieta-block-from-data
          doc
          pt
          nr
          txt-h
          z-prec
          display
          lay-pt
          lay-nr
          lay-h
        )
        (setq count (1+ count))
      )
    )
  )
  count
)

(defun geocad-pikieta-data-delete-sources
  (data / item)
  (foreach item data
    (geocad-safe-delete-object (geocad-pikieta-data-get item 'block-obj))
    (geocad-safe-delete-object (geocad-pikieta-data-get item 'nr-obj))
    (geocad-safe-delete-object (geocad-pikieta-data-get item 'h-obj))
    (geocad-safe-delete-object (geocad-pikieta-data-get item 'point-obj))
  )
  T
)

(defun geocad-pikieta-data-update-text-objects
  (doc data txt-h z-prec display lay-pt lay-nr lay-h / item pt px py pz dX dY nr nr-obj h-obj nr-pt h-pt z-str show-nr show-h count)
  (setq count 0)
  (setq show-nr (if (member display '("Oba" "Numer")) T nil))
  (setq show-h  (if (member display '("Oba" "Rzedna")) T nil))

  (foreach item data
    (setq pt (geocad-pikieta-data-get item 'pt))
    (if pt
      (progn
        (setq px (car pt))
        (setq py (cadr pt))
        (setq pz (caddr pt))
        (if (not pz)
          (setq pz 0.0)
        )
        (setq dX (* txt-h 1.2))
        (setq dY (* txt-h 0.7))
        (setq nr-pt (list (+ px dX) (+ py dY) pz))
        (setq h-pt  (list (+ px dX) (- py dY) pz))
        (setq z-str (rtos pz 2 z-prec))
        (setq nr (geocad-pikieta-data-get item 'nr))
        (if (or (not nr) (= nr ""))
          (setq nr "---")
        )

        (vl-catch-all-apply
          'vla-put-Layer
          (list (geocad-pikieta-data-get item 'point-obj) lay-pt)
        )

        (setq nr-obj (geocad-pikieta-data-get item 'nr-obj))
        (setq h-obj  (geocad-pikieta-data-get item 'h-obj))

        (if nr-obj
          (geocad-update-text-object
            nr-obj
            nr-pt
            txt-h
            lay-nr
            nr
            show-nr
          )
          (if show-nr
            (geocad-make-text-entity
              nr-pt
              txt-h
              nr
              lay-nr
              T
            )
          )
        )

        (if h-obj
          (geocad-update-text-object
            h-obj
            h-pt
            txt-h
            lay-h
            z-str
            show-h
          )
          (geocad-make-text-entity
            h-pt
            txt-h
            z-str
            lay-h
            show-h
          )
        )

        (setq count (1+ count))
      )
    )
  )
  count
)

(defun geocad-pikieta-convert-blocks-to-text
  (doc prefix kolor txt-h z-prec display / pref lay-pt lay-nr lay-h data count)
  (setq pref (geocad-normalize-layer-prefix prefix))
  (setq count 0)
  (if (/= pref "")
    (progn
      (setq lay-pt (geocad-layer-name pref *geocad-layer-type-points*))
      (setq lay-nr (geocad-layer-name pref *geocad-layer-type-label-nr*))
      (setq lay-h  (geocad-layer-name pref *geocad-layer-type-label-h*))

      (geocad-ensure-layer doc lay-pt kolor)
      (geocad-ensure-layer doc lay-nr kolor)
      (geocad-ensure-layer doc lay-h kolor)

      (setq data (geocad-pikieta-data-read-blocks pref))
      (if data
        (progn
          (vla-StartUndoMark doc)
          (setq count
            (geocad-pikieta-data-write-as-text
              data
              txt-h
              z-prec
              display
              lay-pt
              lay-nr
              lay-h
            )
          )
          (geocad-pikieta-data-delete-sources data)
          (vla-EndUndoMark doc)
        )
      )
    )
  )
  count
)

(defun geocad-pikieta-convert-text-to-blocks
  (doc prefix kolor txt-h z-prec display / pref lay-pt lay-nr lay-h data count)
  (setq pref (geocad-normalize-layer-prefix prefix))
  (setq count 0)
  (if (/= pref "")
    (progn
      (setq lay-pt (geocad-layer-name pref *geocad-layer-type-points*))
      (setq lay-nr (geocad-layer-name pref *geocad-layer-type-label-nr*))
      (setq lay-h  (geocad-layer-name pref *geocad-layer-type-label-h*))

      (geocad-ensure-layer doc lay-pt kolor)
      (geocad-ensure-layer doc lay-nr kolor)
      (geocad-ensure-layer doc lay-h kolor)
      (geocad-stworz-blok-pikieta)

      (setq data (geocad-pikieta-data-read-texts pref txt-h))
      (if data
        (progn
          (vla-StartUndoMark doc)
          (setq count
            (geocad-pikieta-data-write-as-blocks
              doc
              data
              txt-h
              z-prec
              display
              lay-pt
              lay-nr
              lay-h
            )
          )
          (geocad-pikieta-data-delete-sources data)
          (vla-EndUndoMark doc)
        )
      )
    )
  )
  count
)

(defun geocad-pikieta-update-text-data
  (doc prefix kolor txt-h z-prec display / pref lay-pt lay-nr lay-h data count)
  (setq pref (geocad-normalize-layer-prefix prefix))
  (setq count 0)
  (if (/= pref "")
    (progn
      (setq lay-pt (geocad-layer-name pref *geocad-layer-type-points*))
      (setq lay-nr (geocad-layer-name pref *geocad-layer-type-label-nr*))
      (setq lay-h  (geocad-layer-name pref *geocad-layer-type-label-h*))

      (geocad-ensure-layer doc lay-pt kolor)
      (geocad-ensure-layer doc lay-nr kolor)
      (geocad-ensure-layer doc lay-h kolor)
      (geocad-update-managed-layer-colors doc pref kolor)

      (setq data (geocad-pikieta-data-read-texts pref txt-h))
      (if data
        (progn
          (vla-StartUndoMark doc)
          (setq count
            (geocad-pikieta-data-update-text-objects
              doc
              data
              txt-h
              z-prec
              display
              lay-pt
              lay-nr
              lay-h
            )
          )
          (vla-EndUndoMark doc)
        )
      )
    )
  )
  count
)

(princ)
