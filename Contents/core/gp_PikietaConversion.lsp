;; ======================================================
;; GEOPROFICAD - PIKIETA CONVERSION
;; ======================================================
;;
;; Zmiana stylu pikiet i aktualizacja istniejacych obiektow.
;; Konwersje ida przez PikietaData: DWG -> PikietaData[] -> writer.
;; ======================================================

(setq *geocad-module-pikietaconversion-loaded* T)

(defun geocad-pikieta-write-data-as-text
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

(defun geocad-pikieta-write-data-as-blocks
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

(defun geocad-pikieta-update-text-objects
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
            (geocad-pikieta-write-data-as-text
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
            (geocad-pikieta-write-data-as-blocks
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
            (geocad-pikieta-update-text-objects
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

(defun geocad-update-existing
  (
    doc target_prefix kolor-str txt-h-str z-prec-str display
    /
    ss i ent obj pt px py pz
    lay-pt lay-nr lay-h
    kolor txt-h z-prec dX dY vis-nr vis-h
    pref
  )
  (setq kolor (atoi kolor-str))
  (setq txt-h (atof txt-h-str))
  (setq z-prec (atoi z-prec-str))
  (setq dX (* txt-h 1.2))
  (setq dY (* txt-h 0.7))
  (setq vis-nr (if (member display '("Oba" "Numer")) :vlax-false :vlax-true))
  (setq vis-h  (if (member display '("Oba" "Rzedna")) :vlax-false :vlax-true))

  (if (= target_prefix *geocad-all-groups-label*)
    (foreach pref (geocad-get-existing-prefixes)
      (geocad-update-managed-layer-colors doc pref kolor)
    )
    (geocad-update-managed-layer-colors doc target_prefix kolor)
  )

  (if (= target_prefix *geocad-all-groups-label*)
    (setq ss (ssget "X" (geocad-pikieta-block-filter)))
    (setq ss
      (ssget
        "X"
        (geocad-pikieta-block-layer-filter
          (geocad-layer-name target_prefix *geocad-layer-type-points*)
        )
      )
    )
  )

  (if ss
    (progn
      (vla-StartUndoMark doc)
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        (setq pt
          (vlax-safearray->list
            (vlax-variant-value (vla-get-InsertionPoint obj))
          )
        )
        (setq px (car pt))
        (setq py (cadr pt))
        (setq pz (caddr pt))

        (setq lay-pt (vla-get-Layer obj))
        (setq pref (geocad-managed-layer-prefix-from-name lay-pt))

        (if pref
          (progn
            (setq lay-pt (geocad-layer-name pref *geocad-layer-type-points*))
            (setq lay-nr (geocad-layer-name pref *geocad-layer-type-label-nr*))
            (setq lay-h  (geocad-layer-name pref *geocad-layer-type-label-h*))
          )
          (progn
            (if (= target_prefix *geocad-all-groups-label*)
              (progn
                (setq lay-pt (vla-get-Layer obj))
                (setq lay-nr lay-pt)
                (setq lay-h lay-pt)
              )
              (progn
                (setq pref (geocad-normalize-layer-prefix target_prefix))
                (setq lay-pt (geocad-layer-name pref *geocad-layer-type-points*))
                (setq lay-nr (geocad-layer-name pref *geocad-layer-type-label-nr*))
                (setq lay-h  (geocad-layer-name pref *geocad-layer-type-label-h*))
              )
            )
          )
        )

        (geocad-ensure-layer doc lay-pt kolor)
        (geocad-ensure-layer doc lay-nr kolor)
        (geocad-ensure-layer doc lay-h kolor)
        (vla-put-Layer obj lay-pt)

        (foreach att (vlax-invoke obj 'GetAttributes)
          (vla-put-Height att txt-h)
          (cond
            ((geocad-pikieta-nr-tag-p (vla-get-TagString att))
              (vla-put-InsertionPoint
                att
                (vlax-3d-point (list (+ px dX) (+ py dY) pz))
              )
              (vla-put-Invisible att vis-nr)
              (vla-put-Layer att lay-nr)
            )

            ((geocad-pikieta-z-tag-p (vla-get-TagString att))
              (vla-put-TextString att (rtos pz 2 z-prec))
              (vla-put-InsertionPoint
                att
                (vlax-3d-point (list (+ px dX) (- py dY) pz))
              )
              (vla-put-Invisible att vis-h)
              (vla-put-Layer att lay-h)
            )
          )
        )
        (setq i (1+ i))
      )
      (vla-EndUndoMark doc)
      (princ
        (strcat
          "\n[SUKCES] Zaktualizowano "
          (itoa i)
          " blokow pikiet dla grupy: "
          target_prefix
        )
      )
    )
    (princ "\n[INFO] Nie znaleziono blokow do aktualizacji. Zaktualizowano kolor istniejacych warstw grupy, jezeli istnialy.")
  )
)

(defun geocad-convert-blocks-to-text
  (doc prefix kolor txt-h z-prec display)
  ;; INSERT Pikieta_Geo -> POINT + TEXT + TEXT.
  ;; Blok jest pelnym zrodlem danych, wiec radar tekstow nie jest tu uzywany.
  (geocad-pikieta-convert-blocks-to-text
    doc
    prefix
    kolor
    txt-h
    z-prec
    display
  )
)

(defun geocad-convert-text-to-blocks
  (doc prefix kolor txt-h z-prec display)
  ;; POINT + TEXT + TEXT -> INSERT Pikieta_Geo.
  ;; Radar tekstow jest uzywany tylko przy odczycie wariantu tekstowego.
  (geocad-pikieta-convert-text-to-blocks
    doc
    prefix
    kolor
    txt-h
    z-prec
    display
  )
)

(defun geocad-update-text-style-existing
  (doc prefix kolor txt-h z-prec display)
  ;; Aktualizacja istniejacego wariantu tekstowego bez konwersji.
  (geocad-pikieta-update-text-data
    doc
    prefix
    kolor
    txt-h
    z-prec
    display
  )
)

(defun geocad-setup-apply-current-group-params
  (
    doc prefix kolor-str txt-h-str z-prec-str styl display
    /
    pref kolor txt-h z-prec converted-count
  )
  ;; Auto-apply dla GEO_SETUP.
  ;; Po udanej konwersji nie robimy drugiego pelnego przebiegu tej samej grupy.

  (setq pref (geocad-normalize-layer-prefix prefix))
  (if (= pref "")
    (setq pref "POMIAR")
  )

  (setq kolor (atoi kolor-str))
  (setq txt-h (atof txt-h-str))
  (setq z-prec (atoi z-prec-str))

  (if (= styl "Tekst")
    (progn
      (setq converted-count
        (geocad-convert-blocks-to-text
          doc
          pref
          kolor
          txt-h
          z-prec
          display
        )
      )
      (if (= converted-count 0)
        (geocad-update-text-style-existing
          doc
          pref
          kolor
          txt-h
          z-prec
          display
        )
      )
    )
    (progn
      (setq converted-count
        (geocad-convert-text-to-blocks
          doc
          pref
          kolor
          txt-h
          z-prec
          display
        )
      )
      (if (= converted-count 0)
        (geocad-update-existing
          doc
          pref
          kolor-str
          txt-h-str
          z-prec-str
          display
        )
      )
    )
  )

  T
)

(princ)
