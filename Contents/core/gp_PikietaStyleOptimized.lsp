;; ======================================================
;; GEOPROFICAD - PIKIETA STYLE OPTIMIZED
;; ======================================================
;;
;; Aktywne runtime definicje operacji stylu pikiet.
;;
;; Zasada porzadkowa:
;; - jedna wspolna szybka funkcja tworzy wariant tekstowy pikiety,
;; - import/wstawianie i konwersja Blok -> Tekst uzywaja tej samej funkcji,
;; - konwersje ida przez PikietaData: DWG -> PikietaData[] -> zapis docelowy.
;; ======================================================

(setq *geocad-module-pikietastyleoptimized-loaded* T)

(defun geocad-create-text-pikieta
  (
    pt-list nr-str txt-h z-prec display
    lay-pt lay-nr lay-h
    /
    px py pz dX dY z-str show-nr show-h
  )
  ;; Wspolny szybki writer tekstowej pikiety.
  ;; Uzywany przez import/wstawianie i przez konwersje Blok -> Tekst.
  ;; Celowo jest zgodny z szybka sciezka importu: czyste entmakex,
  ;; bez COM Visible dla nowo tworzonych tekstow.

  (setq nr-str (geocad-pikieta-empty-nr-if-needed nr-str))

  (setq px (car pt-list))
  (setq py (cadr pt-list))
  (setq pz (caddr pt-list))

  (if (not pz)
    (setq pz 0.0)
  )

  (setq pt-list (list px py pz))
  (setq dX (* txt-h 1.2))
  (setq dY (* txt-h 0.7))
  (setq z-str (rtos pz 2 z-prec))

  (setq show-nr
    (if (member display '("Oba" "Numer"))
      T
      nil
    )
  )

  (setq show-h
    (if (member display '("Oba" "Rzedna"))
      T
      nil
    )
  )

  (entmakex
    (list
      '(0 . "POINT")
      (cons 10 pt-list)
      (cons 8 lay-pt)
    )
  )

  (if show-nr
    (entmakex
      (list
        '(0 . "TEXT")
        (cons 10 (list (+ px dX) (+ py dY) pz))
        (cons 40 txt-h)
        (cons 1 nr-str)
        (cons 8 lay-nr)
      )
    )
  )

  (if show-h
    (entmakex
      (list
        '(0 . "TEXT")
        (cons 10 (list (+ px dX) (- py dY) pz))
        (cons 40 txt-h)
        (cons 1 z-str)
        (cons 8 lay-h)
      )
    )
  )

  T
)

(defun geocad-stworz-blok-pikieta ()
  (if (not (tblsearch "BLOCK" *geocad-pikieta-block-name*))
    (progn
      (entmake
        (list
          '(0 . "BLOCK")
          (cons 2 *geocad-pikieta-block-name*)
          '(70 . 2)
          '(10 0.0 0.0 0.0)
          (cons 8 "0")
        )
      )

      (entmake
        (list
          '(0 . "POINT")
          '(10 0.0 0.0 0.0)
          (cons 8 "0")
        )
      )

      (entmake
        (list
          '(0 . "ATTDEF")
          '(10 1.0 0.5 0.0)
          (cons 1 *geocad-pikieta-empty-nr*)
          (cons 2 *geocad-pikieta-attr-nr*)
          '(3 . "Nr")
          (cons 40 1.0)
          '(70 . 0)
          (cons 8 "0")
        )
      )

      (entmake
        (list
          '(0 . "ATTDEF")
          '(10 1.0 -0.5 0.0)
          (cons 1 *geocad-pikieta-default-z-text*)
          (cons 2 *geocad-pikieta-attr-z-main*)
          (cons 3 *geocad-pikieta-attr-z-main*)
          (cons 40 1.0)
          '(70 . 0)
          (cons 8 "0")
        )
      )

      (entmake
        (list
          '(0 . "ENDBLK")
          (cons 8 "0")
        )
      )
    )
  )

  (princ)
)

(defun geocad-wstaw-pikiete-with-context
  (
    doc space pt-list nr-str show-z ctx
    /
    txt-h z-prec styl pikt-pref
    lay-pt lay-nr lay-h
    vis-nr vis-h dX dY
    pelny-nr px py pz z-str pt-3d blkRef
  )
  ;; Import i zwykle wstawianie pikiety ida przez ten punkt.
  ;; Dla stylu tekstowego delegujemy do wspolnego writer'a,
  ;; zamiast trzymac druga kopie logiki entmakex.

  (setq txt-h (geocad-ctx-get 'txt-h ctx))
  (setq z-prec (geocad-ctx-get 'z-prec ctx))
  (setq styl (geocad-ctx-get 'styl ctx))
  (setq pikt-pref (geocad-ctx-get 'pikt-pref ctx))

  (setq lay-pt (geocad-ctx-get 'lay-pt ctx))
  (setq lay-nr (geocad-ctx-get 'lay-nr ctx))
  (setq lay-h (geocad-ctx-get 'lay-h ctx))

  (if (not nr-str)
    (setq nr-str "")
  )

  (setq nr-str (vl-princ-to-string nr-str))
  (setq pelny-nr (geocad-pikieta-empty-nr-if-needed (strcat pikt-pref nr-str)))

  (if (= styl "Tekst")
    (geocad-create-text-pikieta
      pt-list
      pelny-nr
      txt-h
      z-prec
      (geocad-ctx-get 'display ctx)
      lay-pt
      lay-nr
      lay-h
    )
    (progn
      (setq vis-nr (geocad-ctx-get 'vis-nr ctx))
      (setq vis-h (geocad-ctx-get 'vis-h ctx))
      (setq dX (geocad-ctx-get 'dX ctx))
      (setq dY (geocad-ctx-get 'dY ctx))

      (setq px (car pt-list))
      (setq py (cadr pt-list))
      (setq pz (caddr pt-list))

      (if (not pz)
        (setq pz 0.0)
      )

      (setq pt-list (list px py pz))
      (setq pt-3d (vlax-3d-point pt-list))
      (setq z-str (rtos pz 2 z-prec))

      (setq blkRef
        (vla-InsertBlock
          space
          pt-3d
          *geocad-pikieta-block-name*
          1.0
          1.0
          1.0
          0.0
        )
      )

      (vla-put-Layer blkRef lay-pt)

      (foreach att (vlax-invoke blkRef 'GetAttributes)
        (vla-put-Height att txt-h)
        (cond
          ((geocad-pikieta-nr-tag-p (vla-get-TagString att))
            (vla-put-TextString att pelny-nr)
            (vla-put-InsertionPoint
              att
              (vlax-3d-point (list (+ px dX) (+ py dY) pz))
            )
            (vla-put-Invisible att vis-nr)
            (vla-put-Layer att lay-nr)
          )

          ((geocad-pikieta-z-tag-p (vla-get-TagString att))
            (vla-put-TextString att z-str)
            (vla-put-InsertionPoint
              att
              (vlax-3d-point (list (+ px dX) (- py dY) pz))
            )
            (vla-put-Invisible att vis-h)
            (vla-put-Layer att lay-h)
          )
        )
      )
    )
  )

  (princ)
)

(defun geocad-insert-pikieta-block-from-data
  (
    doc pt-list nr-str txt-h z-prec display
    lay-pt lay-nr lay-h
    /
    space px py pz dX dY z-str pt-3d blkRef vis-nr vis-h
  )
  ;; Tworzy blok pikiety zgodny z centralnym schematem.
  (setq nr-str (geocad-pikieta-empty-nr-if-needed nr-str))
  (geocad-stworz-blok-pikieta)
  (setq space (vla-get-ModelSpace doc))

  (setq px (car pt-list))
  (setq py (cadr pt-list))
  (setq pz (caddr pt-list))
  (if (not pz)
    (setq pz 0.0)
  )

  (setq pt-list (list px py pz))
  (setq pt-3d (vlax-3d-point pt-list))
  (setq dX (* txt-h 1.2))
  (setq dY (* txt-h 0.7))
  (setq z-str (rtos pz 2 z-prec))

  (setq vis-nr (if (member display '("Oba" "Numer")) :vlax-false :vlax-true))
  (setq vis-h  (if (member display '("Oba" "Rzedna")) :vlax-false :vlax-true))

  (setq blkRef
    (vla-InsertBlock
      space
      pt-3d
      *geocad-pikieta-block-name*
      1.0
      1.0
      1.0
      0.0
    )
  )

  (vla-put-Layer blkRef lay-pt)

  (foreach att (vlax-invoke blkRef 'GetAttributes)
    (vla-put-Height att txt-h)
    (cond
      ((geocad-pikieta-nr-tag-p (vla-get-TagString att))
        (vla-put-TextString att nr-str)
        (vla-put-InsertionPoint
          att
          (vlax-3d-point (list (+ px dX) (+ py dY) pz))
        )
        (vla-put-Invisible att vis-nr)
        (vla-put-Layer att lay-nr)
      )

      ((geocad-pikieta-z-tag-p (vla-get-TagString att))
        (vla-put-TextString att z-str)
        (vla-put-InsertionPoint
          att
          (vlax-3d-point (list (+ px dX) (- py dY) pz))
        )
        (vla-put-Invisible att vis-h)
        (vla-put-Layer att lay-h)
      )
    )
  )

  blkRef
)

(defun geocad-pikieta-data-block-z
  (obj fallback / z-txt)
  (setq z-txt (geocad-pikieta-attr-z-text obj))
  (geocad-pikieta-data-safe-z z-txt fallback)
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
      (setq nr (geocad-pikieta-attr-nr-text obj))
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
          (geocad-pikieta-block-layer-filter lay-pt)
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
