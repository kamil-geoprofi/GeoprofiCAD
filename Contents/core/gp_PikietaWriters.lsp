;; ======================================================
;; GEOPROFICAD - PIKIETA WRITERS
;; ======================================================
;;
;; Jedyne miejsce tworzenia pikiety jako tekst albo blok.
;; Factory, import i konwersje maja delegowac tutaj zamiast
;; powielac entmakex/vla-InsertBlock.
;; ======================================================

(setq *geocad-module-pikietawriters-loaded* T)

(defun geocad-create-text-pikieta
  (
    pt-list nr-str txt-h z-prec display
    lay-pt lay-nr lay-h
    /
    px py pz dX dY z-str show-nr show-h
  )
  ;; Wspolny szybki writer tekstowej pikiety.
  ;; Uzywany przez import/wstawianie i przez konwersje Blok -> Tekst.
  ;; Celowo jest zgodny z szybka sciezka importu: czyste entmakex.
  ;; Teksty NR/H zawsze istnieja, a widocznosc steruje DXF 60.

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

  (entmakex
    (list
      '(0 . "TEXT")
      (cons 10 (list (+ px dX) (+ py dY) pz))
      (cons 40 txt-h)
      (cons 1 nr-str)
      (cons 8 lay-nr)
      (cons 60 (if show-nr 0 1))
    )
  )

  (entmakex
    (list
      '(0 . "TEXT")
      (cons 10 (list (+ px dX) (- py dY) pz))
      (cons 40 txt-h)
      (cons 1 z-str)
      (cons 8 lay-h)
      (cons 60 (if show-h 0 1))
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

(defun geocad-insert-pikieta-block-from-data
  (
    doc pt-list nr-str txt-h z-prec display
    lay-pt lay-nr lay-h space
    /
    px py pz dX dY z-str pt-3d blkRef vis-nr vis-h
  )
  ;; Tworzy blok pikiety zgodny z centralnym schematem.
  (setq nr-str (geocad-pikieta-empty-nr-if-needed nr-str))
  (geocad-stworz-blok-pikieta)
  (if (not space)
    (setq space (vla-get-ModelSpace doc))
  )

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

(princ)
