;; ======================================================
;; GEOPROFICAD - PIKIETA STYLE
;; ======================================================
;;
;; Konwersje Blok/Tekst i aktualizacja istniejacych pikiet.
;; Shadow split: definicje sa zgodne z gp_CoreLegacy.lsp
;; i sa ladowane po legacy, bez zmiany publicznego API.
;; ======================================================

(setq *geocad-module-pikietastyle-loaded* T)

(defun geocad-create-text-pikieta
  (
    pt-list nr-str txt-h z-prec display
    lay-pt lay-nr lay-h
    /
    px py pz dX dY z-str show-nr show-h
  )
  ;; Tworzy wariant tekstowy pikiety:
  ;; POINT + TEXT NR + TEXT H.
  ;;
  ;; Nowo tworzone pikiety tekstowe zawsze dostaja oba TEXT-y.
  ;; Widocznosc sterujemy przez Visible, zeby pozniej dalo sie je odzyskac
  ;; przy zmianie trybu widocznosci.

  (if
    (or
      (not nr-str)
      (= nr-str "")
    )
    (setq nr-str "---")
  )

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

  (geocad-make-point-entity pt-list lay-pt)

  (geocad-make-text-entity
    (list (+ px dX) (+ py dY) pz)
    txt-h
    nr-str
    lay-nr
    show-nr
  )

  (geocad-make-text-entity
    (list (+ px dX) (- py dY) pz)
    txt-h
    z-str
    lay-h
    show-h
  )

  T
)

(defun geocad-insert-pikieta-block-from-data
  (
    doc pt-list nr-str txt-h z-prec display
    lay-pt lay-nr lay-h
    /
    space px py pz dX dY z-str pt-3d blkRef vis-nr vis-h tag
  )
  ;; Tworzy blok Pikieta_Geo z danych odczytanych z wariantu tekstowego.

  (if
    (or
      (not nr-str)
      (= nr-str "")
    )
    (setq nr-str "---")
  )

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

  (setq vis-nr
    (if (member display '("Oba" "Numer"))
      :vlax-false
      :vlax-true
    )
  )

  (setq vis-h
    (if (member display '("Oba" "Rzedna"))
      :vlax-false
      :vlax-true
    )
  )

  (setq blkRef
    (vla-InsertBlock
      space
      pt-3d
      "Pikieta_Geo"
      1.0
      1.0
      1.0
      0.0
    )
  )

  (vla-put-Layer blkRef lay-pt)

  (foreach att (vlax-invoke blkRef 'GetAttributes)
    (vla-put-Height att txt-h)
    (setq tag (strcase (vla-get-TagString att)))

    (cond
      ((= tag "NR")
        (vla-put-TextString att nr-str)
        (vla-put-InsertionPoint
          att
          (vlax-3d-point (list (+ px dX) (+ py dY) pz))
        )
        (vla-put-Invisible att vis-nr)
        (vla-put-Layer att lay-nr)
      )

      ((member tag '("H" "Z" "RZEDNA"))
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

(defun geocad-convert-blocks-to-text
  (
    doc prefix kolor txt-h z-prec display
    /
    pref lay-pt lay-nr lay-h ss i obj pt nr count
  )
  ;; Konwersja:
  ;; INSERT Pikieta_Geo -> POINT + TEXT + TEXT.

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
            (setq pt (geocad-object-point-list obj))
            (setq nr (geocad-block-attr-text obj "NR"))

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

                (geocad-safe-delete-object obj)
                (setq count (1+ count))
              )
            )

            (setq i (1+ i))
          )
        )
      )
    )
  )

  count
)

(defun geocad-convert-text-to-blocks
  (
    doc prefix kolor txt-h z-prec display
    /
    pref lay-pt lay-nr lay-h ss i pt-obj pt
    px py pz dX dY tol nr-obj h-obj nr-str count
  )
  ;; Konwersja:
  ;; POINT + TEXT + TEXT -> INSERT Pikieta_Geo.
  ;;
  ;; Uwaga:
  ;; Jezeli stara tekstowa pikieta nie ma tekstu numeru,
  ;; numeru nie da sie odtworzyc. Wtedy blok dostanie NR = "---".

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
            (setq pt (geocad-object-point-list pt-obj))

            (if pt
              (progn
                (setq px (car pt))
                (setq py (cadr pt))
                (setq pz (caddr pt))

                (if (not pz)
                  (setq pz 0.0)
                )

                (setq pt (list px py pz))
                (setq dX (* txt-h 1.2))
                (setq dY (* txt-h 0.7))

                ;; Celowo dosc szeroka tolerancja, bo teksty mogly byc
                ;; utworzone przy innej wysokosci tekstu.
                (setq tol (max 1.0 (* txt-h 8.0)))

                (setq nr-obj
                  (geocad-find-nearest-text-object
                    lay-nr
                    (list (+ px dX) (+ py dY) pz)
                    tol
                  )
                )

                (setq h-obj
                  (geocad-find-nearest-text-object
                    lay-h
                    (list (+ px dX) (- py dY) pz)
                    tol
                  )
                )

                (setq nr-str (geocad-text-string-or-empty nr-obj))

                (if (= nr-str "")
                  (setq nr-str "---")
                )

                (geocad-insert-pikieta-block-from-data
                  doc
                  pt
                  nr-str
                  txt-h
                  z-prec
                  display
                  lay-pt
                  lay-nr
                  lay-h
                )

                (geocad-safe-delete-object nr-obj)
                (geocad-safe-delete-object h-obj)
                (geocad-safe-delete-object pt-obj)

                (setq count (1+ count))
              )
            )

            (setq i (1+ i))
          )
        )
      )
    )
  )

  count
)

(defun geocad-update-text-style-existing
  (
    doc prefix kolor txt-h z-prec display
    /
    pref lay-pt lay-nr lay-h ss i pt-obj pt
    px py pz dX dY tol nr-obj h-obj nr-pt h-pt
    show-nr show-h z-str count nr-str
  )
  ;; Aktualizuje istniejace pikiety tekstowe:
  ;; POINT + TEXT NR + TEXT H.
  ;;
  ;; Nie konwertuje stylu. Tylko poprawia:
  ;; - warstwy,
  ;; - kolor warstw,
  ;; - wysokosc tekstu,
  ;; - pozycje tekstow,
  ;; - widocznosc,
  ;; - tekst rzednej.

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
            (setq pt (geocad-object-point-list pt-obj))

            (if pt
              (progn
                (setq px (car pt))
                (setq py (cadr pt))
                (setq pz (caddr pt))

                (if (not pz)
                  (setq pz 0.0)
                )

                (setq pt (list px py pz))
                (setq dX (* txt-h 1.2))
                (setq dY (* txt-h 0.7))
                (setq tol (max 1.0 (* txt-h 8.0)))

                (setq nr-pt (list (+ px dX) (+ py dY) pz))
                (setq h-pt  (list (+ px dX) (- py dY) pz))
                (setq z-str (rtos pz 2 z-prec))

                (vl-catch-all-apply
                  'vla-put-Layer
                  (list pt-obj lay-pt)
                )

                (setq nr-obj
                  (geocad-find-nearest-text-object
                    lay-nr
                    nr-pt
                    tol
                  )
                )

                (setq h-obj
                  (geocad-find-nearest-text-object
                    lay-h
                    h-pt
                    tol
                  )
                )

                (if nr-obj
                  (progn
                    (setq nr-str (geocad-text-string-or-empty nr-obj))

                    (if (= nr-str "")
                      (setq nr-str "---")
                    )

                    (geocad-update-text-object
                      nr-obj
                      nr-pt
                      txt-h
                      lay-nr
                      nr-str
                      show-nr
                    )
                  )
                  (if show-nr
                    (geocad-make-text-entity
                      nr-pt
                      txt-h
                      "---"
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

            (setq i (1+ i))
          )
        )
      )
    )
  )

  count
)

(defun geocad-setup-apply-current-group-params
  (
    doc prefix kolor-str txt-h-str z-prec-str styl display
    /
    pref kolor txt-h z-prec
  )
  ;; Glowny auto-apply dla GEO_SETUP.
  ;;
  ;; Po kazdej realnej zmianie parametru:
  ;; - zapis parametru robi funkcja autosave,
  ;; - ta funkcja stosuje aktualny profil do obiektow aktywnej grupy.
  ;;
  ;; Styl = Tekst:
  ;; - bloki sa konwertowane do POINT + TEXT,
  ;; - istniejace tekstowe pikiety sa aktualizowane.
  ;;
  ;; Styl = Blok:
  ;; - tekstowe pikiety sa konwertowane do blokow,
  ;; - bloki sa aktualizowane przez istniejace geocad-update-existing.

  (setq pref (geocad-normalize-layer-prefix prefix))

  (if (= pref "")
    (setq pref "POMIAR")
  )

  (setq kolor (atoi kolor-str))
  (setq txt-h (atof txt-h-str))
  (setq z-prec (atoi z-prec-str))

  (if (= styl "Tekst")
    (progn
      (geocad-convert-blocks-to-text
        doc
        pref
        kolor
        txt-h
        z-prec
        display
      )

      (geocad-update-text-style-existing
        doc
        pref
        kolor
        txt-h
        z-prec
        display
      )
    )
    (progn
      (geocad-convert-text-to-blocks
        doc
        pref
        kolor
        txt-h
        z-prec
        display
      )

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

  T
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

  (setq kolor (atoi kolor-str)
        txt-h (atof txt-h-str)
        z-prec (atoi z-prec-str)
  )

  (setq dX (* txt-h 1.2)
        dY (* txt-h 0.7)
  )

  (setq vis-nr
    (if (member display '("Oba" "Numer"))
      :vlax-false
      :vlax-true
    )
  )

  (setq vis-h
    (if (member display '("Oba" "Rzedna"))
      :vlax-false
      :vlax-true
    )
  )

  ;; ------------------------------------------------------
  ;; Aktualizacja kolorow warstw grupy.
  ;; ------------------------------------------------------
  (if (= target_prefix *geocad-all-groups-label*)
    (foreach pref (geocad-get-existing-prefixes)
      (geocad-update-managed-layer-colors doc pref kolor)
    )
    (geocad-update-managed-layer-colors doc target_prefix kolor)
  )

  ;; ------------------------------------------------------
  ;; Wybor blokow do aktualizacji.
  ;; ------------------------------------------------------
  (if (= target_prefix *geocad-all-groups-label*)
    (setq ss
      (ssget "X" '((0 . "INSERT") (2 . "Pikieta_Geo")))
    )
    (setq ss
      (ssget
        "X"
        (list
          '(0 . "INSERT")
          '(2 . "Pikieta_Geo")
          (cons
            8
            (geocad-layer-name target_prefix *geocad-layer-type-points*)
          )
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

        (setq px (car pt)
              py (cadr pt)
              pz (caddr pt)
        )

        ;; ------------------------------------------------------
        ;; Ustalenie grupy z aktualnej warstwy bloku.
        ;; ------------------------------------------------------
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
                ;; Legacy fallback:
                ;; jezeli stary blok nie siedzi na standardowej warstwie,
                ;; zostaje na swojej warstwie.
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

        ;; Warstwy tworzymy bezpiecznie tylko tutaj, bo aktualizujemy
        ;; realne bloki i musimy miec gdzie przeniesc atrybuty.
        (geocad-ensure-layer doc lay-pt kolor)
        (geocad-ensure-layer doc lay-nr kolor)
        (geocad-ensure-layer doc lay-h kolor)

        ;; Blok zostaje na warstwie punktow swojej grupy.
        (vla-put-Layer obj lay-pt)

        (foreach att (vlax-invoke obj 'GetAttributes)
          (vla-put-Height att txt-h)

          (cond
            ((= (vla-get-TagString att) "NR")
              (vla-put-InsertionPoint
                att
                (vlax-3d-point (list (+ px dX) (+ py dY) pz))
              )
              (vla-put-Invisible att vis-nr)
              (vla-put-Layer att lay-nr)
            )

            ((member (vla-get-TagString att) '("H" "Z" "RZEDNA"))
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
    (princ
      "\n[INFO] Nie znaleziono blokow do aktualizacji. Zaktualizowano kolor istniejacych warstw grupy, jezeli istnialy."
    )
  )
)

(princ)
