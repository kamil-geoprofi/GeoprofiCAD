;; ======================================================
;; GEOPROFICAD - PIKIETA STYLE OPTIMIZED
;; ======================================================
;;
;; Optymalizacje konwersji tekstowych pikiet.
;; Ten modul laduje sie po gp_PikietaStyle.lsp i nadpisuje tylko
;; funkcje, ktore w starej wersji robily kosztowne ssget w petli
;; albo wykonywaly zbedny drugi przebieg po konwersji.
;; ======================================================

(setq *geocad-module-pikietastyleoptimized-loaded* T)

(defun geocad-convert-text-to-blocks
  (
    doc prefix kolor txt-h z-prec display
    /
    pref lay-pt lay-nr lay-h ss i pt-obj pt
    px py pz dX dY tol nr-item h-item nr-obj h-obj nr-str count
    nr-items h-items
  )
  ;; Konwersja:
  ;; POINT + TEXT + TEXT -> INSERT Pikieta_Geo.
  ;;
  ;; Optymalizacja:
  ;; - POINT-y sa zbierane jednym ssget,
  ;; - teksty NR/H sa zbierane jednym ssget na warstwe,
  ;; - parowanie punkt -> tekst dziala na listach w pamieci.
  ;; Nie ma juz ssget dla kazdego punktu.

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

                ;; Szeroka tolerancja zachowana z poprzedniej implementacji.
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

                ;; Ten sam tekst nie powinien zostac przypisany do kolejnego punktu.
                (setq nr-items (geocad-text-radar-remove-item nr-item nr-items))
                (setq h-items  (geocad-text-radar-remove-item h-item h-items))

                (setq nr-str (geocad-text-radar-item-text nr-item))

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
    px py pz dX dY tol nr-item h-item nr-obj h-obj nr-pt h-pt
    show-nr show-h z-str count nr-str
    nr-items h-items
  )
  ;; Aktualizuje istniejace pikiety tekstowe:
  ;; POINT + TEXT NR + TEXT H.
  ;;
  ;; Optymalizacja:
  ;; - teksty NR/H sa zbierane raz,
  ;; - parowanie uzywa wspolnego geocad-text-radar-* z core,
  ;; - nie ma ssget w petli po punktach.

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

                (setq nr-item
                  (geocad-text-radar-find-nearest
                    nr-pt
                    nr-items
                    tol
                    nil
                  )
                )

                (setq h-item
                  (geocad-text-radar-find-nearest
                    h-pt
                    h-items
                    tol
                    nil
                  )
                )

                (setq nr-obj (geocad-text-radar-item-object nr-item))
                (setq h-obj  (geocad-text-radar-item-object h-item))

                ;; Zapobiega przypisaniu tego samego tekstu do kolejnej pikiety.
                (setq nr-items (geocad-text-radar-remove-item nr-item nr-items))
                (setq h-items  (geocad-text-radar-remove-item h-item h-items))

                (if nr-obj
                  (progn
                    (setq nr-str (geocad-text-radar-item-text nr-item))
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
    pref kolor txt-h z-prec converted-count
  )
  ;; Glowny auto-apply dla GEO_SETUP.
  ;;
  ;; Optymalizacja krytyczna:
  ;; Przy zmianie stylu Blok -> Tekst nie wolno po konwersji odpalac
  ;; pelnego update tekstowych pikiet po tej samej grupie.
  ;; Konwersja sama tworzy teksty z aktualnymi parametrami, wiec drugi
  ;; przebieg jest zbedny i kosztowny przy tysiacach pikiet.

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

      ;; Jezeli byly bloki, konwersja juz utworzyla poprawne POINT/TEXT.
      ;; Update odpalamy tylko wtedy, gdy blokow nie bylo, czyli grupa
      ;; byla juz tekstowa i faktycznie trzeba przestawic istniejace teksty.
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

(princ)
