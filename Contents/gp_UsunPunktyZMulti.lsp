(vl-load-com)
(load "gp_Core.lsp" "\nBLAD: Nie znaleziono pliku gp_Core.lsp!")

;; ======================================================
;; GEOPROFICAD - USUWANIE PIKIET Z MULTI
;; ======================================================
;;
;; Komendy:
;;   USUNPKT_MULTI
;;   GP_USUN_PKT_Z_MULTI
;;
;; Zasada:
;;   - szuka pikiet/blokow Pikieta_Geo/POINT na wierzcholkach polilinii,
;;   - usuwa punkty posrednie oraz punkty bazowe/wezlowa,
;;   - nie chroni juz poczatku, konca ani miejsc zmiany spadku/dlugosci segmentow.
;;
;; Uwaga:
;;   Kasowanie jest ograniczone do wierzcholkow wybranej polilinii MULTI.
;;   Przy domyslnym filtrze warstwy kasuje tylko obiekty z warstwy <prefix>_PIKIETY.
;; ======================================================


;; Tolerancja dopasowania pikiety do wierzcholka polilinii.
;; 0.01 = 1 cm, jezeli DWG jest w metrach.
(if (not (boundp '*gp-usunpktmulti-tolerance*))
  (setq *gp-usunpktmulti-tolerance* 0.01)
)

;; NIL = dopasowanie tylko po XY.
;; T   = dopasowanie po XY oraz Z.
(if (not (boundp '*gp-usunpktmulti-match-z*))
  (setq *gp-usunpktmulti-match-z* nil)
)


(defun gp-usunpktmulti-z (pt)
  (if (and pt (caddr pt))
    (caddr pt)
    0.0
  )
)


(defun gp-usunpktmulti-xy-distance (a b)
  (distance
    (list (car a) (cadr a))
    (list (car b) (cadr b))
  )
)


(defun gp-usunpktmulti-pt-match-p (a b tol)
  (and
    a
    b
    (<= (gp-usunpktmulti-xy-distance a b) tol)
    (or
      (not *gp-usunpktmulti-match-z*)
      (<=
        (abs (- (gp-usunpktmulti-z a) (gp-usunpktmulti-z b)))
        tol
      )
    )
  )
)


(defun gp-usunpktmulti-any-match-p (pt vertices tol / found)
  (setq found nil)

  (while (and vertices (not found))
    (if (gp-usunpktmulti-pt-match-p pt (car vertices) tol)
      (setq found T)
    )

    (setq vertices (cdr vertices))
  )

  found
)


(defun gp-usunpktmulti-ends-with-p (txt suffix / start)
  (and
    txt
    suffix
    (>= (strlen txt) (strlen suffix))
    (progn
      (setq start (+ 1 (- (strlen txt) (strlen suffix))))
      (=
        (strcase (substr txt start))
        (strcase suffix)
      )
    )
  )
)


(defun gp-usunpktmulti-strip-suffix (txt suffix)
  (if (gp-usunpktmulti-ends-with-p txt suffix)
    (substr txt 1 (- (strlen txt) (strlen suffix)))
    nil
  )
)


(defun gp-usunpktmulti-polyline-prefix (pline / layer suffix)
  (setq layer (cdr (assoc 8 (entget pline))))
  (setq suffix
    (if (boundp '*geocad-layer-suffix-polyline-multi*)
      *geocad-layer-suffix-polyline-multi*
      "_POLYLINES_FROM_MULTI"
    )
  )

  (if layer
    (gp-usunpktmulti-strip-suffix layer suffix)
    nil
  )
)


(defun gp-usunpktmulti-poly-vertices (pline / start-param end-param i pt vertices)
  (setq vertices nil)

  (setq start-param
    (vl-catch-all-apply
      'vlax-curve-getStartParam
      (list pline)
    )
  )

  (setq end-param
    (vl-catch-all-apply
      'vlax-curve-getEndParam
      (list pline)
    )
  )

  (if
    (and
      (not (vl-catch-all-error-p start-param))
      (not (vl-catch-all-error-p end-param))
    )
    (progn
      (setq i (fix start-param))
      (setq end-param (fix end-param))

      (while (<= i end-param)
        (setq pt
          (vl-catch-all-apply
            'vlax-curve-getPointAtParam
            (list pline i)
          )
        )

        (if (not (vl-catch-all-error-p pt))
          (setq vertices (cons pt vertices))
        )

        (setq i (1+ i))
      )
    )
  )

  (reverse vertices)
)


(defun gp-usunpktmulti-effective-block-name (ent / obj res data)
  (setq data (entget ent))

  (setq obj
    (vl-catch-all-apply
      'vlax-ename->vla-object
      (list ent)
    )
  )

  (if (not (vl-catch-all-error-p obj))
    (progn
      (setq res
        (vl-catch-all-apply
          'vla-get-EffectiveName
          (list obj)
        )
      )

      (if (not (vl-catch-all-error-p res))
        res
        (cdr (assoc 2 data))
      )
    )
    (cdr (assoc 2 data))
  )
)


(defun gp-usunpktmulti-entity-point (ent / data typ)
  (setq data (entget ent))
  (setq typ (cdr (assoc 0 data)))

  (cond
    ((= typ "POINT")
      (cdr (assoc 10 data))
    )

    ((= typ "INSERT")
      (cdr (assoc 10 data))
    )

    (T nil)
  )
)


(defun gp-usunpktmulti-candidate-p (ent target-layer / data typ lay block-name)
  (setq data (entget ent))
  (setq typ (cdr (assoc 0 data)))
  (setq lay (cdr (assoc 8 data)))

  (and
    (or
      (not target-layer)
      (and lay (= (strcase lay) (strcase target-layer)))
    )

    (or
      (= typ "POINT")

      (and
        (= typ "INSERT")
        (progn
          (setq block-name (gp-usunpktmulti-effective-block-name ent))
          (and
            block-name
            (= (strcase block-name) (strcase *geocad-pikieta-block-name*))
          )
        )
      )
    )
  )
)


(defun gp-usunpktmulti-build-filter (target-layer / filter)
  (setq filter (list '(0 . "INSERT,POINT")))

  (if target-layer
    (setq filter
      (append
        filter
        (list (cons 8 target-layer))
      )
    )
  )

  filter
)


(defun gp-usunpktmulti-delete-for-polyline
  (
    pline
    use-prefix-filter
    /
    poly-data poly-layer prefix target-layer
    vertices filter ss i ent pt
    deleted candidates matched delete-result
  )

  (setq deleted 0)
  (setq candidates 0)
  (setq matched 0)

  (setq poly-data (entget pline))
  (setq poly-layer (cdr (assoc 8 poly-data)))

  (setq prefix (gp-usunpktmulti-polyline-prefix pline))

  (if (and use-prefix-filter prefix)
    (setq target-layer (geocad-layer-name prefix *geocad-layer-type-points*))
    (setq target-layer nil)
  )

  (prompt
    (strcat
      "\nWarstwa polilinii: "
      (if poly-layer poly-layer "<brak>")
    )
  )

  (prompt
    (strcat
      "\nWarstwa pikiet do szukania: "
      (if target-layer target-layer "<bez filtra warstwy>")
    )
  )

  (setq vertices (gp-usunpktmulti-poly-vertices pline))

  (if (not vertices)
    (prompt "\nNie udalo sie odczytac wierzcholkow polilinii.")
    (progn
      (prompt
        (strcat
          "\nWierzcholkow polilinii: "
          (itoa (length vertices))
        )
      )

      (prompt "\nTryb kasowania: usuwane sa rowniez punkty bazowe/wezlowa.")

      (setq filter (gp-usunpktmulti-build-filter target-layer))
      (setq ss (ssget "_X" filter))

      (prompt
        (strcat
          "\nKandydatow INSERT/POINT znalezionych filtrem: "
          (if ss (itoa (sslength ss)) "0")
        )
      )

      (if ss
        (progn
          (setq i 0)

          (while (< i (sslength ss))
            (setq ent (ssname ss i))

            (if (gp-usunpktmulti-candidate-p ent target-layer)
              (progn
                (setq candidates (1+ candidates))
                (setq pt (gp-usunpktmulti-entity-point ent))

                (if
                  (and
                    pt
                    (gp-usunpktmulti-any-match-p
                      pt
                      vertices
                      *gp-usunpktmulti-tolerance*
                    )
                  )
                  (progn
                    (setq matched (1+ matched))

                    (setq delete-result
                      (vl-catch-all-apply
                        'entdel
                        (list ent)
                      )
                    )

                    (if (not (vl-catch-all-error-p delete-result))
                      (setq deleted (1+ deleted))
                    )
                  )
                )
              )
            )

            (setq i (1+ i))
          )
        )
      )

      (prompt
        (strcat
          "\nKandydatow typu Pikieta_Geo/POINT po weryfikacji: "
          (itoa candidates)
        )
      )

      (prompt
        (strcat
          "\nPikiet dopasowanych do wierzcholkow polilinii: "
          (itoa matched)
        )
      )
    )
  )

  deleted
)


(defun c:GP_USUN_PKT_Z_MULTI
  (
    /
    *error*
    doc old-cmdecho ss ans use-prefix-filter
    i pline deleted total
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq old-cmdecho (getvar "CMDECHO"))

  (defun *error* (msg)
    (if old-cmdecho
      (setvar "CMDECHO" old-cmdecho)
    )

    (if doc
      (vl-catch-all-apply
        'vla-EndUndoMark
        (list doc)
      )
    )

    (if
      (and
        msg
        (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*"))
      )
      (prompt (strcat "\nBlad: " msg))
    )

    (princ)
  )

  (prompt "\nWybierz polilinie utworzone przez MULTI: ")
  (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE"))))

  (if ss
    (progn
      (initget "Tak Nie")
      (setq ans
        (getkword
          "\nFiltrowac pikiety po przedrostku warstwy polilinii? [Tak/Nie] <Tak>: "
        )
      )

      (setq use-prefix-filter
        (not
          (member ans '("Nie" "N"))
        )
      )

      (setvar "CMDECHO" 0)
      (vla-StartUndoMark doc)

      (setq total 0)
      (setq i 0)

      (while (< i (sslength ss))
        (setq pline (ssname ss i))

        (setq deleted
          (gp-usunpktmulti-delete-for-polyline
            pline
            use-prefix-filter
          )
        )

        (setq total (+ total deleted))
        (setq i (1+ i))
      )

      (vla-EndUndoMark doc)
      (setvar "CMDECHO" old-cmdecho)

      (prompt
        (strcat
          "\nUsunieto pikiet: "
          (itoa total)
          "."
        )
      )
    )

    (prompt "\nNie wybrano zadnej polilinii.")
  )

  (princ)
)


(defun c:USUNPKT_MULTI ()
  (c:GP_USUN_PKT_Z_MULTI)
)


(princ "\nKomendy wczytane: USUNPKT_MULTI, GP_USUN_PKT_Z_MULTI")
(princ)
