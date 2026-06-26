(vl-load-com)
(load "gp_Core.lsp" "\nBLAD: Nie znaleziono pliku gp_Core.lsp!")

;; ======================================================
;; GEOPROFICAD - USUWANIE PIKIET Z ZAKRESU NUMEROW
;;
;; Komendy:
;;   USUNPIKIETY_ZAKRES
;;   USUNPKT_ZAKRES
;;   GP_USUN_PIKIETY_ZAKRES
;;
;; Dzialanie:
;;   - bierze aktualny Prefix z GEO_SETUP do wyboru grupy warstw,
;;   - prefix numeru moze byc: Aktualny / Wszystkie / Konkretny,
;;   - usuwa pikiety tylko z aktualnej grupy warstw:
;;       <PREFIX>_PIKIETY
;;       <PREFIX>_ETYKIETA_NR
;;       <PREFIX>_ETYKIETA_H
;;   - styl Blok: usuwa INSERT bloku Pikieta_Geo po atrybucie NR,
;;   - styl Tekst: usuwa TEXT/MTEXT numeru z zakresu oraz powiazany POINT i H.
;; ======================================================

(if (not (boundp '*gp-usunzakres-point-tolerance*))
  (setq *gp-usunzakres-point-tolerance* 0.01)
)

(if (not (boundp '*gp-usunzakres-label-tolerance*))
  (setq *gp-usunzakres-label-tolerance* 0.05)
)

(defun gp-usunzakres-vla-object (ent / res)
  (if ent
    (progn
      (setq res
        (vl-catch-all-apply
          'vlax-ename->vla-object
          (list ent)
        )
      )

      (if (vl-catch-all-error-p res)
        nil
        res
      )
    )
    nil
  )
)

(defun gp-usunzakres-effective-block-name (ent / obj res data)
  (setq data (entget ent))
  (setq obj (gp-usunzakres-vla-object ent))

  (if obj
    (progn
      (setq res
        (vl-catch-all-apply
          'vla-get-EffectiveName
          (list obj)
        )
      )

      (if (vl-catch-all-error-p res)
        (cdr (assoc 2 data))
        res
      )
    )
    (cdr (assoc 2 data))
  )
)

(defun gp-usunzakres-block-pikieta-p (ent / data typ block-name)
  (setq data (entget ent))
  (setq typ (cdr (assoc 0 data)))

  (and
    (= typ "INSERT")
    (progn
      (setq block-name (gp-usunzakres-effective-block-name ent))
      (and block-name (= (strcase block-name) "PIKIETA_GEO"))
    )
  )
)

(defun gp-usunzakres-entity-point (ent / data typ)
  (setq data (entget ent))
  (setq typ (cdr (assoc 0 data)))

  (cond
    ((member typ '("POINT" "INSERT" "TEXT" "MTEXT"))
      (cdr (assoc 10 data))
    )
    (T nil)
  )
)

(defun gp-usunzakres-entity-text (ent / obj res data typ)
  (setq data (entget ent))
  (setq typ (cdr (assoc 0 data)))

  (if (member typ '("TEXT" "MTEXT"))
    (progn
      (setq obj (gp-usunzakres-vla-object ent))

      (if obj
        (progn
          (setq res
            (vl-catch-all-apply
              'vla-get-TextString
              (list obj)
            )
          )

          (if (vl-catch-all-error-p res)
            (cdr (assoc 1 data))
            res
          )
        )
        (cdr (assoc 1 data))
      )
    )
    nil
  )
)

(defun gp-usunzakres-block-nr-text (ent / obj atts att result)
  (setq result nil)

  (if (gp-usunzakres-block-pikieta-p ent)
    (progn
      (setq obj (gp-usunzakres-vla-object ent))

      (if obj
        (progn
          (setq atts
            (vl-catch-all-apply
              'vlax-invoke
              (list obj 'GetAttributes)
            )
          )

          (if (not (vl-catch-all-error-p atts))
            (foreach att atts
              (if (= (strcase (vla-get-TagString att)) "NR")
                (setq result (vla-get-TextString att))
              )
            )
          )
        )
      )
    )
  )

  result
)

(defun gp-usunzakres-string-prefix-p (txt pref)
  (and
    txt
    pref
    (> (strlen pref) 0)
    (>= (strlen txt) (strlen pref))
    (=
      (strcase (substr txt 1 (strlen pref)))
      (strcase pref)
    )
  )
)

(defun gp-usunzakres-digits-only-p (txt / ok chars ch)
  (setq ok T)

  (if (or (not txt) (= txt ""))
    (setq ok nil)
    (progn
      (setq chars (vl-string->list txt))

      (foreach ch chars
        (if (or (< ch 48) (> ch 57))
          (setq ok nil)
        )
      )
    )
  )

  ok
)


(defun gp-usunzakres-trailing-number-from-text (txt / raw len pos ch digits)
  ;; Zwraca koncowy numer z tekstu niezaleznie od prefixu.
  ;;
  ;; Przyklady:
  ;; "123"       -> 123
  ;; "P123"      -> 123
  ;; "ETAP_A123" -> 123
  ;; "P123A"     -> nil
  (if txt
    (progn
      (setq raw (geocad-trim-string (vl-princ-to-string txt)))
      (setq len (strlen raw))
      (setq pos len)

      (while
        (and
          (> pos 0)
          (setq ch (ascii (substr raw pos 1)))
          (>= ch 48)
          (<= ch 57)
        )
        (setq pos (1- pos))
      )

      (if (< pos len)
        (progn
          (setq digits (substr raw (1+ pos)))

          (if (gp-usunzakres-digits-only-p digits)
            (atoi digits)
            nil
          )
        )

        nil
      )
    )

    nil
  )
)


(defun gp-usunzakres-number-from-text-by-mode
  (
    txt mode filter-prefix
  )

  ;; mode:
  ;; "Aktualny"  - numer musi miec aktualny PiktPrefix z GEO_SETUP,
  ;;               a jezeli aktualny prefix jest pusty, tekst musi byc samymi cyframi.
  ;; "Wszystkie" - ignoruje prefix numeru i bierze koncowa liczbe z tekstu.
  ;; "Konkretny" - numer musi miec prefix wpisany przez uzytkownika.
  (cond
    ((= mode "Wszystkie")
      (gp-usunzakres-trailing-number-from-text txt)
    )

    ((= mode "Konkretny")
      (gp-usunzakres-number-from-text txt filter-prefix)
    )

    (T
      (gp-usunzakres-number-from-text txt filter-prefix)
    )
  )
)


(defun gp-usunzakres-number-from-text (txt pikt-pref / raw)
  (if txt
    (progn
      (setq raw (geocad-trim-string (vl-princ-to-string txt)))

      (if (and pikt-pref (/= pikt-pref ""))
        (if (gp-usunzakres-string-prefix-p raw pikt-pref)
          (setq raw
            (substr raw (1+ (strlen pikt-pref)))
          )
        )
      )

      (setq raw (geocad-trim-string raw))

      (if (gp-usunzakres-digits-only-p raw)
        (atoi raw)
        nil
      )
    )
    nil
  )
)

(defun gp-usunzakres-number-in-range-p (nr from-nr to-nr)
  (and
    nr
    (numberp nr)
    (>= nr from-nr)
    (<= nr to-nr)
  )
)

(defun gp-usunzakres-xy-distance (a b)
  (distance
    (list (car a) (cadr a))
    (list (car b) (cadr b))
  )
)

(defun gp-usunzakres-point-match-p (a b tol / za zb)
  (and
    a
    b
    (numberp (car a))
    (numberp (cadr a))
    (numberp (car b))
    (numberp (cadr b))
    (<= (gp-usunzakres-xy-distance a b) tol)
    (progn
      (setq za (if (caddr a) (caddr a) 0.0))
      (setq zb (if (caddr b) (caddr b) 0.0))
      (<= (abs (- za zb)) tol)
    )
  )
)

(defun gp-usunzakres-layer-filter (types layer)
  (list
    (cons 0 types)
    (cons 8 layer)
  )
)

(defun gp-usunzakres-delete-near-point
  (
    layer target-pt tol
    /
    ss i ent pt deleted
  )

  (setq deleted 0)

  (if (and layer target-pt)
    (progn
      (setq ss
        (ssget
          "X"
          (gp-usunzakres-layer-filter "POINT" layer)
        )
      )

      (if ss
        (progn
          (setq i 0)

          (while (< i (sslength ss))
            (setq ent (ssname ss i))
            (setq pt (gp-usunzakres-entity-point ent))

            (if (gp-usunzakres-point-match-p pt target-pt tol)
              (progn
                (entdel ent)
                (setq deleted (1+ deleted))
              )
            )

            (setq i (1+ i))
          )
        )
      )
    )
  )

  deleted
)

(defun gp-usunzakres-delete-near-label
  (
    layer target-pt tol
    /
    ss i ent pt deleted
  )

  (setq deleted 0)

  (if (and layer target-pt)
    (progn
      (setq ss
        (ssget
          "X"
          (gp-usunzakres-layer-filter "TEXT,MTEXT" layer)
        )
      )

      (if ss
        (progn
          (setq i 0)

          (while (< i (sslength ss))
            (setq ent (ssname ss i))
            (setq pt (gp-usunzakres-entity-point ent))

            (if
              (and
                pt
                (<= (gp-usunzakres-xy-distance pt target-pt) tol)
              )
              (progn
                (entdel ent)
                (setq deleted (1+ deleted))
              )
            )

            (setq i (1+ i))
          )
        )
      )
    )
  )

  deleted
)

(defun gp-usunzakres-delete-blocks
  (
    lay-pt number-prefix-mode number-filter-prefix from-nr to-nr
    /
    ss i ent txt nr deleted
  )

  (setq deleted 0)

  (setq ss
    (ssget
      "X"
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
        (setq ent (ssname ss i))
        (setq txt (gp-usunzakres-block-nr-text ent))
        (setq nr
          (gp-usunzakres-number-from-text-by-mode
            txt
            number-prefix-mode
            number-filter-prefix
          )
        )

        (if (gp-usunzakres-number-in-range-p nr from-nr to-nr)
          (progn
            (entdel ent)
            (setq deleted (1+ deleted))
          )
        )

        (setq i (1+ i))
      )
    )
  )

  deleted
)

(defun gp-usunzakres-delete-text-style
  (
    lay-pt lay-nr lay-h number-prefix-mode number-filter-prefix from-nr to-nr
    /
    txt-h dX dY
    ss i ent label-pt base-pt h-pt
    nr-text nr
    deleted-nr deleted-pt deleted-h
  )

  (setq deleted-nr 0)
  (setq deleted-pt 0)
  (setq deleted-h 0)

  (setq txt-h (atof (geocad-get-cfg "TxtH" "1.0")))
  (setq dX (* txt-h 1.2))
  (setq dY (* txt-h 0.7))

  (setq ss
    (ssget
      "X"
      (gp-usunzakres-layer-filter "TEXT,MTEXT" lay-nr)
    )
  )

  (if ss
    (progn
      (setq i 0)

      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq nr-text (gp-usunzakres-entity-text ent))
        (setq nr
          (gp-usunzakres-number-from-text-by-mode
            nr-text
            number-prefix-mode
            number-filter-prefix
          )
        )

        (if (gp-usunzakres-number-in-range-p nr from-nr to-nr)
          (progn
            (setq label-pt (gp-usunzakres-entity-point ent))

            (if label-pt
              (progn
                ;; NR = base + (dX, +dY)
                ;; H  = base + (dX, -dY)
                (setq base-pt
                  (list
                    (- (car label-pt) dX)
                    (- (cadr label-pt) dY)
                    (if (caddr label-pt) (caddr label-pt) 0.0)
                  )
                )

                (setq h-pt
                  (list
                    (car label-pt)
                    (- (cadr label-pt) (* 2.0 dY))
                    (if (caddr label-pt) (caddr label-pt) 0.0)
                  )
                )

                (setq deleted-pt
                  (+
                    deleted-pt
                    (gp-usunzakres-delete-near-point
                      lay-pt
                      base-pt
                      *gp-usunzakres-point-tolerance*
                    )
                  )
                )

                (setq deleted-h
                  (+
                    deleted-h
                    (gp-usunzakres-delete-near-label
                      lay-h
                      h-pt
                      *gp-usunzakres-label-tolerance*
                    )
                  )
                )
              )
            )

            (entdel ent)
            (setq deleted-nr (1+ deleted-nr))
          )
        )

        (setq i (1+ i))
      )
    )
  )

  (list deleted-nr deleted-pt deleted-h)
)

(defun gp-usunzakres-print-summary
  (
    lay-pt lay-nr lay-h from-nr to-nr
    number-prefix-mode number-filter-prefix
    block-deleted text-stats
    /
    text-nr-deleted text-point-deleted text-h-deleted total prefix-info
  )

  (setq text-nr-deleted (nth 0 text-stats))
  (setq text-point-deleted (nth 1 text-stats))
  (setq text-h-deleted (nth 2 text-stats))

  (setq total
    (+
      block-deleted
      text-nr-deleted
      text-point-deleted
      text-h-deleted
    )
  )

  (setq prefix-info
    (cond
      ((= number-prefix-mode "Wszystkie")
        "Wszystkie prefixy numeru - numer czytany z koncowych cyfr"
      )

      ((and number-filter-prefix (/= number-filter-prefix ""))
        (strcat number-prefix-mode ": " number-filter-prefix)
      )

      (T
        (strcat number-prefix-mode ": <brak prefixu numeru>")
      )
    )
  )

  (princ
    (strcat
      "\n\nUsuwanie zakonczone."
      "\nZakres numerow: "
      (itoa from-nr)
      " - "
      (itoa to-nr)
      "\nFiltr prefixu numeru: "
      prefix-info
      "\nWarstwa punktow/blokow: "
      lay-pt
      "\nWarstwa etykiet NR: "
      lay-nr
      "\nWarstwa etykiet H: "
      lay-h
      "\nUsuniete bloki Pikieta_Geo: "
      (itoa block-deleted)
      "\nUsuniete etykiety NR stylu Tekst: "
      (itoa text-nr-deleted)
      "\nUsuniete punkty POINT stylu Tekst: "
      (itoa text-point-deleted)
      "\nUsuniete etykiety H stylu Tekst: "
      (itoa text-h-deleted)
      "\nRazem usunietych obiektow: "
      (itoa total)
    )
  )
)

(defun gp-usunzakres-command
  (
    /
    old-err doc undo-started
    prefix pikt-pref number-prefix-mode number-filter-prefix custom-prefix
    lay-pt lay-nr lay-h
    from-nr to-nr tmp confirm
    block-deleted text-stats
  )

  (setq undo-started nil)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  (setq old-err *error*
        *error*
        (lambda (msg)
          (if undo-started
            (progn
              (vl-catch-all-apply 'vla-EndUndoMark (list doc))
              (setq undo-started nil)
            )
          )

          (setq *error* old-err)

          (if (not (member msg '("Function cancelled" "quit / exit abort")))
            (princ (strcat "\nPrzerwano: " msg))
            (princ "\nPrzerwano.")
          )

          (princ)
        )
  )

  (setq prefix (geocad-trim-string (geocad-get-cfg "Prefix" "POMIAR")))
  (setq pikt-pref (geocad-trim-string (geocad-get-cfg "PiktPrefix" "")))

  (if (= prefix "")
    (setq prefix "POMIAR")
  )

  (setq lay-pt (geocad-layer-name prefix *geocad-layer-type-points*))
  (setq lay-nr (geocad-layer-name prefix *geocad-layer-type-label-nr*))
  (setq lay-h  (geocad-layer-name prefix *geocad-layer-type-label-h*))

  (princ "\nGEOPROFICAD - USUWANIE PIKIET Z ZAKRESU NUMEROW")
  (princ "\n=================================================")
  (princ
    (strcat
      "\nAktualny Prefix z GEO_SETUP: "
      prefix
      "\nAktualny PiktPrefix: "
      (if (= pikt-pref "") "<brak>" pikt-pref)
      "\nWarstwa punktow/blokow: "
      lay-pt
    )
  )

  (initget "Aktualny Wszystkie Konkretny")
  (setq number-prefix-mode
    (getkword
      "\nFiltr prefixu numeru [Aktualny/Wszystkie/Konkretny] <Aktualny>: "
    )
  )

  (if (not number-prefix-mode)
    (setq number-prefix-mode "Aktualny")
  )

  (cond
    ((= number-prefix-mode "Aktualny")
      (setq number-filter-prefix pikt-pref)
    )

    ((= number-prefix-mode "Wszystkie")
      (setq number-filter-prefix "")
    )

    ((= number-prefix-mode "Konkretny")
      (setq custom-prefix
        (getstring T "\nPodaj konkretny prefix numeru pikiety, np. P albo ETAP_A: ")
      )

      (setq number-filter-prefix (geocad-trim-string custom-prefix))
    )
  )

  (princ
    (strcat
      "\nTryb numeracji: "
      number-prefix-mode
      (if (= number-prefix-mode "Wszystkie")
        " - usuwanie po koncowej liczbie niezaleznie od prefixu numeru."
        (strcat
          " - prefix numeru: "
          (if (= number-filter-prefix "") "<brak>" number-filter-prefix)
        )
      )
    )
  )

  (setq from-nr (getint "\nPodaj numer poczatkowy zakresu: "))

  (if (not from-nr)
    (exit)
  )

  (setq to-nr (getint "\nPodaj numer koncowy zakresu: "))

  (if (not to-nr)
    (exit)
  )

  (if (> from-nr to-nr)
    (progn
      (setq tmp from-nr)
      (setq from-nr to-nr)
      (setq to-nr tmp)
    )
  )

  (initget "Tak Nie")
  (setq confirm
    (getkword
      (strcat
        "\nUsunac pikiety od "
        (itoa from-nr)
        " do "
        (itoa to-nr)
        " z aktualnej grupy "
        prefix
        " / filtr numeru: "
        number-prefix-mode
        "? [Tak/Nie] <Nie>: "
      )
    )
  )

  (if (not confirm)
    (setq confirm "Nie")
  )

  (if (/= confirm "Tak")
    (progn
      (princ "\nAnulowano. Nic nie usunieto.")
      (setq *error* old-err)
      (princ)
      (exit)
    )
  )

  (vla-StartUndoMark doc)
  (setq undo-started T)

  (setq block-deleted
    (gp-usunzakres-delete-blocks
      lay-pt
      number-prefix-mode
      number-filter-prefix
      from-nr
      to-nr
    )
  )

  (setq text-stats
    (gp-usunzakres-delete-text-style
      lay-pt
      lay-nr
      lay-h
      number-prefix-mode
      number-filter-prefix
      from-nr
      to-nr
    )
  )

  (vla-EndUndoMark doc)
  (setq undo-started nil)

  (setq *error* old-err)

  (gp-usunzakres-print-summary
    lay-pt
    lay-nr
    lay-h
    from-nr
    to-nr
    number-prefix-mode
    number-filter-prefix
    block-deleted
    text-stats
  )

  (princ)
)

(defun c:USUNPIKIETY_ZAKRES ()
  (gp-usunzakres-command)
)

(defun c:USUNPKT_ZAKRES ()
  (gp-usunzakres-command)
)

(defun c:GP_USUN_PIKIETY_ZAKRES ()
  (gp-usunzakres-command)
)

(princ "\nKomenda: USUNPIKIETY_ZAKRES wczytana.")
(princ)