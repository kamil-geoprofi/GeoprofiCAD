(vl-load-com)

;; ======================================================
;; GEOPROFICAD - CENTRALNA KONFIGURACJA
;; ======================================================
;;
;; Zasada modelu warstw:
;;
;; Prefix / grupa robocza = czysty identyfikator logiczny
;; Np.:
;;   DROGI
;;   KANAL
;;   WODA
;;
;; Warstwa AutoCAD = prefix + sufiks klasy
;; Np.:
;;   DROGI_PIKIETY
;;   DROGI_ETYKIETA_NR
;;   DROGI_ETYKIETA_H
;;   DROGI_POLYLINES_FROM_MULTI
;;
;; Nigdy nie zapisujemy jako prefixu pelnej nazwy warstwy typu:
;;   DROGI_PIKIETY
;;
;; Taka wartosc jest normalizowana do:
;;   DROGI
;; ======================================================

(if (not *geocad-config-loaded*)
  (progn
    (setq *geocad-config-loaded* T)

    ;; ------------------------------------------------------
    ;; Rejestr ustawien uzytkownika - fallback globalny.
    ;; Pamiec rysunku jest obslugiwana przez vlax-ldata.
    ;; ------------------------------------------------------
    (setq *geocad-registry-path*
      "HKEY_CURRENT_USER\\Software\\GeoCadSkrypty"
    )

    ;; ------------------------------------------------------
    ;; Slownik LDATA dla pamieci konkretnego DWG.
    ;; ------------------------------------------------------
    (setq *geocad-ldata-setup-dict* "GeoSetup")

    ;; ------------------------------------------------------
    ;; Typy warstw - traktuj jak enum
    ;; ------------------------------------------------------
    (setq *geocad-layer-type-points* "PIKIETY")
    (setq *geocad-layer-type-label-nr* "ETYKIETA_NR")
    (setq *geocad-layer-type-label-h* "ETYKIETA_H")
    (setq *geocad-layer-type-polyline-multi* "POLYLINES_MULTI")

    ;; ------------------------------------------------------
    ;; Sufiksy warstw GeoprofiCAD
    ;; ------------------------------------------------------
    (setq *geocad-layer-suffix-points* "_PIKIETY")
    (setq *geocad-layer-suffix-label-nr* "_ETYKIETA_NR")
    (setq *geocad-layer-suffix-label-h* "_ETYKIETA_H")
    (setq *geocad-layer-suffix-polyline-multi* "_POLYLINES_FROM_MULTI")

    (setq *geocad-managed-layer-suffixes*
      (list
        *geocad-layer-suffix-points*
        *geocad-layer-suffix-label-nr*
        *geocad-layer-suffix-label-h*
        *geocad-layer-suffix-polyline-multi*
      )
    )

    ;; ------------------------------------------------------
    ;; Zwraca sufiks dla typu warstwy
    ;; ------------------------------------------------------
    (defun geocad-layer-suffix (layer-type)
      (cond
        ((= layer-type *geocad-layer-type-points*)
          *geocad-layer-suffix-points*
        )
        ((= layer-type *geocad-layer-type-label-nr*)
          *geocad-layer-suffix-label-nr*
        )
        ((= layer-type *geocad-layer-type-label-h*)
          *geocad-layer-suffix-label-h*
        )
        ((= layer-type *geocad-layer-type-polyline-multi*)
          *geocad-layer-suffix-polyline-multi*
        )
        (T "")
      )
    )

    ;; ------------------------------------------------------
    ;; Bezpieczne przyciecie tekstu z bialych znakow
    ;; ------------------------------------------------------
    (defun geocad-trim-string (val)
      (if val
        (vl-string-trim " \t\r\n" val)
        ""
      )
    )

    ;; ------------------------------------------------------
    ;; Case-insensitive ends-with.
    ;; ------------------------------------------------------
    (defun geocad-string-ends-with-p
      (txt suffix / txt-up suffix-up txt-len suffix-len start-pos)
      (setq txt-up (strcase (if txt txt "")))
      (setq suffix-up (strcase (if suffix suffix "")))

      (setq txt-len (strlen txt-up))
      (setq suffix-len (strlen suffix-up))

      (if
        (and
          (> suffix-len 0)
          (>= txt-len suffix-len)
        )
        (progn
          (setq start-pos (+ 1 (- txt-len suffix-len)))
          (= (substr txt-up start-pos suffix-len) suffix-up)
        )
        nil
      )
    )

    ;; ------------------------------------------------------
    ;; Zdejmuje jeden znany sufiks warstwy GeoprofiCAD.
    ;;
    ;; DROGI_PIKIETY              -> DROGI
    ;; DROGI_ETYKIETA_NR         -> DROGI
    ;; DROGI_POLYLINES_FROM_MULTI -> DROGI
    ;; ------------------------------------------------------
    (defun geocad-strip-managed-layer-suffix
      (name / result suffix)
      (setq result (geocad-trim-string name))

      (foreach suffix *geocad-managed-layer-suffixes*
        (if
          (and
            (/= result "")
            (geocad-string-ends-with-p result suffix)
            (> (strlen result) (strlen suffix))
          )
          (setq result
            (substr
              result
              1
              (- (strlen result) (strlen suffix))
            )
          )
        )
      )

      (geocad-trim-string result)
    )

    ;; ------------------------------------------------------
    ;; Normalizuje prefix / grupe robocza.
    ;;
    ;; To zabezpiecza przed:
    ;;   DROGI_PIKIETY
    ;;   DROGI_PIKIETY_PIKIETY
    ;;
    ;; I sprowadza to do czystego prefixu:
    ;;   DROGI
    ;; ------------------------------------------------------
    (defun geocad-normalize-layer-prefix
      (prefix / old new)
      (setq new (geocad-trim-string prefix))
      (setq old nil)

      (while (/= old new)
        (setq old new)
        (setq new (geocad-strip-managed-layer-suffix new))
      )

      new
    )

    ;; ------------------------------------------------------
    ;; Buduje pelna nazwe warstwy z prefiksu i typu.
    ;;
    ;; Prefix jest zawsze normalizowany.
    ;;
    ;; (geocad-layer-name "DROGI" *geocad-layer-type-points*)
    ;; -> "DROGI_PIKIETY"
    ;;
    ;; (geocad-layer-name "DROGI_PIKIETY" *geocad-layer-type-points*)
    ;; -> "DROGI_PIKIETY"
    ;; a nie:
    ;; -> "DROGI_PIKIETY_PIKIETY"
    ;; ------------------------------------------------------
    (defun geocad-layer-name (prefix layer-type)
      (strcat
        (geocad-normalize-layer-prefix prefix)
        (geocad-layer-suffix layer-type)
      )
    )

    ;; ------------------------------------------------------
    ;; Wyciaga czysty prefix z nazwy warstwy zarzadzanej.
    ;;
    ;; DROGI_PIKIETY              -> DROGI
    ;; DROGI_ETYKIETA_NR         -> DROGI
    ;; DROGI_POLYLINES_FROM_MULTI -> DROGI
    ;;
    ;; Jezeli warstwa nie jest warstwa GeoprofiCAD,
    ;; zwraca nil.
    ;; ------------------------------------------------------
    (defun geocad-managed-layer-prefix-from-name
      (layer-name / layer-clean suffix result)
      (setq result nil)
      (setq layer-clean (geocad-trim-string layer-name))

      (if (/= layer-clean "")
        (foreach suffix *geocad-managed-layer-suffixes*
          (if
            (and
              (not result)
              (geocad-string-ends-with-p layer-clean suffix)
              (> (strlen layer-clean) (strlen suffix))
            )
            (setq result
              (geocad-normalize-layer-prefix
                (substr
                  layer-clean
                  1
                  (- (strlen layer-clean) (strlen suffix))
                )
              )
            )
          )
        )
      )

      result
    )

    ;; ------------------------------------------------------
    ;; Dodaje string do listy tylko jezeli jest niepusty i unikalny
    ;; ------------------------------------------------------
    (defun geocad-add-unique-string (val lst)
      (if
        (and
          val
          (/= val "")
          (not (member val lst))
        )
        (append lst (list val))
        lst
      )
    )

    ;; ------------------------------------------------------
    ;; Dodaje prefix do listy po normalizacji.
    ;; ------------------------------------------------------
    (defun geocad-add-unique-prefix
      (prefix lst / pref)
      (setq pref (geocad-normalize-layer-prefix prefix))

      (if
        (and
          pref
          (/= pref "")
          (not (member pref lst))
        )
        (append lst (list pref))
        lst
      )
    )
  )
)

(princ)