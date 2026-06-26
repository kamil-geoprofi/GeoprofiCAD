(vl-load-com)

;; ======================================================
;; GEOPROFICAD - CENTRALNA KONFIGURACJA
;; ======================================================
;;
;; Ten plik trzyma stale standardy aplikacji:
;; - sciezke rejestru,
;; - typy warstw,
;; - sufiksy warstw,
;; - funkcje budowania i rozpoznawania nazw warstw.
;;
;; Nie trzymamy tutaj aktualnie wybranego prefiksu uzytkownika.
;; Aktualny Prefix nadal jest zapisywany w rejestrze jako ustawienie uzytkownika.
;; ======================================================

(if (not *geocad-config-loaded*)
  (progn
    (setq *geocad-config-loaded* T)

    ;; ------------------------------------------------------
    ;; Rejestr ustawien uzytkownika
    ;; ------------------------------------------------------
    (setq *geocad-registry-path*
      "HKEY_CURRENT_USER\\Software\\GeoCadSkrypty"
    )

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
    ;; Buduje pelna nazwe warstwy z prefiksu i typu
    ;; Np.:
    ;; (geocad-layer-name "POMIAR" *geocad-layer-type-points*)
    ;; -> "POMIAR_PIKIETY"
    ;; ------------------------------------------------------
    (defun geocad-layer-name (prefix layer-type)
      (strcat prefix (geocad-layer-suffix layer-type))
    )

    ;; ------------------------------------------------------
    ;; Wyciaga prefiks z nazwy warstwy zarzadzanej przez GeoprofiCAD
    ;; Np.:
    ;; "POMIAR_PIKIETY" -> "POMIAR"
    ;; "POMIAR_ETYKIETA_NR" -> "POMIAR"
    ;; "POMIAR_POLYLINES_FROM_MULTI" -> "POMIAR"
    ;;
    ;; Jezeli warstwa nie pasuje do standardu GeoprofiCAD, zwraca nil.
    ;; ------------------------------------------------------
    (defun geocad-managed-layer-prefix-from-name
      (layer-name / layer-up suffix suffix-up pos result)
      (setq result nil)

      (if layer-name
        (progn
          (setq layer-up (strcase layer-name))

          (foreach suffix *geocad-managed-layer-suffixes*
            (if (not result)
              (progn
                (setq suffix-up (strcase suffix))
                (setq pos (vl-string-search suffix-up layer-up))

                ;; pos jest 0-based.
                ;; Dopuszczamy tylko sytuacje, gdzie przed sufiksem jest realny prefiks.
                (if (and pos (> pos 0))
                  (setq result (substr layer-name 1 pos))
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
      (if (and val (/= val "") (not (member val lst)))
        (append lst (list val))
        lst
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
  )
)

(princ)