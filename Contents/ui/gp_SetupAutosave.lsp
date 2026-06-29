;; ======================================================
;; GEOPROFICAD - SETUP AUTOSAVE UI HELPERS
;; ======================================================
;;
;; Autosave i walidacja pol dialogu GEO_SETUP.
;; Shadow split: definicje sa zgodne z gp_CoreLegacy.lsp
;; i sa ladowane po legacy, bez zmiany publicznego API.
;; ======================================================

(setq *geocad-module-setupautosave-loaded* T)

(defun geocad-setup-try-save-txt-h
  (doc group-prefix old-txt-h kolor z-prec styl display / raw norm val)
  (if
    (or
      (not old-txt-h)
      (= old-txt-h "")
    )
    (setq old-txt-h "1.0")
  )

  (setq raw (get_tile "txt_h"))
  (setq norm (geocad-setup-normalize-real-text raw))
  (setq val (geocad-setup-positive-real-from-text raw))

  (if
    (not val)
    (progn
      (set_tile "txt_h" old-txt-h)
      (set_tile "txt_h_status" "BLAD - wysokosc tekstu musi byc liczba > 0. Przywrocono poprzednia wartosc.")
      (set_tile "dirty_status" "BLAD - nie zapisano wysokosci tekstu.")
      (list nil old-txt-h)
    )
    (progn
      (set_tile "txt_h" norm)

      (if (/= norm old-txt-h)
        (progn
          (geocad-setup-save-group-param group-prefix "TxtH" norm)

          (geocad-setup-apply-current-group-params
            doc
            group-prefix
            kolor
            norm
            z-prec
            styl
            display
          )

          (set_tile "txt_h_status" (strcat "(Zmieniono " old-txt-h " -> " norm ")"))
          (set_tile "dirty_status" (strcat "ZAPISANO wysokosc tekstu i zaktualizowano grupe " group-prefix "."))
        )
      )

      (list T norm)
    )
  )
)

(defun geocad-setup-try-save-z-prec
  (doc group-prefix old-z-prec kolor txt-h styl display / raw norm val)
  (if
    (or
      (not old-z-prec)
      (= old-z-prec "")
    )
    (setq old-z-prec "2")
  )

  (setq raw (get_tile "z_prec"))
  (setq norm (geocad-trim-string raw))

  (if
    (not (geocad-setup-digits-only-p norm))
    (progn
      (set_tile "z_prec" old-z-prec)
      (set_tile "z_prec_status" "BLAD - precyzja Z musi byc liczba calkowita 0-8. Przywrocono poprzednia wartosc.")
      (set_tile "dirty_status" "BLAD - nie zapisano precyzji Z.")
      (list nil old-z-prec)
    )
    (progn
      (setq val (atoi norm))

      (if
        (or
          (< val 0)
          (> val 8)
        )
        (progn
          (set_tile "z_prec" old-z-prec)
          (set_tile "z_prec_status" "BLAD - precyzja Z musi byc w zakresie 0-8. Przywrocono poprzednia wartosc.")
          (set_tile "dirty_status" "BLAD - nie zapisano precyzji Z.")
          (list nil old-z-prec)
        )
        (progn
          (setq norm (itoa val))
          (set_tile "z_prec" norm)

          (if (/= norm old-z-prec)
            (progn
              (geocad-setup-save-group-param group-prefix "Prec" norm)

              (geocad-setup-apply-current-group-params
                doc
                group-prefix
                kolor
                txt-h
                norm
                styl
                display
              )

              (set_tile "z_prec_status" (strcat "(Zmieniono " old-z-prec " -> " norm ")"))
              (set_tile "dirty_status" (strcat "ZAPISANO precyzje Z i zaktualizowano grupe " group-prefix "."))
            )
          )

          (list T norm)
        )
      )
    )
  )
)

(defun geocad-setup-autosave-style
  (doc group-prefix old-styl kolor txt-h z-prec display / idx new-styl)
  (setq idx (get_tile "styl_rys"))

  (setq new-styl
    (if (= idx "1")
      "Tekst"
      "Blok"
    )
  )

  (if (/= new-styl old-styl)
    (progn
      (geocad-setup-save-group-param group-prefix "Styl" new-styl)

      (geocad-setup-apply-current-group-params
        doc
        group-prefix
        kolor
        txt-h
        z-prec
        new-styl
        display
      )

      (set_tile
        "styl_status"
        (strcat
          "(Zmieniono "
          (geocad-setup-style-label old-styl)
          " -> "
          (geocad-setup-style-label new-styl)
          ")"
        )
      )

      (set_tile "dirty_status" (strcat "ZAPISANO styl i przekonwertowano grupe " group-prefix "."))
    )
  )

  new-styl
)

(defun geocad-setup-autosave-display
  (doc group-prefix old-display kolor txt-h z-prec styl / idx new-display)
  (setq idx (get_tile "display_mode"))
  (setq new-display (geocad-display-from-popup-index idx))

  (if (/= new-display old-display)
    (progn
      (geocad-setup-save-group-param group-prefix "Display" new-display)

      (geocad-setup-apply-current-group-params
        doc
        group-prefix
        kolor
        txt-h
        z-prec
        styl
        new-display
      )

      (set_tile
        "display_status"
        (strcat
          "(Zmieniono "
          (geocad-setup-display-label old-display)
          " -> "
          (geocad-setup-display-label new-display)
          ")"
        )
      )

      (set_tile "dirty_status" (strcat "ZAPISANO widocznosc i zaktualizowano grupe " group-prefix "."))
    )
  )

  new-display
)

(defun geocad-setup-autosave-color
  (doc group-prefix old-color txt-h z-prec styl display / idx new-color)
  (setq idx (atoi (get_tile "kolor")))
  (setq new-color (itoa (1+ idx)))

  (if (/= new-color old-color)
    (progn
      (geocad-setup-save-group-param group-prefix "Color" new-color)

      (geocad-setup-apply-current-group-params
        doc
        group-prefix
        new-color
        txt-h
        z-prec
        styl
        display
      )

      (set_tile
        "kolor_status"
        (strcat
          "(Zmieniono "
          (geocad-setup-color-label old-color)
          " -> "
          (geocad-setup-color-label new-color)
          ")"
        )
      )

      (set_tile "dirty_status" (strcat "ZAPISANO kolor i zaktualizowano grupe " group-prefix "."))
    )
  )

  new-color
)

(princ)
