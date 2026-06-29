;; ======================================================
;; GEOPROFICAD - PIKIETA FACTORY
;; ======================================================
;;
;; Tworzenie i batchowe wstawianie pikiet.
;; Shadow split: definicje sa zgodne z gp_CoreLegacy.lsp
;; i sa ladowane po legacy, bez zmiany publicznego API.
;; ======================================================

(setq *geocad-module-pikietafactory-loaded* T)

(defun geocad-ctx-get (key ctx)
  (cdr (assoc key ctx))
)

(defun geocad-ctx-set (key val ctx / pair)
  (setq pair (assoc key ctx))

  (if pair
    (subst (cons key val) pair ctx)
    (cons (cons key val) ctx)
  )
)

(defun geocad-pikieta-prepare-context
  (
    doc
    /
    txt-h z-prec prefix kolor styl display pikt-pref
    lay-pt lay-nr lay-h
    vis-nr vis-h dX dY
  )
  ;; Jezeli uzytkownik wstawia pikiety bez wchodzenia w GEO_SETUP,
  ;; nadal inicjalizujemy pamiec DWG z realnego rysunku.
  (geocad-ensure-dwg-setup-initialized doc)
  ;; Ustawienia czytamy raz na serie pikiet.
  (setq txt-h (atof (geocad-get-cfg "TxtH" "1.0")))
  (setq z-prec (atoi (geocad-get-cfg "Prec" "2")))
  (setq prefix (geocad-normalize-layer-prefix (geocad-get-cfg "Prefix" "POMIAR")))
  (setq kolor (atoi (geocad-get-cfg "Color" "3")))
  (setq styl (geocad-get-cfg "Styl" "Blok"))
  (setq display (geocad-get-cfg "Display" "Oba"))
  (setq pikt-pref (geocad-trim-string (geocad-get-cfg "PiktPrefix" "")))

  (if (= prefix "")
    (setq prefix "POMIAR")
  )

  (setq lay-pt
    (geocad-layer-name prefix *geocad-layer-type-points*)
  )

  (setq lay-nr
    (geocad-layer-name prefix *geocad-layer-type-label-nr*)
  )

  (setq lay-h
    (geocad-layer-name prefix *geocad-layer-type-label-h*)
  )

  ;; Warstwy przygotowujemy raz.
  (geocad-ensure-layer doc lay-pt kolor)
  (geocad-ensure-layer doc lay-nr kolor)
  (geocad-ensure-layer doc lay-h kolor)

  ;; Blok tez przygotowujemy raz, a nie przy kazdej pikiecie.
  (if (= styl "Blok")
    (geocad-stworz-blok-pikieta)
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

  (setq dX (* txt-h 1.2))
  (setq dY (* txt-h 0.7))

  (list
    (cons 'txt-h txt-h)
    (cons 'z-prec z-prec)
    (cons 'prefix prefix)
    (cons 'kolor kolor)
    (cons 'styl styl)
    (cons 'display display)
    (cons 'pikt-pref pikt-pref)
    (cons 'lay-pt lay-pt)
    (cons 'lay-nr lay-nr)
    (cons 'lay-h lay-h)
    (cons 'vis-nr vis-nr)
    (cons 'vis-h vis-h)
    (cons 'dX dX)
    (cons 'dY dY)
    (cons 'doc doc)
  )
)

(defun geocad-wstaw-pikiete-with-context
  (
    doc space pt-list nr-str show-z ctx
    /
    txt-h z-prec styl pikt-pref
    lay-pt lay-nr lay-h pelny-nr
  )
  ;; show-z zostaje w sygnaturze dla kompatybilnosci.
  ;; Factory odpowiada za kontekst, prefix i numeracje, a tworzenie
  ;; encji deleguje do gp_PikietaWriters.lsp.
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
    (geocad-insert-pikieta-block-from-data
      (geocad-ctx-get 'doc ctx)
      pt-list
      pelny-nr
      txt-h
      z-prec
      (geocad-ctx-get 'display ctx)
      lay-pt
      lay-nr
      lay-h
      space
    )
  )

  (princ)
)

(defun geocad-pikieta-batch-start (doc / ctx)
  ;; Start sesji masowego wstawiania pikiet.
  ;; Numer automatyczny pobieramy leniwie dopiero przy pierwszym insercie auto.
  (setq ctx (geocad-pikieta-prepare-context doc))

  (list
    (cons 'ctx ctx)
    (cons 'next-nr nil)
    (cons 'auto-used nil)
  )
)

(defun geocad-pikieta-batch-insert
  (
    batch space pt-list nr-str show-z
    /
    ctx next-nr actual-nr group
  )

  (setq ctx (geocad-ctx-get 'ctx batch))
  (setq group (geocad-ctx-get 'prefix ctx))

  (if
    (and
      (geocad-imported-group-p group)
      (not (geocad-imported-group-unlocked-p group))
      (or (not nr-str) (= nr-str ""))
    )
    (progn
      (alert "Aktywna grupa imported ma zalozony bezpiecznik. W GEO_SETUP odbezpiecz grupe albo przelacz na grupe generated.")
      (exit)
    )
  )

  (if (or (not nr-str) (= nr-str ""))
    (progn
      ;; Pierwszy numer pobieramy tylko raz.
      ;; GP:PobierzNastepnyNumer od razu zapisuje kolejny numer,
      ;; a batch-end na koncu nadpisze go finalnym stanem po calej serii.
      (setq next-nr (geocad-ctx-get 'next-nr batch))

      (if (not next-nr)
        (setq next-nr (atoi (GP:PobierzNastepnyNumer)))
      )

      (setq actual-nr (itoa next-nr))
      (setq next-nr (1+ next-nr))

      (setq batch (geocad-ctx-set 'next-nr next-nr batch))
      (setq batch (geocad-ctx-set 'auto-used T batch))
    )

    (setq actual-nr (vl-princ-to-string nr-str))
  )

  (geocad-wstaw-pikiete-with-context
    nil
    space
    pt-list
    actual-nr
    show-z
    ctx
  )

  batch
)

(defun geocad-pikieta-batch-end (batch / ctx next-nr group pikt-pref)
  ;; Zapisuje finalny licznik tylko wtedy, gdy w sesji uzyto auto-numeracji.
  ;; Licznik zapisujemy per:
  ;; - grupa robocza,
  ;; - prefix numeracji pikiety.
  (if (and batch (geocad-ctx-get 'auto-used batch))
    (progn
      (setq ctx (geocad-ctx-get 'ctx batch))
      (setq next-nr (geocad-ctx-get 'next-nr batch))
      (setq group (geocad-ctx-get 'prefix ctx))
      (setq pikt-pref (geocad-ctx-get 'pikt-pref ctx))

      (if next-nr
        (progn
          (geocad-save-known-pikt-prefix-for-group group pikt-pref)
          (geocad-set-pikt-counter group pikt-pref next-nr)
        )
      )

      (setq batch (geocad-ctx-set 'auto-used nil batch))
    )
  )

  batch
)

(defun geocad-wstaw-pikiete-full (doc space pt-list nr-str show-z / batch)
  ;; Kompatybilny wrapper dla starych wywolan.
  ;; Dla pojedynczej pikiety zachowuje stare API.
  ;; Dla nowych komend masowych lepiej uzywac batch-start/insert/end.
  (setq batch (geocad-pikieta-batch-start doc))
  (setq batch
    (geocad-pikieta-batch-insert
      batch
      space
      pt-list
      nr-str
      show-z
    )
  )
  (setq batch (geocad-pikieta-batch-end batch))

  (princ)
)

(princ)
