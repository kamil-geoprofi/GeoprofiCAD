;; ======================================================
;; GEOPROFICAD - PIKIETA FACTORY
;; ======================================================
;;
;; Tworzenie i batchowe wstawianie pikiet.
;; Shadow split: definicje sa zgodne z gp_CoreLegacy.lsp
;; i sa ladowane po legacy, bez zmiany publicznego API.
;; ======================================================

(setq *geocad-module-pikietafactory-loaded* T)

(defun geocad-stworz-blok-pikieta ()
  (if (not (tblsearch "BLOCK" "Pikieta_Geo"))
    (progn
      (entmake
        (list
          '(0 . "BLOCK")
          '(2 . "Pikieta_Geo")
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
          '(1 . "---")
          '(2 . "NR")
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
          '(1 . "0.00")
          '(2 . "H")
          '(3 . "H")
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
  )
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

  ;; show-z zostaje w sygnaturze dla kompatybilnosci.
  ;; Obecna stara funkcja tez realnie opierala widocznosc H na ustawieniu Display.
  (setq txt-h (geocad-ctx-get 'txt-h ctx))
  (setq z-prec (geocad-ctx-get 'z-prec ctx))
  (setq styl (geocad-ctx-get 'styl ctx))
  (setq pikt-pref (geocad-ctx-get 'pikt-pref ctx))

  (setq lay-pt (geocad-ctx-get 'lay-pt ctx))
  (setq lay-nr (geocad-ctx-get 'lay-nr ctx))
  (setq lay-h (geocad-ctx-get 'lay-h ctx))

  (setq vis-nr (geocad-ctx-get 'vis-nr ctx))
  (setq vis-h (geocad-ctx-get 'vis-h ctx))

  (setq dX (geocad-ctx-get 'dX ctx))
  (setq dY (geocad-ctx-get 'dY ctx))

  (if (not nr-str)
    (setq nr-str "")
  )

  (setq nr-str (vl-princ-to-string nr-str))
  (setq pelny-nr (strcat pikt-pref nr-str))

  (setq px (car pt-list))
  (setq py (cadr pt-list))
  (setq pz (caddr pt-list))

  (if (not pz)
    (setq pz 0.0)
  )

  (setq pt-list (list px py pz))
  (setq pt-3d (vlax-3d-point pt-list))
  (setq z-str (rtos pz 2 z-prec))

  (if (= styl "Tekst")
    (progn
      (entmakex
        (list
          '(0 . "POINT")
          (cons 10 pt-list)
          (cons 8 lay-pt)
        )
      )

      (if (= vis-nr :vlax-false)
        (entmakex
          (list
            '(0 . "TEXT")
            (cons 10 (list (+ px dX) (+ py dY) pz))
            (cons 40 txt-h)
            (cons 1 pelny-nr)
            (cons 8 lay-nr)
          )
        )
      )

      (if (= vis-h :vlax-false)
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
    )

    (progn
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

        (cond
          ((= (vla-get-TagString att) "NR")
            (vla-put-TextString att pelny-nr)
            (vla-put-InsertionPoint
              att
              (vlax-3d-point (list (+ px dX) (+ py dY) pz))
            )
            (vla-put-Invisible att vis-nr)
            (vla-put-Layer att lay-nr)
          )

          ((member (vla-get-TagString att) '("H" "Z" "RZEDNA"))
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
    ctx next-nr actual-nr
  )

  (setq ctx (geocad-ctx-get 'ctx batch))

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
