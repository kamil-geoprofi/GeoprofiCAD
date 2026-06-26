(vl-load-com)
(load "gp_Core.lsp" "\nBLAD: Nie znaleziono pliku gp_Core.lsp!")

;; --- FUNKCJA POMOCNICZA: Bezpieczne rzutowanie 2D (Widok z góry) ---
;; Ignoruje różnice wysokości Z, zapobiega "ślizganiu" się rzutów po liniach 3D
(defun get-safe-proj-2d (crv pt / p-proj)
  (setq p-proj
    (vl-catch-all-apply
      'vlax-curve-getClosestPointToProjection
      (list crv pt '(0.0 0.0 1.0))
    )
  )

  (if (vl-catch-all-error-p p-proj)
    (vlax-curve-getClosestPointTo crv pt) ;; Fallback awaryjny
    p-proj
  )
)


(defun c:NIWELACJA_KRAWEDZI
  (
    /
    old-err
    srcEnt srcObj
    ssAll i obj
    pt projPt ptsList
    tgtEnt tgtObj
    slope-pct
    doc space
    pt-src pt-tgt
    dist2d z-tgt pt-list
    is-orthogonal
    param deriv
    Ux Uy vx vy dot lenU cos-theta
    skipped-nr zlicz
    safe-pt-tgt param-catch
    batch
  )

  (setq old-err *error*
        *error*
        (lambda (msg)
          (if batch
            (progn
              (setq batch (geocad-pikieta-batch-end batch))
              (setq batch nil)
            )
          )

          (setq *error* old-err)

          (princ (strcat "\nPrzerwano: " msg))
          (princ)
        )
  )

  (setq srcEnt
    (car
      (entsel "\n1. Wybierz linie ZRODLOWA (na ktorej sa juz pikiety): ")
    )
  )

  (if (not srcEnt)
    (exit)
  )

  (setq srcObj (vlax-ename->vla-object srcEnt))

  (princ "\nSkanowanie rysunku...")

  (setq ssAll
    (ssget
      "X"
      '(
        (0 . "INSERT,POINT")
        (-4 . "<OR")
        (2 . "Pikieta_Geo")
        (0 . "POINT")
        (-4 . "OR>")
      )
    )
  )

  (setq ptsList '())

  (if ssAll
    (progn
      (setq i 0)

      (while (< i (sslength ssAll))
        (setq obj (vlax-ename->vla-object (ssname ssAll i)))
        (setq pt (get-pt-from-obj obj))

        (if pt
          (progn
            ;; UŻYWAMY RZUTU 2D (Ignoruje "Z")
            (setq projPt (get-safe-proj-2d srcObj pt))

            (if projPt
              (progn
                ;; Powiększona tolerancja łapania pikiet do 5 cm (0.05 m)
                (if
                  (<
                    (distance
                      (list (car pt) (cadr pt))
                      (list (car projPt) (cadr projPt))
                    )
                    0.05
                  )
                  (setq ptsList (cons pt ptsList))
                )
              )
            )
          )
        )

        (setq i (1+ i))
      )
    )
  )

  (if (null ptsList)
    (progn
      (alert "Nie znaleziono pikiet lezacych na linii zrodlowej!")
      (exit)
    )
  )

  (princ
    (strcat
      "\nZnaleziono "
      (itoa (length ptsList))
      " pikiet."
    )
  )

  (setq tgtEnt
    (car
      (entsel "\n2. Wybierz linie DOCELOWA (na ktora przeniesc pikiety): ")
    )
  )

  (if (not tgtEnt)
    (exit)
  )

  (setq tgtObj (vlax-ename->vla-object tgtEnt))

  (setq slope-pct
    (getreal "\n3. Podaj spadek poprzeczny [%] (- w dol, + w gore): ")
  )

  (if (not slope-pct)
    (exit)
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq space (vla-get-ModelSpace doc))
  (setq skipped-nr 0)
  (setq zlicz 0)

  (princ "\nGenerowanie pikiet...")

  (foreach pt-src (reverse ptsList)

    ;; RZUTOWANIE 2D NA KRAWĘDŹ DOCELOWĄ
    (setq pt-tgt (get-safe-proj-2d tgtObj pt-src))

    (if pt-tgt
      (progn
        (setq dist2d
          (distance
            (list (car pt-src) (cadr pt-src))
            (list (car pt-tgt) (cadr pt-tgt))
          )
        )

        (setq is-orthogonal T)

        (if (> dist2d 0.001)
          (progn
            ;; Dociągnięcie matematyczne ułamków milimetra, żeby getParamAtPoint nie wariował
            (setq safe-pt-tgt
              (vlax-curve-getClosestPointTo tgtObj pt-tgt)
            )

            (setq param-catch
              (vl-catch-all-apply
                'vlax-curve-getParamAtPoint
                (list tgtObj safe-pt-tgt)
              )
            )

            (if (not (vl-catch-all-error-p param-catch))
              (progn
                (setq param param-catch)
                (setq deriv (vlax-curve-getFirstDeriv tgtObj param))

                (setq Ux (car deriv))
                (setq Uy (cadr deriv))

                (setq vx (- (car pt-src) (car pt-tgt)))
                (setq vy (- (cadr pt-src) (cadr pt-tgt)))

                (setq dot
                  (abs
                    (+
                      (* Ux vx)
                      (* Uy vy)
                    )
                  )
                )

                (setq lenU
                  (distance
                    '(0.0 0.0)
                    (list Ux Uy)
                  )
                )

                ;; Obliczenie kąta (Zabezpieczone przed przerwaniem skryptu)
                (if (> (* lenU dist2d) 0.0)
                  (setq cos-theta (/ dot (* lenU dist2d)))
                  (setq cos-theta 0.0)
                )

                ;; Tolerancja prostopadłości poluzowana do 0.15 (ok. 8,6 stopnia).
                ;; Skrypt nie urywa się na drobnych krzywiznach.
                (if (> cos-theta 0.15)
                  (setq is-orthogonal nil)
                )
              )
              (setq is-orthogonal nil)
            )
          )
        )

        (if is-orthogonal
          (progn
            (setq z-tgt
              (+
                (caddr pt-src)
                (* dist2d (/ slope-pct 100.0))
              )
            )

            (setq pt-list
              (list
                (car pt-tgt)
                (cadr pt-tgt)
                z-tgt
              )
            )

            ;; Batch startuje dopiero przy pierwszej faktycznie wstawianej pikiecie.
            (if (not batch)
              (setq batch (geocad-pikieta-batch-start doc))
            )

            (setq batch
              (geocad-pikieta-batch-insert
                batch
                space
                pt-list
                nil
                T
              )
            )

            (setq zlicz (1+ zlicz))
          )

          (setq skipped-nr (1+ skipped-nr))
        )
      )
    )
  )

  ;; Zapis finalnego licznika po wygenerowaniu pikiet.
  (if batch
    (progn
      (setq batch (geocad-pikieta-batch-end batch))
      (setq batch nil)
    )
  )

  (setq *error* old-err)

  (princ
    (strcat
      "\nSukces! Wygenerowano "
      (itoa zlicz)
      " nowych pikiet."
    )
  )

  (if (> skipped-nr 0)
    (princ
      (strcat
        " (Zignorowano "
        (itoa skipped-nr)
        " pikiet rzutujacych sie poza fizyczna dlugosc linii)."
      )
    )
  )

  (princ)
)


(princ "\nKomenda wczytana: NIWELACJA_KRAWEDZI")
(princ)