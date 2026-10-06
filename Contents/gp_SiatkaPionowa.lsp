(vl-load-com)
(load "gp_Core.lsp" "\nBLAD: Nie znaleziono pliku gp_Core.lsp!")

;; ======================================================
;; GEOPROFICAD - SIATKA PIONOWA
;; Komenda: SIATKA_PIONOWA
;;
;; Funkcja:
;; - wybiera zamkniety obrys,
;; - generuje pikiety na obrysie tym samym mechanizmem,
;;   ktory jest uzywany przez SIATKA_PUNKTOW,
;; - powtarza ten zestaw punktow na kolejnych poziomach Z,
;; - kierunek: Gora / Dol,
;; - uzywa jednego batcha pikiet dla calej operacji.
;;
;; WAZNE:
;; - nie duplikujemy logiki probkowania obrysu,
;; - nie duplikujemy logiki tworzenia pikiet,
;; - seen jest resetowane dla kazdego poziomu,
;;   poniewaz geocad-grid-key deduplikuje po XY.
;; ======================================================


;; ------------------------------------------------------
;; Helpery geometrii siatki sa obecnie zdefiniowane
;; w gp_SiatkaPunktow.lsp.
;;
;; PackageContents powinien ladowac gp_SiatkaPunktow
;; przed tym modulem, ale zostawiamy fallback dla
;; recznego APPLOAD.
;; ------------------------------------------------------
(if (not geocad-grid-insert-border-points)
  (load
    "gp_SiatkaPunktow.lsp"
    "\nBLAD: Nie znaleziono pliku gp_SiatkaPunktow.lsp!"
  )
)


(defun c:SIATKA_PIONOWA
  (
    /
    olderr
    acad doc space
    undo-started
    batch

    ent obj len
    border-step
    add-breakpoints

    base-z
    default-z

    direction
    z-sign
    level-count
    vertical-step

    level-index
    level-z

    seen
    cnt
    cnt-before
    cnt-level

    res
  )

  ;; ------------------------------------------------------
  ;; Stan poczatkowy
  ;; ------------------------------------------------------

  (setq olderr *error*)
  (setq undo-started nil)
  (setq batch nil)
  (setq cnt 0)


  ;; ------------------------------------------------------
  ;; Error handler
  ;; ------------------------------------------------------

  (defun *error* (msg)

    ;; Zamkniecie batcha zapisuje finalny licznik pikiet,
    ;; jezeli zostala uzyta automatyczna numeracja.
    (if batch
      (progn
        (setq batch (geocad-pikieta-batch-end batch))
        (setq batch nil)
      )
    )

    ;; Zamkniecie grupy UNDO.
    (if (and doc undo-started)
      (progn
        (vl-catch-all-apply
          'vla-EndUndoMark
          (list doc)
        )
        (setq undo-started nil)
      )
    )

    (setq *error* olderr)

    (if msg
      (if
        (not
          (member
            msg
            '(
              "Function cancelled"
              "quit / exit abort"
            )
          )
        )

        (princ
          (strcat
            "\nPrzerwano: "
            msg
          )
        )

        (princ "\nPrzerwano.")
      )
    )

    (princ)
  )


  ;; ------------------------------------------------------
  ;; AutoCAD
  ;; ------------------------------------------------------

  (setq acad
    (vlax-get-acad-object)
  )

  (setq doc
    (vla-get-ActiveDocument acad)
  )

  (setq space
    (vla-get-ModelSpace doc)
  )


  (princ "\n==============================================")
  (princ "\nGEOPROFICAD - SIATKA PIONOWA")
  (princ "\n==============================================")


  ;; ------------------------------------------------------
  ;; 1. Wybor obrysu
  ;; ------------------------------------------------------

  (setq ent
    (car
      (entsel
        "\nWybierz zamkniety obrys: "
      )
    )
  )

  (if (not ent)
    (progn
      (princ "\nNie wybrano obrysu.")
      (setq *error* olderr)
      (princ)
      (exit)
    )
  )

  (setq obj
    (vlax-ename->vla-object ent)
  )


  ;; ------------------------------------------------------
  ;; 2. Sprawdzenie krzywej
  ;; ------------------------------------------------------

  (setq len
    (geocad-grid-safe-curve-length obj)
  )

  (if (not len)
    (progn
      (alert
        "Wybrany obiekt nie jest obslugiwanym obrysem."
      )

      (setq *error* olderr)
      (princ)
      (exit)
    )
  )


  ;; ------------------------------------------------------
  ;; 3. Sprawdzenie zamkniecia
  ;; ------------------------------------------------------

  (if
    (not
      (geocad-grid-closed-p obj)
    )

    (progn
      (alert
        "Wybrany obrys nie jest zamkniety."
      )

      (setq *error* olderr)
      (princ)
      (exit)
    )
  )


  ;; ------------------------------------------------------
  ;; 4. Rozstaw punktow wzdluz obrysu
  ;; ------------------------------------------------------

  (setq border-step
    (getreal
      "\nPodaj rozstaw punktow na obrysie [m] <1.00>: "
    )
  )

  (if (not border-step)
    (setq border-step 1.0)
  )

  (if (<= border-step 0.0)
    (progn
      (alert
        "Rozstaw punktow musi byc wiekszy od zera."
      )

      (setq *error* olderr)
      (princ)
      (exit)
    )
  )


  ;; ------------------------------------------------------
  ;; 5. Narozniki / zalamania
  ;; ------------------------------------------------------

  (initget "Tak Nie")

  (setq add-breakpoints
    (getkword
      "\nDodac punkty w naroznikach/zalamaniach? [Tak/Nie] <Tak>: "
    )
  )

  (if (not add-breakpoints)
    (setq add-breakpoints "Tak")
  )


  ;; ------------------------------------------------------
  ;; 6. Bazowa rzedna Z
  ;; ------------------------------------------------------

  ;; Korzystamy z tego samego helpera co SIATKA_PUNKTOW.
  ;; Typowa polilinia 2D zwroci swoje elevation/start Z.

  (setq default-z
    (geocad-grid-get-test-z obj)
  )

  (setq base-z
    (getreal
      (strcat
        "\nPodaj bazowa rzedna Z <"
        (rtos default-z 2 3)
        ">: "
      )
    )
  )

  (if (not base-z)
    (setq base-z default-z)
  )


  ;; ------------------------------------------------------
  ;; 7. Kierunek
  ;; ------------------------------------------------------

  (initget "Gora Dol")

  (setq direction
    (getkword
      "\nKierunek generowania [Gora/Dol] <Gora>: "
    )
  )

  (if (not direction)
    (setq direction "Gora")
  )

  (if (= direction "Dol")
    (setq z-sign -1.0)
    (setq z-sign 1.0)
  )


  ;; ------------------------------------------------------
  ;; 8. Liczba poziomow
  ;; ------------------------------------------------------

  (setq level-count
    (getint
      "\nPodaj liczbe poziomow punktow: "
    )
  )

  (if
    (or
      (not level-count)
      (<= level-count 0)
    )

    (progn
      (alert
        "Liczba poziomow musi byc wieksza od zera."
      )

      (setq *error* olderr)
      (princ)
      (exit)
    )
  )


  ;; ------------------------------------------------------
  ;; 9. Odstep pionowy
  ;; ------------------------------------------------------

  (setq vertical-step
    (getreal
      "\nPodaj odstep pionowy pomiedzy poziomami [m]: "
    )
  )

  (if
    (or
      (not vertical-step)
      (<= vertical-step 0.0)
    )

    (progn
      (alert
        "Odstep pionowy musi byc wiekszy od zera."
      )

      (setq *error* olderr)
      (princ)
      (exit)
    )
  )


  ;; ------------------------------------------------------
  ;; 10. Start operacji
  ;; ------------------------------------------------------

  (vla-StartUndoMark doc)
  (setq undo-started T)

  ;; Jeden batch dla WSZYSTKICH poziomow.
  ;; Nie tworzymy nowego contextu dla kazdej warstwy Z.
  (setq batch
    (geocad-pikieta-batch-start doc)
  )

  (setq cnt 0)
  (setq level-index 0)


  ;; ------------------------------------------------------
  ;; 11. Generowanie kolejnych poziomow
  ;; ------------------------------------------------------

  (while (< level-index level-count)

    ;; Poziom 0 = base-z.
    ;;
    ;; Gora:
    ;; base
    ;; base + step
    ;; base + 2*step
    ;;
    ;; Dol:
    ;; base
    ;; base - step
    ;; base - 2*step

    (setq level-z
      (+
        base-z
        (*
          z-sign
          vertical-step
          level-index
        )
      )
    )


    ;; ====================================================
    ;; WAZNE
    ;;
    ;; geocad-grid-key rozpoznaje duplikaty po XY.
    ;;
    ;; Dlatego seen MUSI byc nowe dla kazdego poziomu.
    ;; W przeciwnym razie punkt:
    ;;
    ;; X=100 Y=200 Z=120
    ;;
    ;; zablokowalby:
    ;;
    ;; X=100 Y=200 Z=121
    ;; ====================================================

    (setq seen '())

    (setq cnt-before cnt)


    ;; ----------------------------------------------------
    ;; Narozniki / zalamania
    ;; ----------------------------------------------------

    (if (= add-breakpoints "Tak")
      (progn

        (setq res
          (geocad-grid-insert-breakpoints
            obj
            space
            level-z
            seen
            batch
            cnt
          )
        )

        (setq seen
          (car res)
        )

        (setq batch
          (cadr res)
        )

        (setq cnt
          (caddr res)
        )
      )
    )


    ;; ----------------------------------------------------
    ;; Punkty wzdluz obrysu
    ;;
    ;; To jest TEN SAM helper co w SIATKA_PUNKTOW.
    ;; Nie ma tutaj drugiej implementacji probkowania.
    ;; ----------------------------------------------------

    (setq res
      (geocad-grid-insert-border-points
        obj
        space
        level-z
        border-step
        seen
        batch
        cnt
      )
    )

    (setq seen
      (car res)
    )

    (setq batch
      (cadr res)
    )

    (setq cnt
      (caddr res)
    )


    ;; ----------------------------------------------------
    ;; Informacja o poziomie
    ;; ----------------------------------------------------

    (setq cnt-level
      (- cnt cnt-before)
    )

    (princ
      (strcat
        "\nPoziom "
        (itoa (1+ level-index))
        "/"
        (itoa level-count)
        " | Z="
        (rtos level-z 2 3)
        " | pikiet: "
        (itoa cnt-level)
      )
    )


    (setq level-index
      (1+ level-index)
    )
  )


  ;; ------------------------------------------------------
  ;; 12. Finalizacja batcha
  ;; ------------------------------------------------------

  (if batch
    (progn
      (setq batch
        (geocad-pikieta-batch-end batch)
      )

      (setq batch nil)
    )
  )


  ;; ------------------------------------------------------
  ;; 13. Koniec UNDO
  ;; ------------------------------------------------------

  (if undo-started
    (progn
      (vla-EndUndoMark doc)
      (setq undo-started nil)
    )
  )


  ;; ------------------------------------------------------
  ;; 14. Koniec
  ;; ------------------------------------------------------

  (setq *error* olderr)

  (princ
    (strcat
      "\nSukces. Wygenerowano "
      (itoa cnt)
      " pikiet."
      "\nLiczba poziomow: "
      (itoa level-count)
      "\nKierunek: "
      direction
      "\nOdstep pionowy: "
      (rtos vertical-step 2 3)
      " m"
    )
  )

  (princ)
)


(princ "\nKomenda wczytana: SIATKA_PIONOWA")
(princ)