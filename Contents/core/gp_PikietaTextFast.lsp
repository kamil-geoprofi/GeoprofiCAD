;; ======================================================
;; GEOPROFICAD - PIKIETA TEXT FAST PATH
;; ======================================================
;;
;; Szybka sciezka tworzenia wariantu tekstowego pikiety.
;; Celowo zgodna z szybka logika importu:
;; - czyste entmakex,
;; - bez vlax-ename->vla-object dla nowych TEXT,
;; - bez vla-put-Visible,
;; - tworzy tylko te teksty, ktore maja byc widoczne.
;;
;; Brakujace teksty sa odtwarzane pozniej przez update wariantu
;; tekstowego, jezeli uzytkownik zmieni widocznosc.
;; ======================================================

(setq *geocad-module-pikietatextfast-loaded* T)

(defun geocad-create-text-pikieta
  (
    pt-list nr-str txt-h z-prec display
    lay-pt lay-nr lay-h
    /
    px py pz dX dY z-str show-nr show-h
  )
  ;; POINT + opcjonalny TEXT NR + opcjonalny TEXT H.
  ;; To nadpisuje bazowa wersje, ktora tworzyla oba teksty i sterowala
  ;; widocznoscia przez COM Visible, co bylo zbyt wolne przy tysiacach pikiet.

  (setq nr-str (geocad-pikieta-empty-nr-if-needed nr-str))

  (setq px (car pt-list))
  (setq py (cadr pt-list))
  (setq pz (caddr pt-list))

  (if (not pz)
    (setq pz 0.0)
  )

  (setq pt-list (list px py pz))
  (setq dX (* txt-h 1.2))
  (setq dY (* txt-h 0.7))
  (setq z-str (rtos pz 2 z-prec))

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

  (entmakex
    (list
      '(0 . "POINT")
      (cons 10 pt-list)
      (cons 8 lay-pt)
    )
  )

  (if show-nr
    (entmakex
      (list
        '(0 . "TEXT")
        (cons 10 (list (+ px dX) (+ py dY) pz))
        (cons 40 txt-h)
        (cons 1 nr-str)
        (cons 8 lay-nr)
      )
    )
  )

  (if show-h
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

  T
)

(princ)
