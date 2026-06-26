(vl-load-com)
(load "gp_Core.lsp" "\nBLAD: Nie znaleziono pliku gp_Core.lsp!")

;; --- FUNKCJA POMOCNICZA: Bezpieczne pobieranie punktu ---
(defun get-safe-curve-pt (crv L / total)
  (setq total (vlax-curve-getDistAtParam crv (vlax-curve-getEndParam crv)))
  (if (and (>= L -0.001) (<= L (+ total 0.001)))
    (vlax-curve-getPointAtDist crv L)
    nil
  )
)


;; --- FUNKCJA POMOCNICZA: Utworzenie / odblokowanie warstwy ---
(defun geocad-multi-ensure-layer (doc layname color / layers lay res)
  ;; Tworzy warstwe, jezeli nie istnieje.
  ;; Jezeli istnieje, wlacza ja, odmraza, odblokowuje i ustawia kolor.
  (setq layers (vla-get-Layers doc))

  (setq res
    (vl-catch-all-apply
      'vla-Item
      (list layers layname)
    )
  )

  (if (vl-catch-all-error-p res)
    (setq lay (vla-Add layers layname))
    (setq lay res)
  )

  (vl-catch-all-apply 'vla-put-Color (list lay color))
  (vl-catch-all-apply 'vla-put-LayerOn (list lay :vlax-true))
  (vl-catch-all-apply 'vla-put-Freeze (list lay :vlax-false))
  (vl-catch-all-apply 'vla-put-Lock (list lay :vlax-false))

  layname
)


;; --- FUNKCJA POMOCNICZA: Warstwa dla polilinii z NIWELACJA_MULTI ---
(defun geocad-multi-get-polyline-layer (doc / prefix color layname)
  ;; Prefix pobierany z Geo Menedzera:
  ;; Prefix + "_POLYLINES_FROM_MULTI"
  (setq prefix (geocad-get-cfg "Prefix" "POMIAR"))
  (setq color (atoi (geocad-get-cfg "Color" "3")))

  (setq layname (strcat prefix "_POLYLINES_FROM_MULTI"))

  (geocad-multi-ensure-layer doc layname color)
)


(defun c:NIWELACJA_MULTI
  (
    /
    old-err old-osmode old-cmdecho old-clayer
    crvEnt crvObj ss i obj pt
    p-proj L-base gap param deriv len Ux Uy vx vy dot L-virt
    valid-nodes node1 node2 L1 Z1 L2 Z2 dL slope
    mode step num-pts segment-step L-cur
    doc space pt-cur z-cur zlicz draw-3d poly-pts omitted
    poly-layer
  )

  ;; --- OBSLUGA BLEDOW ---
  (setq old-osmode (getvar "OSMODE"))
  (setq old-cmdecho (getvar "CMDECHO"))
  (setq old-clayer (getvar "CLAYER"))

  (setq old-err *error*
        *error*
        (lambda (msg)
          (if old-osmode
            (setvar "OSMODE" old-osmode)
          )

          (if old-cmdecho
            (setvar "CMDECHO" old-cmdecho)
          )

          (if old-clayer
            (vl-catch-all-apply 'setvar (list "CLAYER" old-clayer))
          )

          (setq *error* old-err)

          (princ (strcat "\nPrzerwano: " msg))
          (princ)
        )
  )

  ;; --- 1. WYBOR OSI ---
  (setq crvEnt (car (entsel "\n1. Wybierz os trasy (Linia/Polilinia/Luk): ")))
  (if (not crvEnt) (exit))

  (setq crvObj (vlax-ename->vla-object crvEnt))

  ;; --- 2. WYBOR MASOWY PUNKTOW BAZOWYCH ---
  (princ "\n2. Zaznacz oknem wszystkie PUNKTY/PIKIETY bazowe wzdluz trasy:")
  (setq ss (ssget '((0 . "INSERT,POINT,TEXT,MTEXT"))))
  (if (not ss)
    (progn
      (alert "Nic nie wybrano!")
      (exit)
    )
  )

  ;; --- 3. PRZETWARZANIE I SORTOWANIE WEZLOW ---
  (setq valid-nodes '() omitted 0 i 0)
  (princ "\nAnaliza i sortowanie wezlow...")

  (while (< i (sslength ss))
    (setq obj (vlax-ename->vla-object (ssname ss i)))
    (setq pt (get-pt-from-obj obj))

    ;; Filtrujemy - punkt musi istniec i miec Z rozne od 0.
    (if (and pt (caddr pt) (/= (caddr pt) 0.0))
      (progn
        ;; Obliczanie wirtualnego rzutu, czyli pikietazu.
        (setq p-proj (vlax-curve-getClosestPointTo crvObj pt))
        (setq L-base (vlax-curve-getDistAtPoint crvObj p-proj))
        (setq gap (distance (list (car pt) (cadr pt)) (list (car p-proj) (cadr p-proj))))

        (if (> gap 0.001)
          (progn
            (setq param (vlax-curve-getParamAtPoint crvObj p-proj))
            (setq deriv (vlax-curve-getFirstDeriv crvObj param))
            (setq len (distance '(0.0 0.0) (list (car deriv) (cadr deriv))))
            (setq Ux (/ (car deriv) len) Uy (/ (cadr deriv) len))
            (setq vx (- (car pt) (car p-proj)) vy (- (cadr pt) (cadr p-proj)))
            (setq dot (+ (* vx Ux) (* vy Uy)))
            (setq L-virt (+ L-base dot))
          )
          (setq L-virt L-base)
        )

        ;; Zapisujemy punkt do listy w formacie: (Pikietaz Rzedna_Z)
        (setq valid-nodes (cons (list L-virt (caddr pt)) valid-nodes))
      )
      (setq omitted (1+ omitted))
    )

    (setq i (1+ i))
  )

  ;; Weryfikacja
  (if (< (length valid-nodes) 2)
    (progn
      (alert "Za malo poprawnych punktow! Zaznacz przynajmniej 2 obiekty posiadajace rzedna Z.")
      (exit)
    )
  )

  ;; Sortowanie po pikietazu L-virt od najmniejszego do najwiekszego.
  (setq valid-nodes (vl-sort valid-nodes (function (lambda (a b) (< (car a) (car b))))))
  (princ (strcat "\n>>> Znaleziono " (itoa (length valid-nodes)) " wezlow wzdluz trasy (Pominieto: " (itoa omitted) ")."))

  ;; --- 4. METODA I OPCJE ---
  (initget 1 "Odleglosc Podzial")
  (setq mode (getkword "\n3. Metoda generowania miedzy wezlami [Odleglosc/Podzial]: "))

  (if (= mode "Odleglosc")
    (setq step (getreal "\nPodaj odstep [m]: "))
    (setq num-pts (getint "\nPodaj ilosc odcinkow miedzy KAZDA para wezlow: "))
  )

  (initget "Tak Nie")
  (setq draw-3d (getkword "\n4. Czy na koniec narysowac ciagla Polilinie 3D? [Tak/Nie] <Tak>: "))

  (if (not draw-3d)
    (setq draw-3d "Tak")
  )

  ;; --- 5. GENEROWANIE W PETLI DLA KAZDEGO SEGMENTU ---
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)) space (vla-get-ModelSpace doc) zlicz 0 poly-pts '())

  ;; Zabezpieczenie pierwszej krawedzi do 3D.
  (setq L1 (car (nth 0 valid-nodes)) Z1 (cadr (nth 0 valid-nodes)))
  (setq pt-cur (get-safe-curve-pt crvObj L1))

  (if pt-cur
    (setq poly-pts (append poly-pts (list (list (car pt-cur) (cadr pt-cur) Z1))))
  )

  (setq i 0)

  (while (< i (1- (length valid-nodes)))
    (setq node1 (nth i valid-nodes) node2 (nth (1+ i) valid-nodes))
    (setq L1 (car node1) Z1 (cadr node1))
    (setq L2 (car node2) Z2 (cadr node2))
    (setq dL (- L2 L1))

    ;; Wykonuj tylko, jesli punkty nie sa w tym samym miejscu.
    (if (> dL 0.001)
      (progn
        (setq slope (/ (- Z2 Z1) dL))
        (setq L-cur L1)

        ;; Tryb "Odleglosc"
        (if (= mode "Odleglosc")
          (progn
            (setq L-cur (+ L-cur step))

            (while (< L-cur (- L2 0.01))
              (setq z-cur (+ Z1 (* (- L-cur L1) slope)) pt-cur (get-safe-curve-pt crvObj L-cur))

              (if pt-cur
                (progn
                  (geocad-wstaw-pikiete-full doc space (list (car pt-cur) (cadr pt-cur) z-cur) "" T)
                  (setq zlicz (1+ zlicz))
                  (setq poly-pts (append poly-pts (list (list (car pt-cur) (cadr pt-cur) z-cur))))
                )
              )

              (setq L-cur (+ L-cur step))
            )
          )

          ;; Tryb "Podzial"
          (progn
            (setq segment-step (/ dL (float num-pts)))
            (setq L-cur (+ L-cur segment-step))

            (repeat (1- num-pts)
              (if (< L-cur (- L2 0.01))
                (progn
                  (setq z-cur (+ Z1 (* (- L-cur L1) slope)) pt-cur (get-safe-curve-pt crvObj L-cur))

                  (if pt-cur
                    (progn
                      (geocad-wstaw-pikiete-full doc space (list (car pt-cur) (cadr pt-cur) z-cur) "" T)
                      (setq zlicz (1+ zlicz))
                      (setq poly-pts (append poly-pts (list (list (car pt-cur) (cadr pt-cur) z-cur))))
                    )
                  )
                )
              )

              (setq L-cur (+ L-cur segment-step))
            )
          )
        )
      )
    )

    ;; Zamkniecie aktualnego segmentu dla Linii 3D.
    (setq pt-cur (get-safe-curve-pt crvObj L2))

    (if pt-cur
      (setq poly-pts (append poly-pts (list (list (car pt-cur) (cadr pt-cur) Z2))))
    )

    (setq i (1+ i))
  )

  ;; --- 6. RYSOWANIE POLILINII 3D ---
  (if (and (= draw-3d "Tak") (> (length poly-pts) 1))
    (progn
      (setvar "CMDECHO" 0)
      (setvar "OSMODE" 0)

      ;; Warstwa docelowa dla 3DPOLY:
      ;; <PREFIX>_POLYLINES_FROM_MULTI
      (setq poly-layer (geocad-multi-get-polyline-layer doc))

      ;; 3DPOLY tworzy obiekt na aktualnej warstwie,
      ;; dlatego tylko na czas tej komendy zmieniamy CLAYER.
      (setvar "CLAYER" poly-layer)

      (command "._3DPOLY")

      (foreach pt poly-pts
        (command pt)
      )

      (command "")

      ;; Przywracamy poprzednia warstwe od razu po narysowaniu polilinii.
      (if old-clayer
        (vl-catch-all-apply 'setvar (list "CLAYER" old-clayer))
      )

      (princ
        (strcat
          "\n-> Narysowano ciagla Polilinie 3D na warstwie: "
          poly-layer
        )
      )
    )
  )

  ;; --- 7. ZAKONCZENIE ---
  (setvar "OSMODE" old-osmode)
  (setvar "CMDECHO" old-cmdecho)

  (if old-clayer
    (vl-catch-all-apply 'setvar (list "CLAYER" old-clayer))
  )

  (setq *error* old-err)

  (princ (strcat "\nSukces! Wygenerowano " (itoa zlicz) " pikiet we wszystkich segmentach."))
  (princ)
)


(princ "\nKomenda: NIWELACJA_MULTI wczytana.")
(princ)