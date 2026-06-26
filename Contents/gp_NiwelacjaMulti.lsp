(vl-load-com)
(load "gp_Core.lsp" "\nBLAD: Nie znaleziono pliku gp_Core.lsp!")


;; --- FUNKCJA POMOCNICZA: Bezpieczne pobieranie punktu z krzywej po pikietazu ---
(defun get-safe-curve-pt (crv L / total-res total pt-res)
  (setq total-res
    (vl-catch-all-apply
      'vlax-curve-getDistAtParam
      (list crv (vlax-curve-getEndParam crv))
    )
  )

  (if (or (vl-catch-all-error-p total-res) (not (numberp total-res)))
    nil

    (progn
      (setq total total-res)

      (if (and (numberp L) (>= L -0.001) (<= L (+ total 0.001)))
        (progn
          (setq pt-res
            (vl-catch-all-apply
              'vlax-curve-getPointAtDist
              (list crv L)
            )
          )

          (if (vl-catch-all-error-p pt-res)
            nil
            pt-res
          )
        )

        nil
      )
    )
  )
)


;; --- FUNKCJA POMOCNICZA: Sprawdzenie, czy obiekt nadaje sie na os trasy ---
(defun geocad-multi-valid-curve-p (crvObj / end-param-res dist-res)
  (setq end-param-res
    (vl-catch-all-apply
      'vlax-curve-getEndParam
      (list crvObj)
    )
  )

  (if (or (vl-catch-all-error-p end-param-res) (not (numberp end-param-res)))
    nil

    (progn
      (setq dist-res
        (vl-catch-all-apply
          'vlax-curve-getDistAtParam
          (list crvObj end-param-res)
        )
      )

      (if (and (not (vl-catch-all-error-p dist-res)) (numberp dist-res) (> dist-res 0.001))
        T
        nil
      )
    )
  )
)


;; --- FUNKCJA POMOCNICZA: Sufit matematyczny dla liczb dodatnich ---
(defun geocad-multi-ceil-positive (val / base)
  ;; Np.:
  ;; 4.00 -> 4
  ;; 4.01 -> 5
  ;; 4.99 -> 5
  (setq base (fix val))

  (if (> val (+ base 0.0000001))
    (1+ base)
    base
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


;; --- FUNKCJA POMOCNICZA: Aktualna warstwa pikiet GeoprofiCAD ---
(defun geocad-multi-get-current-point-layer ( / prefix)
  ;; Ta sama konwencja, ktorej uzywa wstawianie pikiet:
  ;; <Prefix>_PIKIETY.
  (setq prefix (geocad-trim-string (geocad-get-cfg "Prefix" "POMIAR")))

  (if (= prefix "")
    (setq prefix "POMIAR")
  )

  (geocad-layer-name prefix *geocad-layer-type-points*)
)


;; --- FUNKCJA POMOCNICZA: Warstwa dla polilinii z NIWELACJA_MULTI ---
(defun geocad-multi-get-polyline-layer (doc / prefix color layname)
  ;; Prefix pobierany z Geo Menedzera:
  ;; Prefix + standardowy sufiks warstwy polilinii MULTI.
  (setq prefix (geocad-trim-string (geocad-get-cfg "Prefix" "POMIAR")))
  (setq color (atoi (geocad-get-cfg "Color" "3")))

  (if (= prefix "")
    (setq prefix "POMIAR")
  )

  (setq layname
    (geocad-layer-name prefix *geocad-layer-type-polyline-multi*)
  )

  (geocad-multi-ensure-layer doc layname color)
)


;; --- FUNKCJA POMOCNICZA: Bezpieczna nazwa bloku ---
(defun geocad-multi-get-block-name (obj / res)
  ;; Dla blokow dynamicznych EffectiveName zwraca nazwe definicji,
  ;; a Name moze byc anonimowe typu *U123.
  (setq res
    (vl-catch-all-apply
      'vla-get-EffectiveName
      (list obj)
    )
  )

  (if (or (vl-catch-all-error-p res) (not res))
    (progn
      (setq res
        (vl-catch-all-apply
          'vla-get-Name
          (list obj)
        )
      )

      (if (vl-catch-all-error-p res)
        nil
        res
      )
    )

    res
  )
)


;; --- FUNKCJA POMOCNICZA: Bezpieczna nazwa warstwy ---
(defun geocad-multi-get-object-layer (obj / res)
  (setq res
    (vl-catch-all-apply
      'vla-get-Layer
      (list obj)
    )
  )

  (if (vl-catch-all-error-p res)
    nil
    res
  )
)


;; --- FUNKCJA POMOCNICZA: Czy obiekt moze byc wezlem bazowym MULTI ---
;; --- FUNKCJA POMOCNICZA: Czy obiekt moze byc wezlem bazowym MULTI ---
(defun geocad-multi-supported-base-object-p
  (
    obj point-layer
    /
    type layer block-name managed-prefix
  )

  ;; Automat i tryb reczny biora tylko:
  ;;
  ;; 1. INSERT bloku Pikieta_Geo.
  ;;    Warstwa NIE jest sprawdzana, bo sam blok jest jednoznaczny.
  ;;    To obsluguje styl "Blok".
  ;;
  ;; 2. POINT na dowolnej zarzadzanej warstwie *_PIKIETY.
  ;;    To obsluguje styl "Tekst", gdzie POINT ma prawdziwe XYZ,
  ;;    a teksty NR/H sa tylko opisami obok.
  ;;
  ;; TEXT/MTEXT celowo NIE sa obslugiwane.

  (setq type
    (vl-catch-all-apply
      'vla-get-ObjectName
      (list obj)
    )
  )

  (if (vl-catch-all-error-p type)
    nil

    (cond
      ;; Blok Pikieta_Geo przyjmujemy niezaleznie od warstwy.
      ((= type "AcDbBlockReference")
        (setq block-name (geocad-multi-get-block-name obj))

        (if
          (and
            block-name
            (= (strcase block-name) "PIKIETA_GEO")
          )
          T
          nil
        )
      )

      ;; POINT przyjmujemy tylko z warstwy zarzadzanej GeoprofiCAD typu *_PIKIETY.
      ((= type "AcDbPoint")
        (setq layer (geocad-multi-get-object-layer obj))

        (if layer
          (progn
            (setq managed-prefix
              (geocad-managed-layer-prefix-from-name layer)
            )

            (if
              (and
                managed-prefix
                (= 
                  (strcase layer)
                  (strcase
                    (geocad-layer-name
                      managed-prefix
                      *geocad-layer-type-points*
                    )
                  )
                )
              )
              T
              nil
            )
          )

          nil
        )
      )

      (T nil)
    )
  )
)


;; --- FUNKCJA POMOCNICZA: Bezpieczna odleglosc XY ---
(defun geocad-multi-distance-xy (pt1 pt2 / x1 y1 x2 y2)
  (if
    (and
      pt1 pt2
      (numberp (car pt1))
      (numberp (cadr pt1))
      (numberp (car pt2))
      (numberp (cadr pt2))
    )
    (progn
      (setq x1 (car pt1))
      (setq y1 (cadr pt1))
      (setq x2 (car pt2))
      (setq y2 (cadr pt2))

      (distance
        (list x1 y1)
        (list x2 y2)
      )
    )

    nil
  )
)


;; --- FUNKCJA POMOCNICZA: Bezpieczny rzut punktu na krzywa ---
(defun geocad-multi-safe-closest-point (crvObj pt / pt-xy res)
  ;; Przynaleznosc do osi sprawdzamy po XY.
  ;; Rzutujemy punkt z Z=0 na krzywa w kierunku osi Z.
  (if
    (and
      pt
      (numberp (car pt))
      (numberp (cadr pt))
    )
    (progn
      (setq pt-xy (list (car pt) (cadr pt) 0.0))

      (setq res
        (vl-catch-all-apply
          'vlax-curve-getClosestPointToProjection
          (list crvObj pt-xy '(0.0 0.0 1.0))
        )
      )

      (if (or (vl-catch-all-error-p res) (not res))
        (progn
          ;; Fallback dla obiektow / wersji, gdzie projekcja nie zadziala.
          (setq res
            (vl-catch-all-apply
              'vlax-curve-getClosestPointTo
              (list crvObj pt-xy)
            )
          )

          (if (vl-catch-all-error-p res)
            nil
            res
          )
        )

        res
      )
    )

    nil
  )
)


;; --- FUNKCJA POMOCNICZA: Bezpieczny pikietaz punktu na krzywej ---
(defun geocad-multi-safe-dist-at-point (crvObj pt / res param-res)
  (setq res
    (vl-catch-all-apply
      'vlax-curve-getDistAtPoint
      (list crvObj pt)
    )
  )

  (if (and (not (vl-catch-all-error-p res)) (numberp res))
    res

    (progn
      ;; Fallback przez parametr krzywej.
      (setq param-res
        (vl-catch-all-apply
          'vlax-curve-getParamAtPoint
          (list crvObj pt)
        )
      )

      (if (and (not (vl-catch-all-error-p param-res)) (numberp param-res))
        (progn
          (setq res
            (vl-catch-all-apply
              'vlax-curve-getDistAtParam
              (list crvObj param-res)
            )
          )

          (if (and (not (vl-catch-all-error-p res)) (numberp res))
            res
            nil
          )
        )

        nil
      )
    )
  )
)


;; --- FUNKCJA POMOCNICZA: Zamiana obiektu punktowego na wezel MULTI ---
(defun geocad-multi-node-from-point
  (
    crvObj pt
    /
    p-proj L-base gap
    param-res deriv-res
    deriv len
    Ux Uy vx vy dot L-virt
  )

  ;; Logika zgodna z dotychczasowym MULTI:
  ;; - najblizszy punkt na osi,
  ;; - pikietaz L-base,
  ;; - korekta L-virt po stycznej, jezeli punkt minimalnie nie lezy na osi.
  ;;
  ;; Roznica: wszystkie wywolania krzywej sa zabezpieczone, bo automat
  ;; skanuje rysunek i musi ignorowac obiekty nietypowe zamiast wywalac komende.
  (setq p-proj (geocad-multi-safe-closest-point crvObj pt))

  (if p-proj
    (progn
      (setq L-base (geocad-multi-safe-dist-at-point crvObj p-proj))
      (setq gap (geocad-multi-distance-xy pt p-proj))

      (if (and (numberp L-base) (numberp gap))
        (progn
          (setq L-virt L-base)

          (if (> gap 0.001)
            (progn
              (setq param-res
                (vl-catch-all-apply
                  'vlax-curve-getParamAtPoint
                  (list crvObj p-proj)
                )
              )

              (if (and (not (vl-catch-all-error-p param-res)) (numberp param-res))
                (progn
                  (setq deriv-res
                    (vl-catch-all-apply
                      'vlax-curve-getFirstDeriv
                      (list crvObj param-res)
                    )
                  )

                  (if
                    (and
                      (not (vl-catch-all-error-p deriv-res))
                      deriv-res
                      (numberp (car deriv-res))
                      (numberp (cadr deriv-res))
                    )
                    (progn
                      (setq deriv deriv-res)

                      (setq len
                        (distance
                          '(0.0 0.0)
                          (list (car deriv) (cadr deriv))
                        )
                      )

                      (if (> len 0.0000001)
                        (progn
                          (setq Ux (/ (car deriv) len))
                          (setq Uy (/ (cadr deriv) len))

                          (setq vx (- (car pt) (car p-proj)))
                          (setq vy (- (cadr pt) (cadr p-proj)))

                          (setq dot (+ (* vx Ux) (* vy Uy)))
                          (setq L-virt (+ L-base dot))
                        )
                      )
                    )
                  )
                )
              )
            )
          )

          ;; Zwracamy:
          ;; (pikietaz rzedna_Z odleglosc_XY_od_osi)
          (if (numberp L-virt)
            (list L-virt (caddr pt) gap)
            nil
          )
        )

        nil
      )
    )

    nil
  )
)


;; --- FUNKCJA POMOCNICZA: Bezpieczne pobranie punktu z obiektu ---
(defun geocad-multi-safe-get-pt-from-obj (obj / res)
  (setq res
    (vl-catch-all-apply
      'get-pt-from-obj
      (list obj)
    )
  )

  (if (vl-catch-all-error-p res)
    nil
    res
  )
)


;; --- FUNKCJA POMOCNICZA: Budowanie wezlow z selection set ---
(defun geocad-multi-build-valid-nodes-from-ss
  (
    ss crvObj tolerance point-layer
    /
    valid-nodes omitted
    i en obj pt node gap
  )

  ;; tolerance = nil    -> tryb reczny, bez filtrowania po odleglosci od osi.
  ;; tolerance = liczba -> tryb automat, tylko punkty w zadanej odleglosci XY od osi.
  ;;
  ;; W obu trybach przyjmujemy tylko standardowe zrodla GeoprofiCAD:
  ;; - INSERT bloku Pikieta_Geo na warstwie <Prefix>_PIKIETY,
  ;; - POINT na warstwie <Prefix>_PIKIETY.
  (setq valid-nodes '())
  (setq omitted 0)
  (setq i 0)

  (if ss
    (while (< i (sslength ss))
      (setq en (ssname ss i))

      (setq obj
        (vl-catch-all-apply
          'vlax-ename->vla-object
          (list en)
        )
      )

      (if
        (or
          (vl-catch-all-error-p obj)
          (not obj)
          (not (geocad-multi-supported-base-object-p obj point-layer))
        )
        (setq omitted (1+ omitted))

        (progn
          (setq pt (geocad-multi-safe-get-pt-from-obj obj))

          ;; Punkt musi istniec i miec liczbowe Z rozne od 0.
          ;; To zachowuje dotychczasowa logike MULTI.
          (if
            (and
              pt
              (numberp (car pt))
              (numberp (cadr pt))
              (numberp (caddr pt))
              (/= (caddr pt) 0.0)
            )
            (progn
              (setq node (geocad-multi-node-from-point crvObj pt))

              (if node
                (progn
                  (setq gap (caddr node))

                  (if
                    (or
                      (not tolerance)
                      (<= gap tolerance)
                    )
                    (setq valid-nodes
                      (cons
                        (list
                          (car node)
                          (cadr node)
                        )
                        valid-nodes
                      )
                    )

                    (setq omitted (1+ omitted))
                  )
                )

                (setq omitted (1+ omitted))
              )
            )

            (setq omitted (1+ omitted))
          )
        )
      )

      (setq i (1+ i))
    )
  )

  (list valid-nodes omitted)
)


;; --- FUNKCJA POMOCNICZA: Automatyczne skanowanie calego rysunku ---
(defun geocad-multi-scan-auto-nodes (crvObj tolerance point-layer / ss)
  ;; Skanujemy tylko potencjalne zrodla GeoprofiCAD.
  ;; Dokladny filtr nazwy bloku i warstwy jest robiony pozniej,
  ;; bo ssget nie obsluzy wygodnie warunku:
  ;; (INSERT Pikieta_Geo) albo (POINT na <Prefix>_PIKIETY).
  (setq ss (ssget "X" '((0 . "INSERT,POINT"))))

  (geocad-multi-build-valid-nodes-from-ss ss crvObj tolerance point-layer)
)


(defun c:NIWELACJA_MULTI
  (
    /
    old-err old-osmode old-cmdecho old-clayer
    crvEnt crvObj ss
    valid-nodes auto-valid-nodes
    auto-result manual-result
    auto-omitted omitted
    auto-tolerance tol-input point-mode point-layer
    node1 node2 L1 Z1 L2 Z2 dL slope
    mode step num-pts segment-step L-cur segment-count effective-step k
    doc space pt-cur z-cur zlicz draw-3d poly-pts
    poly-layer
    batch
    i
  )

  ;; --- OBSLUGA BLEDOW ---
  (setq old-osmode (getvar "OSMODE"))
  (setq old-cmdecho (getvar "CMDECHO"))
  (setq old-clayer (getvar "CLAYER"))

  (setq old-err *error*
        *error*
        (lambda (msg)
          ;; Jezeli przerwano po wstawieniu czesci pikiet,
          ;; zapisujemy finalny licznik z batcha.
          (if batch
            (progn
              (setq batch (geocad-pikieta-batch-end batch))
              (setq batch nil)
            )
          )

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

          (if (not (member msg '("Function cancelled" "quit / exit abort")))
            (princ (strcat "\nPrzerwano: " msg))
            (princ "\nPrzerwano.")
          )

          (princ)
        )
  )

  ;; --- 1. WYBOR OSI ---
  (setq crvEnt (car (entsel "\n1. Wybierz os trasy (Linia/Polilinia/Luk): ")))

  (if (not crvEnt)
    (exit)
  )

  (setq crvObj
    (vl-catch-all-apply
      'vlax-ename->vla-object
      (list crvEnt)
    )
  )

  (if
    (or
      (vl-catch-all-error-p crvObj)
      (not crvObj)
      (not (geocad-multi-valid-curve-p crvObj))
    )
    (progn
      (alert "Wybrany obiekt nie jest poprawna osia trasy. Wybierz linie, polilinie albo luk.")
      (exit)
    )
  )


  ;; --- 2. WYBOR / AUTOMATYCZNE WYKRYWANIE PUNKTOW BAZOWYCH ---
  (setq auto-tolerance 0.05)
  (setq point-layer (geocad-multi-get-current-point-layer))

  (princ
    (strcat
    "\n2. Skanowanie punktow bazowych przy osi..."
    "\nZrodla automatu: Pikieta_Geo oraz POINT na warstwach *_PIKIETY."
    )
  )

  (setq auto-result
    (geocad-multi-scan-auto-nodes crvObj auto-tolerance point-layer)
  )

  (setq auto-valid-nodes (car auto-result))
  (setq auto-omitted (cadr auto-result))

  (princ
    (strcat
      "\nAutomat: wykryto "
      (itoa (length auto-valid-nodes))
      " punktow bazowych na osi. Tolerancja: "
      (rtos auto-tolerance 2 3)
      " m."
    )
  )

  (setq point-mode nil)

  (while (not point-mode)
    (initget "Automat Recznie Tolerancja")

    (setq point-mode
      (getkword
        "\nWybierz tryb punktow bazowych [Automat/Recznie/Tolerancja] <Automat>: "
      )
    )

    (if (not point-mode)
      (setq point-mode "Automat")
    )

    (cond
      ((= point-mode "Tolerancja")
        (setq tol-input
          (getreal
            (strcat
              "\nPodaj tolerancje szukania punktow na osi <"
              (rtos auto-tolerance 2 3)
              ">: "
            )
          )
        )

        (if tol-input
          (if (> tol-input 0.0)
            (setq auto-tolerance tol-input)

            (alert "Tolerancja musi byc wieksza od 0.")
          )
        )

        (setq auto-result
          (geocad-multi-scan-auto-nodes crvObj auto-tolerance point-layer)
        )

        (setq auto-valid-nodes (car auto-result))
        (setq auto-omitted (cadr auto-result))

        (princ
          (strcat
            "\nAutomat: wykryto "
            (itoa (length auto-valid-nodes))
            " punktow bazowych na osi. Tolerancja: "
            (rtos auto-tolerance 2 3)
            " m."
          )
        )

        ;; Wracamy do menu.
        (setq point-mode nil)
      )

      ((= point-mode "Automat")
        (if (< (length auto-valid-nodes) 2)
          (progn
            (alert
              "Automat znalazl mniej niz 2 poprawne punkty bazowe. Zmien tolerancje albo wybierz punkty recznie."
            )
            (setq point-mode nil)
          )

          (progn
            (setq valid-nodes auto-valid-nodes)
            (setq omitted auto-omitted)
          )
        )
      )

      ((= point-mode "Recznie")
        (princ
  (strcat
    "\nZaznacz oknem pikiety bazowe."
    "\nPrzyjmowane beda tylko: Pikieta_Geo oraz POINT na warstwach *_PIKIETY."
  )
)

        (setq ss (ssget '((0 . "INSERT,POINT"))))

        (if (not ss)
          (progn
            (alert "Nic nie wybrano!")
            (setq point-mode nil)
          )

          (progn
            ;; Tryb reczny bez tolerancji, czyli nie odrzuca punktow po odleglosci
            ;; od osi, ale nadal przyjmuje tylko standardowe zrodla GeoprofiCAD.
            (setq manual-result
              (geocad-multi-build-valid-nodes-from-ss ss crvObj nil point-layer)
            )

            (setq valid-nodes (car manual-result))
            (setq omitted (cadr manual-result))

            (if (< (length valid-nodes) 2)
              (progn
                (alert
                  "Za malo poprawnych punktow! Zaznacz przynajmniej 2 pikiety GeoprofiCAD z rzedna Z."
                )
                (setq point-mode nil)
              )
            )
          )
        )
      )
    )
  )


  ;; --- 3. WERYFIKACJA I SORTOWANIE WEZLOW ---
  (if (< (length valid-nodes) 2)
    (progn
      (alert "Za malo poprawnych punktow! Wymagane sa przynajmniej 2 pikiety z rzedna Z.")
      (exit)
    )
  )

  ;; Sortowanie po pikietazu L-virt od najmniejszego do najwiekszego.
  (setq valid-nodes
    (vl-sort
      valid-nodes
      (function
        (lambda (a b)
          (< (car a) (car b))
        )
      )
    )
  )

  (princ
    (strcat
      "\n>>> Znaleziono "
      (itoa (length valid-nodes))
      " wezlow wzdluz trasy (Pominieto: "
      (itoa omitted)
      ")."
    )
  )


  ;; --- 4. METODA I OPCJE ---
  ;; Rowna     - nowa logika: rowne odstepy miedzy wierzcholkami/wezlami.
  ;; Odleglosc - stara logika: sztywny krok co podana odleglosc.
  ;; Podzial   - stara logika: podana liczba odcinkow miedzy kazda para wezlow.
  (initget 1 "Rowna Odleglosc Podzial")

  (setq mode
    (getkword
      "\n3. Metoda generowania miedzy wezlami [Rowna/Odleglosc/Podzial]: "
    )
  )

  (cond
    ((member mode '("Rowna" "Odleglosc"))
      (setq step (getreal "\nPodaj odstep [m]: "))

      (if (or (not step) (<= step 0.0))
        (progn
          (alert "Odstep musi byc wiekszy od 0.")
          (exit)
        )
      )
    )

    ((= mode "Podzial")
      (setq num-pts (getint "\nPodaj ilosc odcinkow miedzy KAZDA para wezlow: "))

      (if (or (not num-pts) (< num-pts 1))
        (progn
          (alert "Ilosc odcinkow musi byc wieksza lub rowna 1.")
          (exit)
        )
      )
    )
  )


  (initget "Tak Nie")
  (setq draw-3d
    (getkword "\n4. Czy na koniec narysowac ciagla Polilinie 3D? [Tak/Nie] <Tak>: ")
  )

  (if (not draw-3d)
    (setq draw-3d "Tak")
  )


  ;; --- 5. GENEROWANIE W PETLI DLA KAZDEGO SEGMENTU ---
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq space (vla-get-ModelSpace doc))

  ;; Sesja masowego wstawiania pikiet.
  ;; Context, warstwy i konfiguracja sa przygotowane raz.
  ;; Numer automatyczny jest pobierany leniwie przy pierwszym insercie.
  (setq batch (geocad-pikieta-batch-start doc))

  (setq zlicz 0)
  (setq poly-pts '())


  ;; Zabezpieczenie pierwszej krawedzi do 3D.
  (setq L1 (car (nth 0 valid-nodes)))
  (setq Z1 (cadr (nth 0 valid-nodes)))
  (setq pt-cur (get-safe-curve-pt crvObj L1))

  (if pt-cur
    (setq poly-pts
      (append
        poly-pts
        (list
          (list
            (car pt-cur)
            (cadr pt-cur)
            Z1
          )
        )
      )
    )
  )


  (setq i 0)

  (while (< i (1- (length valid-nodes)))
    (setq node1 (nth i valid-nodes))
    (setq node2 (nth (1+ i) valid-nodes))

    (setq L1 (car node1))
    (setq Z1 (cadr node1))

    (setq L2 (car node2))
    (setq Z2 (cadr node2))

    (setq dL (- L2 L1))

    ;; Wykonuj tylko, jesli punkty nie sa w tym samym miejscu.
    (if (> dL 0.001)
      (progn
        (setq slope (/ (- Z2 Z1) dL))
        (setq L-cur L1)

        (cond

          ;; ======================================================
          ;; NOWY TRYB: Rowna
          ;;
          ;; Podany odstep traktujemy jako odstep maksymalny/przyblizony.
          ;; Segment L1-L2 jest dzielony na rowne czesci.
          ;;
          ;; Przyklad:
          ;; dL = 21.20, step = 5.00
          ;; segment-count = ceil(21.20 / 5.00) = 5
          ;; effective-step = 21.20 / 5 = 4.24
          ;;
          ;; Generowane sa tylko punkty posrednie:
          ;; L1 + 4.24
          ;; L1 + 8.48
          ;; L1 + 12.72
          ;; L1 + 16.96
          ;;
          ;; L1 i L2 sa wezlami bazowymi, wiec nie sa wstawiane jako nowe pikiety.
          ;; ======================================================
          ((= mode "Rowna")
            (setq segment-count
              (geocad-multi-ceil-positive (/ dL step))
            )

            (if (< segment-count 1)
              (setq segment-count 1)
            )

            (setq effective-step (/ dL (float segment-count)))

            (setq k 1)

            (while (< k segment-count)
              (setq L-cur (+ L1 (* k effective-step)))
              (setq z-cur (+ Z1 (* (- L-cur L1) slope)))
              (setq pt-cur (get-safe-curve-pt crvObj L-cur))

              (if pt-cur
                (progn
                  (setq batch
                    (geocad-pikieta-batch-insert
                      batch
                      space
                      (list
                        (car pt-cur)
                        (cadr pt-cur)
                        z-cur
                      )
                      nil
                      T
                    )
                  )

                  (setq zlicz (1+ zlicz))

                  (setq poly-pts
                    (append
                      poly-pts
                      (list
                        (list
                          (car pt-cur)
                          (cadr pt-cur)
                          z-cur
                        )
                      )
                    )
                  )
                )
              )

              (setq k (1+ k))
            )
          )


          ;; ======================================================
          ;; STARY TRYB: Odleglosc
          ;;
          ;; Bez zmian.
          ;; Idzie sztywnym krokiem co "step".
          ;; Moze zostawic krotka koncowke przy wezle.
          ;; ======================================================
          ((= mode "Odleglosc")
            (setq L-cur (+ L-cur step))

            (while (< L-cur (- L2 0.01))
              (setq z-cur (+ Z1 (* (- L-cur L1) slope)))
              (setq pt-cur (get-safe-curve-pt crvObj L-cur))

              (if pt-cur
                (progn
                  (setq batch
                    (geocad-pikieta-batch-insert
                      batch
                      space
                      (list
                        (car pt-cur)
                        (cadr pt-cur)
                        z-cur
                      )
                      nil
                      T
                    )
                  )

                  (setq zlicz (1+ zlicz))

                  (setq poly-pts
                    (append
                      poly-pts
                      (list
                        (list
                          (car pt-cur)
                          (cadr pt-cur)
                          z-cur
                        )
                      )
                    )
                  )
                )
              )

              (setq L-cur (+ L-cur step))
            )
          )


          ;; ======================================================
          ;; STARY TRYB: Podzial
          ;;
          ;; Bez zmian.
          ;; Dzieli kazdy segment na podana liczbe odcinkow.
          ;; ======================================================
          ((= mode "Podzial")
            (setq segment-step (/ dL (float num-pts)))
            (setq L-cur (+ L-cur segment-step))

            (repeat (1- num-pts)
              (if (< L-cur (- L2 0.01))
                (progn
                  (setq z-cur (+ Z1 (* (- L-cur L1) slope)))
                  (setq pt-cur (get-safe-curve-pt crvObj L-cur))

                  (if pt-cur
                    (progn
                      (setq batch
                        (geocad-pikieta-batch-insert
                          batch
                          space
                          (list
                            (car pt-cur)
                            (cadr pt-cur)
                            z-cur
                          )
                          nil
                          T
                        )
                      )

                      (setq zlicz (1+ zlicz))

                      (setq poly-pts
                        (append
                          poly-pts
                          (list
                            (list
                              (car pt-cur)
                              (cadr pt-cur)
                              z-cur
                            )
                          )
                        )
                      )
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
      (setq poly-pts
        (append
          poly-pts
          (list
            (list
              (car pt-cur)
              (cadr pt-cur)
              Z2
            )
          )
        )
      )
    )

    (setq i (1+ i))
  )


  ;; Zapis finalnego licznika po wygenerowaniu pikiet.
  ;; Robimy to przed 3DPOLY, zeby ewentualne przerwanie rysowania polilinii
  ;; nie cofalo numeracji pikiet.
  (if batch
    (progn
      (setq batch (geocad-pikieta-batch-end batch))
      (setq batch nil)
    )
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

  (princ
    (strcat
      "\nSukces! Wygenerowano "
      (itoa zlicz)
      " pikiet we wszystkich segmentach."
    )
  )

  (princ)
)


(princ "\nKomenda: NIWELACJA_MULTI wczytana.")
(princ)