(vl-load-com)
(load "gp_Core.lsp" "\nBLAD: Nie znaleziono pliku gp_Core.lsp!")

;; ======================================================
;; GEOPROFICAD - SIATKA PUNKTOW W ZAMKNIETYM OBRYSIE
;; Komenda: SIATKA_PUNKTOW
;;
;; Funkcja:
;; - wybiera zamkniety obrys,
;; - tryb Standard:
;;   - opcjonalnie dodaje punkty w naroznikach/zalamaniach obrysu,
;;   - generuje pikiety na obrysie co zadany interwal,
;;   - generuje pikiety wewnatrz obrysu,
;; - tryb Budynek:
;;   - tworzy drugi obrys odsuniety do wewnatrz,
;;   - opcjonalnie dodaje punkty w naroznikach/zalamaniach obrysu zewnetrznego i wewnetrznego,
;;   - generuje pikiety na obrysie zewnetrznym i wewnetrznym,
;;   - generuje siatke tylko wewnatrz obrysu wewnetrznego,
;; - pikiety dostaja zadana wysokosc Z,
;; - test wnetrza robiony jest na wysokosci obrysu.
;; ======================================================


(defun geocad-grid-ceil (x / i)
  (setq i (fix x))
  (if (and (> x 0.0) (/= x (float i)))
    (1+ i)
    i
  )
)


(defun geocad-grid-first-at-or-after (minv basev step)
  (+ basev (* (geocad-grid-ceil (/ (- minv basev) step)) step))
)


(defun geocad-grid-key (pt / x y)
  ;; Klucz XY z dokladnoscia do 1 mm.
  ;; Bez zamiany na ogromne liczby calkowite.
  (setq x (rtos (car pt) 2 3))
  (setq y (rtos (cadr pt) 2 3))
  (strcat x "|" y)
)


(defun geocad-grid-get-bbox (obj / minp maxp minl maxl)
  (vla-GetBoundingBox obj 'minp 'maxp)
  (setq minl (vlax-safearray->list minp))
  (setq maxl (vlax-safearray->list maxp))
  (list minl maxl)
)


(defun geocad-grid-curve-length (crv)
  (vlax-curve-getDistAtParam crv (vlax-curve-getEndParam crv))
)


(defun geocad-grid-safe-curve-length (crv / res)
  (setq res
    (vl-catch-all-apply
      'geocad-grid-curve-length
      (list crv)
    )
  )

  (if (and (not (vl-catch-all-error-p res)) (numberp res) (> res 0.001))
    res
    nil
  )
)


(defun geocad-grid-closed-p (obj / typ res sp ep)
  (setq typ (vla-get-ObjectName obj))

  (cond
    ((member typ '("AcDbCircle" "AcDbEllipse"))
      T
    )

    ((vlax-property-available-p obj 'Closed)
      (setq res (vl-catch-all-apply 'vla-get-Closed (list obj)))
      (and
        (not (vl-catch-all-error-p res))
        (= res :vlax-true)
      )
    )

    (T
      (setq sp (vl-catch-all-apply 'vlax-curve-getStartPoint (list obj)))
      (setq ep (vl-catch-all-apply 'vlax-curve-getEndPoint (list obj)))
      (if
        (and
          (not (vl-catch-all-error-p sp))
          (not (vl-catch-all-error-p ep))
        )
        (equal sp ep 0.001)
        nil
      )
    )
  )
)


(defun geocad-grid-get-breakpoints (obj / typ sp ep i p verts)
  ;; Pobiera wierzcholki / zalamania polilinii.
  ;;
  ;; Dziala dla:
  ;; - AcDbPolyline,
  ;; - AcDb2dPolyline,
  ;; - AcDb3dPolyline.
  ;;
  ;; Dla okregu/elipsy zwraca pusta liste, bo nie ma naroznikow.
  (setq typ (vla-get-ObjectName obj))
  (setq verts '())

  (if (member typ '("AcDbPolyline" "AcDb2dPolyline" "AcDb3dPolyline"))
    (progn
      (setq sp (vlax-curve-getStartParam obj))
      (setq ep (vlax-curve-getEndParam obj))

      ;; W poliliniach wierzcholki sa zwykle na parametrach calkowitych.
      ;; Dla polilinii zamknietej ostatni parametr moze byc powtorka pierwszego
      ;; punktu - duplikaty i tak usuwa geocad-grid-insert-unique.
      (setq i (fix sp))

      (while (<= i (+ ep 0.0000001))
        (setq p (vl-catch-all-apply 'vlax-curve-getPointAtParam (list obj i)))

        (if (and p (not (vl-catch-all-error-p p)))
          (setq verts (cons p verts))
        )

        (setq i (1+ i))
      )
    )
  )

  (reverse verts)
)


(defun geocad-grid-list-from-variant (var / res)
  ;; Bezpiecznie zamienia wynik IntersectWith na liste liczb.
  (setq res
    (vl-catch-all-apply
      '(lambda ()
         (vlax-safearray->list (vlax-variant-value var))
       )
    )
  )

  (if (vl-catch-all-error-p res)
    nil
    res
  )
)


(defun geocad-grid-count-unique-intersections (lst / keys cnt x y k)
  ;; IntersectWith zwraca plaska liste:
  ;; x1 y1 z1 x2 y2 z2 ...
  ;;
  ;; Liczymy unikalne punkty przeciecia, bo przy wierzcholkach
  ;; AutoCAD moze zwrocic bardzo bliskie lub zdublowane trafienia.
  (setq keys '())
  (setq cnt 0)

  (while (and lst (cadr lst))
    (setq x (car lst))
    (setq y (cadr lst))
    (setq k (strcat (rtos x 2 3) "|" (rtos y 2 3)))

    (if (not (member k keys))
      (progn
        (setq keys (cons k keys))
        (setq cnt (1+ cnt))
      )
    )

    ;; przejscie o x y z
    (if (cdddr lst)
      (setq lst (cdddr lst))
      (setq lst nil)
    )
  )

  cnt
)


(defun geocad-grid-intersection-count (obj space pt farpt / line res lst cnt)
  ;; Tworzy tymczasowa linie od badanego punktu do punktu daleko poza obrysem,
  ;; liczy przeciecia z obrysem i od razu usuwa linie.
  ;;
  ;; Zasada:
  ;; - liczba przeciec nieparzysta = punkt wewnatrz,
  ;; - liczba przeciec parzysta = punkt na zewnatrz.
  (setq line
    (vla-AddLine
      space
      (vlax-3d-point pt)
      (vlax-3d-point farpt)
    )
  )

  ;; Ukrycie linii pomocniczej, jezeli dana wersja AutoCAD to wspiera.
  (vl-catch-all-apply 'vla-put-Visible (list line :vlax-false))

  (setq res
    (vl-catch-all-apply
      'vla-IntersectWith
      (list line obj 0)
    )
  )

  (vl-catch-all-apply 'vla-Delete (list line))

  (if (vl-catch-all-error-p res)
    0
    (progn
      (setq lst (geocad-grid-list-from-variant res))
      (if lst
        (geocad-grid-count-unique-intersections lst)
        0
      )
    )
  )
)


(defun geocad-grid-inside-p (obj space pt minx miny maxx maxy test-z / w h test-pt farpt cnt)
  ;; Test czy punkt jest wewnatrz obrysu.
  ;;
  ;; pt ma Z pikiety, np. 134.78.
  ;; Obrys zwykle lezy na Z=0.
  ;; Dlatego przeciecia sprawdzamy na wysokosci obrysu: test-z.

  (setq w (- maxx minx))
  (setq h (- maxy miny))

  (if (< w 1.0) (setq w 1.0))
  (if (< h 1.0) (setq h 1.0))

  ;; Punkt testowy ma XY z siatki, ale Z z obrysu.
  (setq test-pt
    (list
      (car pt)
      (cadr pt)
      test-z
    )
  )

  ;; Punkt daleko poza obrysem, tez na Z obrysu.
  ;; Celowo po skosie, zeby ograniczyc trafienie idealnie w wierzcholek.
  (setq farpt
    (list
      (+ maxx (* 3.0 w) 123.456)
      (+ maxy (* 1.7 h) 78.912)
      test-z
    )
  )

  (setq cnt (geocad-grid-intersection-count obj space test-pt farpt))

  (= (rem cnt 2) 1)
)


(defun geocad-grid-insert-unique (space pt zval seen batch cnt / p key)
  ;; Wstawia pikiete tylko raz dla danego XY.
  ;; Numeracja jest obslugiwana przez geocad-pikieta-batch-insert.
  (setq p (list (car pt) (cadr pt) zval))
  (setq key (geocad-grid-key p))

  (if (not (member key seen))
    (progn
      (setq seen (cons key seen))

      (setq batch
        (geocad-pikieta-batch-insert
          batch
          space
          p
          nil
          T
        )
      )

      (setq cnt (1+ cnt))
    )
  )

  (list seen batch cnt)
)


(defun geocad-grid-get-test-z (obj / start-pt)
  ;; Z obiektu, na ktorej testujemy przeciecia.
  ;; Dla typowego obrysu 2D bedzie to 0.0.
  (setq start-pt
    (vl-catch-all-apply
      'vlax-curve-getStartPoint
      (list obj)
    )
  )

  (if
    (and
      start-pt
      (not (vl-catch-all-error-p start-pt))
      (caddr start-pt)
    )
    (caddr start-pt)
    0.0
  )
)


(defun geocad-grid-offset-result-to-list (res / lst)
  ;; vla-Offset zwykle zwraca VARIANT z tablica obiektow.
  ;; Zabezpieczamy tez przypadek pojedynczego obiektu.
  (cond
    ((vl-catch-all-error-p res)
      nil
    )

    ((vlax-objectp res)
      (list res)
    )

    (T
      (setq lst
        (vl-catch-all-apply
          '(lambda ()
             (vlax-safearray->list (vlax-variant-value res))
           )
        )
      )

      (if (vl-catch-all-error-p lst)
        nil
        lst
      )
    )
  )
)


(defun geocad-grid-make-offset-objects (obj dist / res)
  (setq res
    (vl-catch-all-apply
      'vla-Offset
      (list obj dist)
    )
  )

  (geocad-grid-offset-result-to-list res)
)


(defun geocad-grid-delete-objects (objs)
  (foreach o objs
    (if (vlax-objectp o)
      (vl-catch-all-apply 'vla-Delete (list o))
    )
  )
)


(defun geocad-grid-delete-objects-except (objs keep)
  (foreach o objs
    (if
      (and
        (vlax-objectp o)
        (not (eq o keep))
      )
      (vl-catch-all-apply 'vla-Delete (list o))
    )
  )
)


(defun geocad-grid-valid-inner-offset-p
  (
    outer-obj candidate-obj space
    outer-minx outer-miny outer-maxx outer-maxy outer-test-z
    /
    len
    p1 p2 p3
  )

  ;; Kandydat offsetu musi:
  ;; - byc zamkniety,
  ;; - miec dodatnia dlugosc,
  ;; - miec kilka punktow probnych wewnatrz obrysu zewnetrznego.
  (if
    (and
      candidate-obj
      (vlax-objectp candidate-obj)
      (geocad-grid-closed-p candidate-obj)
      (setq len (geocad-grid-safe-curve-length candidate-obj))
    )
    (progn
      (setq p1
        (vl-catch-all-apply
          'vlax-curve-getPointAtDist
          (list candidate-obj (* len 0.15))
        )
      )

      (setq p2
        (vl-catch-all-apply
          'vlax-curve-getPointAtDist
          (list candidate-obj (* len 0.50))
        )
      )

      (setq p3
        (vl-catch-all-apply
          'vlax-curve-getPointAtDist
          (list candidate-obj (* len 0.85))
        )
      )

      (and
        p1
        p2
        p3
        (not (vl-catch-all-error-p p1))
        (not (vl-catch-all-error-p p2))
        (not (vl-catch-all-error-p p3))
        (geocad-grid-inside-p outer-obj space p1 outer-minx outer-miny outer-maxx outer-maxy outer-test-z)
        (geocad-grid-inside-p outer-obj space p2 outer-minx outer-miny outer-maxx outer-maxy outer-test-z)
        (geocad-grid-inside-p outer-obj space p3 outer-minx outer-miny outer-maxx outer-maxy outer-test-z)
      )
    )

    nil
  )
)


(defun geocad-grid-find-inner-offset
  (
    outer-obj objs space
    outer-minx outer-miny outer-maxx outer-maxy outer-test-z
    /
    found
  )

  (setq found nil)

  (foreach o objs
    (if
      (and
        (not found)
        (geocad-grid-valid-inner-offset-p
          outer-obj
          o
          space
          outer-minx
          outer-miny
          outer-maxx
          outer-maxy
          outer-test-z
        )
      )
      (setq found o)
    )
  )

  found
)


(defun geocad-grid-create-inner-offset
  (
    outer-obj space offset-distance
    outer-minx outer-miny outer-maxx outer-maxy outer-test-z
    /
    objs inner
  )

  ;; Kierunek offsetu zalezy od kierunku polilinii.
  ;; Dlatego probujemy +d, sprawdzamy czy jest wewnatrz,
  ;; a jezeli nie, probujemy -d.
  (setq objs (geocad-grid-make-offset-objects outer-obj offset-distance))

  (setq inner
    (geocad-grid-find-inner-offset
      outer-obj
      objs
      space
      outer-minx
      outer-miny
      outer-maxx
      outer-maxy
      outer-test-z
    )
  )

  (if inner
    (progn
      (geocad-grid-delete-objects-except objs inner)
      inner
    )

    (progn
      (geocad-grid-delete-objects objs)

      (setq objs (geocad-grid-make-offset-objects outer-obj (- offset-distance)))

      (setq inner
        (geocad-grid-find-inner-offset
          outer-obj
          objs
          space
          outer-minx
          outer-miny
          outer-maxx
          outer-maxy
          outer-test-z
        )
      )

      (if inner
        (progn
          (geocad-grid-delete-objects-except objs inner)
          inner
        )

        (progn
          (geocad-grid-delete-objects objs)
          nil
        )
      )
    )
  )
)


(defun geocad-grid-insert-breakpoints
  (
    obj space zval seen batch cnt
    /
    breakpoints bp res old-cnt added
  )

  (setq added 0)
  (setq breakpoints (geocad-grid-get-breakpoints obj))

  (foreach bp breakpoints
    (setq old-cnt cnt)

    (setq res
      (geocad-grid-insert-unique
        space
        bp
        zval
        seen
        batch
        cnt
      )
    )

    (setq seen (car res))
    (setq batch (cadr res))
    (setq cnt (caddr res))

    (if (> cnt old-cnt)
      (setq added (1+ added))
    )
  )

  (list seen batch cnt added)
)


(defun geocad-grid-insert-border-points
  (
    obj space zval border-step seen batch cnt
    /
    len d p res old-cnt added
  )

  (setq added 0)
  (setq len (geocad-grid-safe-curve-length obj))

  (if len
    (progn
      (setq d 0.0)

      (while (< d len)
        (setq p
          (vl-catch-all-apply
            'vlax-curve-getPointAtDist
            (list obj d)
          )
        )

        (if
          (and
            p
            (not (vl-catch-all-error-p p))
          )
          (progn
            (setq old-cnt cnt)

            (setq res
              (geocad-grid-insert-unique
                space
                p
                zval
                seen
                batch
                cnt
              )
            )

            (setq seen (car res))
            (setq batch (cadr res))
            (setq cnt (caddr res))

            (if (> cnt old-cnt)
              (setq added (1+ added))
            )
          )
        )

        (setq d (+ d border-step))
      )
    )
  )

  (list seen batch cnt added)
)


(defun c:SIATKA_PUNKTOW
  (
    /
    olderr
    acad doc space
    undo-started keep-inner-offset
    ent obj grid-obj inner-obj
    grid-mode offset-distance
    len len-res inner-len
    bbox minp maxp minx miny maxx maxy
    outer-bbox outer-minp outer-maxp outer-minx outer-miny outer-maxx outer-maxy outer-test-z
    test-z
    step-x step-y border-step
    add-breakpoints
    zval
    basept basept-wcs basex basey
    x y start-x start-y
    pt res
    seen batch cnt
    cnt-corners cnt-inner-corners
    cnt-border cnt-inner-border
    cnt-inside
    ncols nrows est answer
  )

  (setq olderr *error*)
  (setq undo-started nil)
  (setq keep-inner-offset nil)
  (setq inner-obj nil)
  (setq batch nil)
  (setq cnt 0)

  (defun *error* (msg)
    ;; Jezeli przerwano po wstawieniu czesci pikiet,
    ;; zapisujemy finalny licznik z batcha.
    (if batch
      (progn
        (setq batch (geocad-pikieta-batch-end batch))
        (setq batch nil)
      )
    )

    ;; Jezeli w trybie Budynek utworzono offset, ale komenda padla
    ;; zanim faktycznie zaczela generowac punkty, kasujemy niedokonczony offset.
    (if
      (and
        inner-obj
        (vlax-objectp inner-obj)
        (not keep-inner-offset)
        (or (not cnt) (= cnt 0))
      )
      (vl-catch-all-apply 'vla-Delete (list inner-obj))
    )

    (if (and doc undo-started)
      (progn
        (vl-catch-all-apply 'vla-EndUndoMark (list doc))
        (setq undo-started nil)
      )
    )

    (setq *error* olderr)

    (if msg
      (if (not (member msg '("Function cancelled" "quit / exit abort")))
        (princ (strcat "\nPrzerwano: " msg))
        (princ "\nPrzerwano.")
      )
    )

    (princ)
  )

  (setq acad (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acad))
  (setq space (vla-get-ModelSpace doc))

  (princ "\n==============================================")
  (princ "\nGEOPROFICAD - SIATKA PUNKTOW")
  (princ "\n==============================================")

  ;; ------------------------------------------------------
  ;; 1. Wybor obrysu
  ;; ------------------------------------------------------
  (setq ent (car (entsel "\nWybierz zamkniety obrys siatki: ")))

  (if (not ent)
    (progn
      (princ "\nNie wybrano obrysu.")
      (setq *error* olderr)
      (princ)
      (exit)
    )
  )

  (setq obj (vlax-ename->vla-object ent))

  ;; ------------------------------------------------------
  ;; 2. Sprawdzenie, czy obiekt jest krzywa
  ;; ------------------------------------------------------
  (setq len-res (vl-catch-all-apply 'geocad-grid-curve-length (list obj)))

  (if (vl-catch-all-error-p len-res)
    (progn
      (alert "Wybrany obiekt nie jest obslugiwanym obrysem. Wybierz zamknieta polilinie, okrag albo elipse. Jezeli obrys jest w bloku, najpierw rozbij blok albo wybierz sama polilinie.")
      (setq *error* olderr)
      (princ)
      (exit)
    )
    (setq len len-res)
  )

  ;; ------------------------------------------------------
  ;; 3. Sprawdzenie zamkniecia
  ;; ------------------------------------------------------
  (if (not (geocad-grid-closed-p obj))
    (progn
      (alert "Wybrany obrys nie jest zamkniety. Zamknij polilinie i uruchom komende ponownie.")
      (setq *error* olderr)
      (princ)
      (exit)
    )
  )

  ;; ------------------------------------------------------
  ;; 4. Tryb pracy
  ;; ------------------------------------------------------
  (initget "Standard Budynek")
  (setq grid-mode
    (getkword "\nTryb siatki [Standard/Budynek] <Standard>: ")
  )

  (if (not grid-mode)
    (setq grid-mode "Standard")
  )

  (if (= grid-mode "Budynek")
    (progn
      (setq offset-distance
        (getreal "\nPodaj odsuniecie wewnetrznego obrysu do srodka [m]: ")
      )

      (if (or (not offset-distance) (<= offset-distance 0.0))
        (progn
          (alert "Odsuniecie wewnetrznego obrysu musi byc wieksze od zera.")
          (setq *error* olderr)
          (princ)
          (exit)
        )
      )
    )
  )

  ;; ------------------------------------------------------
  ;; 5. Parametry siatki
  ;; ------------------------------------------------------
  (setq step-x (getreal "\nPodaj rozstaw siatki w osi X [m] <1.00>: "))
  (if (not step-x)
    (setq step-x 1.0)
  )

  (setq step-y (getreal (strcat "\nPodaj rozstaw siatki w osi Y [m] <" (rtos step-x 2 2) ">: ")))
  (if (not step-y)
    (setq step-y step-x)
  )

  (if (or (<= step-x 0.0) (<= step-y 0.0))
    (progn
      (alert "Rozstaw siatki musi byc wiekszy od zera.")
      (setq *error* olderr)
      (princ)
      (exit)
    )
  )

  (setq border-step (getreal (strcat "\nPodaj rozstaw punktow na obrysie [m] <" (rtos step-x 2 2) ">: ")))
  (if (not border-step)
    (setq border-step step-x)
  )

  (if (<= border-step 0.0)
    (progn
      (alert "Rozstaw punktow na obrysie musi byc wiekszy od zera.")
      (setq *error* olderr)
      (princ)
      (exit)
    )
  )

  ;; ------------------------------------------------------
  ;; 6. Opcja dodawania naroznikow / zalaman
  ;; ------------------------------------------------------
  (initget "Tak Nie")
  (setq add-breakpoints
    (getkword "\nDodac punkty w naroznikach/zalamaniach obrysu? [Tak/Nie] <Tak>: ")
  )

  (if (not add-breakpoints)
    (setq add-breakpoints "Tak")
  )

  ;; ------------------------------------------------------
  ;; 7. Rzedna Z pikiet
  ;; ------------------------------------------------------
  (setq zval (getreal "\nPodaj rzedna/wysokosc Z dla punktow: "))

  (if (not zval)
    (progn
      (princ "\nNie podano rzednej Z.")
      (setq *error* olderr)
      (princ)
      (exit)
    )
  )

  ;; ------------------------------------------------------
  ;; 8. Przygotowanie obrysu roboczego
  ;; ------------------------------------------------------
  (setq grid-obj obj)

  (vla-StartUndoMark doc)
  (setq undo-started T)

  (if (= grid-mode "Budynek")
    (progn
      (princ "\nTworzenie wewnetrznego obrysu...")

      (setq outer-bbox (geocad-grid-get-bbox obj))
      (setq outer-minp (car outer-bbox))
      (setq outer-maxp (cadr outer-bbox))

      (setq outer-minx (car outer-minp))
      (setq outer-miny (cadr outer-minp))
      (setq outer-maxx (car outer-maxp))
      (setq outer-maxy (cadr outer-maxp))

      (setq outer-test-z (geocad-grid-get-test-z obj))

      (setq inner-obj
        (geocad-grid-create-inner-offset
          obj
          space
          offset-distance
          outer-minx
          outer-miny
          outer-maxx
          outer-maxy
          outer-test-z
        )
      )

      (if (not inner-obj)
        (progn
          (alert "Nie udalo sie utworzyc poprawnego obrysu wewnetrznego. Sprawdz, czy odsuniecie nie jest za duze albo czy obrys nie ma problematycznej geometrii.")
          (if undo-started
            (progn
              (vl-catch-all-apply 'vla-EndUndoMark (list doc))
              (setq undo-started nil)
            )
          )
          (setq *error* olderr)
          (princ)
          (exit)
        )
      )

      (setq grid-obj inner-obj)
      (setq inner-len (geocad-grid-safe-curve-length inner-obj))

      (if (not inner-len)
        (progn
          (alert "Wewnetrzny obrys zostal utworzony, ale nie ma poprawnej dlugosci.")
          (vl-catch-all-apply 'vla-Delete (list inner-obj))
          (if undo-started
            (progn
              (vl-catch-all-apply 'vla-EndUndoMark (list doc))
              (setq undo-started nil)
            )
          )
          (setq *error* olderr)
          (princ)
          (exit)
        )
      )

      (princ "\n-> Utworzono wewnetrzny obrys.")
    )
  )

  ;; ------------------------------------------------------
  ;; 9. Bounding box obrysu roboczego
  ;; ------------------------------------------------------
  (setq bbox (geocad-grid-get-bbox grid-obj))
  (setq minp (car bbox))
  (setq maxp (cadr bbox))

  (setq minx (car minp))
  (setq miny (cadr minp))
  (setq maxx (car maxp))
  (setq maxy (cadr maxp))

  ;; ------------------------------------------------------
  ;; 10. Wysokosc Z obrysu do testu przeciecia
  ;; ------------------------------------------------------
  ;; Pikiety moga miec Z=134.78, ale obrys zwykle lezy na Z=0.
  ;; Test przeciecia musi isc na Z obrysu roboczego.
  (setq test-z (geocad-grid-get-test-z grid-obj))

  ;; ------------------------------------------------------
  ;; 11. Punkt startowy siatki
  ;; ------------------------------------------------------
  (setq basept (getpoint "\nWskaz punkt startowy siatki albo ENTER = automatycznie od lewego dolnego zakresu obrysu roboczego: "))

  (if basept
    (progn
      (setq basept-wcs (trans basept 1 0))
      (setq basex (car basept-wcs))
      (setq basey (cadr basept-wcs))
    )
    (progn
      (setq basex minx)
      (setq basey miny)
    )
  )

  (setq start-x (geocad-grid-first-at-or-after minx basex step-x))
  (setq start-y (geocad-grid-first-at-or-after miny basey step-y))

  ;; ------------------------------------------------------
  ;; 12. Ostrzezenie przy duzej liczbie kandydatow
  ;; ------------------------------------------------------
  (setq ncols (1+ (fix (/ (- maxx minx) step-x))))
  (setq nrows (1+ (fix (/ (- maxy miny) step-y))))
  (setq est (* ncols nrows))

  (if (> est 20000)
    (progn
      (initget "Tak Nie")
      (setq answer
        (getkword
          (strcat
            "\nUWAGA: siatka moze sprawdzic okolo "
            (itoa est)
            " kandydatow. Kontynuowac? [Tak/Nie] <Nie>: "
          )
        )
      )

      (if (/= answer "Tak")
        (progn
          (princ "\nAnulowano.")

          (if
            (and
              inner-obj
              (vlax-objectp inner-obj)
              (not keep-inner-offset)
            )
            (vl-catch-all-apply 'vla-Delete (list inner-obj))
          )

          (if undo-started
            (progn
              (vl-catch-all-apply 'vla-EndUndoMark (list doc))
              (setq undo-started nil)
            )
          )

          (setq *error* olderr)
          (princ)
          (exit)
        )
      )
    )
  )

  ;; ------------------------------------------------------
  ;; 13. Generowanie punktow
  ;; ------------------------------------------------------
  (setq seen '())
  (setq batch nil)
  (setq cnt 0)

  (setq cnt-corners 0)
  (setq cnt-inner-corners 0)
  (setq cnt-border 0)
  (setq cnt-inner-border 0)
  (setq cnt-inside 0)

  ;; Sesja masowego wstawiania pikiet.
  ;; Context, warstwy i konfiguracja sa przygotowane raz.
  ;; Numer automatyczny jest pobierany leniwie przy pierwszym insercie.
  (setq batch (geocad-pikieta-batch-start doc))

  ;; ------------------------------------------------------
  ;; 13A. Punkty w naroznikach / zalamaniach
  ;; ------------------------------------------------------
  (if (= add-breakpoints "Tak")
    (progn
      (princ "\nGenerowanie punktow w naroznikach/zalamaniach obrysu zewnetrznego...")

      (setq res
        (geocad-grid-insert-breakpoints
          obj
          space
          zval
          seen
          batch
          cnt
        )
      )

      (setq seen (car res))
      (setq batch (cadr res))
      (setq cnt (caddr res))
      (setq cnt-corners (cadddr res))

      (if (= grid-mode "Budynek")
        (progn
          (princ "\nGenerowanie punktow w naroznikach/zalamaniach obrysu wewnetrznego...")

          (setq res
            (geocad-grid-insert-breakpoints
              inner-obj
              space
              zval
              seen
              batch
              cnt
            )
          )

          (setq seen (car res))
          (setq batch (cadr res))
          (setq cnt (caddr res))
          (setq cnt-inner-corners (cadddr res))
        )
      )
    )
  )

  ;; ------------------------------------------------------
  ;; 13B. Punkty na obrysie zewnetrznym co zadany interwal
  ;; ------------------------------------------------------
  (princ "\nGenerowanie punktow na obrysie zewnetrznym...")

  (setq res
    (geocad-grid-insert-border-points
      obj
      space
      zval
      border-step
      seen
      batch
      cnt
    )
  )

  (setq seen (car res))
  (setq batch (cadr res))
  (setq cnt (caddr res))
  (setq cnt-border (cadddr res))

  ;; ------------------------------------------------------
  ;; 13C. Punkty na obrysie wewnetrznym co zadany interwal
  ;; ------------------------------------------------------
  (if (= grid-mode "Budynek")
    (progn
      (princ "\nGenerowanie punktow na obrysie wewnetrznym...")

      (setq res
        (geocad-grid-insert-border-points
          inner-obj
          space
          zval
          border-step
          seen
          batch
          cnt
        )
      )

      (setq seen (car res))
      (setq batch (cadr res))
      (setq cnt (caddr res))
      (setq cnt-inner-border (cadddr res))
    )
  )

  ;; ------------------------------------------------------
  ;; 13D. Punkty wewnatrz obrysu roboczego
  ;; ------------------------------------------------------
  (if (= grid-mode "Budynek")
    (princ "\nGenerowanie punktow wewnatrz obrysu wewnetrznego...")
    (princ "\nGenerowanie punktow wewnatrz obrysu...")
  )

  (setq x start-x)

  (while (<= x (+ maxx 0.0001))
    (setq y start-y)

    (while (<= y (+ maxy 0.0001))
      (setq pt (list x y zval))

      (if (geocad-grid-inside-p grid-obj space pt minx miny maxx maxy test-z)
        (progn
          (setq res (geocad-grid-insert-unique space pt zval seen batch cnt))

          (if (> (caddr res) cnt)
            (setq cnt-inside (1+ cnt-inside))
          )

          (setq seen (car res))
          (setq batch (cadr res))
          (setq cnt (caddr res))
        )
      )

      (setq y (+ y step-y))
    )

    (setq x (+ x step-x))
  )

  ;; ------------------------------------------------------
  ;; 14. Aktualizacja licznika GeoprofiCAD
  ;; ------------------------------------------------------
  ;; Batch zapisuje finalny licznik tylko wtedy, gdy faktycznie uzyto auto-numeracji.
  (if batch
    (progn
      (setq batch (geocad-pikieta-batch-end batch))
      (setq batch nil)
    )
  )

  ;; W trybie Budynek wewnetrzny obrys ma zostac w rysunku.
  (if (= grid-mode "Budynek")
    (setq keep-inner-offset T)
  )

  (if undo-started
    (progn
      (vla-EndUndoMark doc)
      (setq undo-started nil)
    )
  )

  (setq *error* olderr)

  (if (= grid-mode "Budynek")
    (princ
      (strcat
        "\nSukces. Wygenerowano "
        (itoa cnt)
        " pikiet."
        "\nTryb: Budynek"
        "\n - narozniki/zalamania zewnetrzne: "
        (itoa cnt-corners)
        "\n - narozniki/zalamania wewnetrzne: "
        (itoa cnt-inner-corners)
        "\n - na obrysie zewnetrznym: "
        (itoa cnt-border)
        "\n - na obrysie wewnetrznym: "
        (itoa cnt-inner-border)
        "\n - wewnatrz obrysu wewnetrznego: "
        (itoa cnt-inside)
        "\nZ testu obrysu roboczego: "
        (rtos test-z 2 3)
      )
    )

    (princ
      (strcat
        "\nSukces. Wygenerowano "
        (itoa cnt)
        " pikiet."
        "\nTryb: Standard"
        "\n - narozniki/zalamania: "
        (itoa cnt-corners)
        "\n - na obrysie: "
        (itoa cnt-border)
        "\n - wewnatrz: "
        (itoa cnt-inside)
        "\nZ testu obrysu: "
        (rtos test-z 2 3)
      )
    )
  )

  (princ)
)


(princ "\nKomenda wczytana: SIATKA_PUNKTOW")
(princ)