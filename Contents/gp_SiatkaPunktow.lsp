(vl-load-com)
(load "gp_Core.lsp" "\nBLAD: Nie znaleziono pliku gp_Core.lsp!")

;; ======================================================
;; GEOPROFICAD - SIATKA PUNKTOW W ZAMKNIETYM OBRYSIE
;; Komenda: SIATKA_PUNKTOW
;;
;; Funkcja:
;; - wybiera zamkniety obrys,
;; - opcjonalnie dodaje punkty w naroznikach/zalamaniach obrysu,
;; - generuje pikiety na obrysie co zadany interwal,
;; - generuje pikiety wewnatrz obrysu,
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


(defun geocad-grid-insert-unique (doc space pt zval seen next-nr cnt / p key)
  ;; Wstawia pikiete tylko raz dla danego XY.
  ;; Numeracje pobieramy raz, potem zwiekszamy lokalnie.
  (setq p (list (car pt) (cadr pt) zval))
  (setq key (geocad-grid-key p))

  (if (not (member key seen))
    (progn
      (setq seen (cons key seen))

      (if (not next-nr)
        (setq next-nr (atoi (GP:PobierzNastepnyNumer)))
      )

      (geocad-wstaw-pikiete-full doc space p (itoa next-nr) T)

      (setq next-nr (1+ next-nr))
      (setq cnt (1+ cnt))
    )
  )

  (list seen next-nr cnt)
)


(defun c:SIATKA_PUNKTOW
  (
    /
    olderr
    acad doc space
    ent obj
    len len-res
    bbox minp maxp minx miny maxx maxy
    test-z start-pt
    step-x step-y border-step
    add-breakpoints breakpoints bp
    zval
    basept basept-wcs basex basey
    x y start-x start-y
    pt res
    seen next-nr cnt cnt-corners cnt-border cnt-inside
    ncols nrows est answer
    d p
    prefix
  )

  (setq olderr *error*)

  (defun *error* (msg)
    (if doc
      (vl-catch-all-apply 'vla-EndUndoMark (list doc))
    )
    (setq *error* olderr)
    (if msg
      (princ (strcat "\nPrzerwano: " msg))
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
  ;; 4. Parametry siatki
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
  ;; 5. Opcja dodawania naroznikow / zalaman
  ;; ------------------------------------------------------
  (initget "Tak Nie")
  (setq add-breakpoints
    (getkword "\nDodac punkty w naroznikach/zalamaniach obrysu? [Tak/Nie] <Tak>: ")
  )

  (if (not add-breakpoints)
    (setq add-breakpoints "Tak")
  )

  ;; ------------------------------------------------------
  ;; 6. Rzedna Z pikiet
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
  ;; 7. Bounding box
  ;; ------------------------------------------------------
  (setq bbox (geocad-grid-get-bbox obj))
  (setq minp (car bbox))
  (setq maxp (cadr bbox))

  (setq minx (car minp))
  (setq miny (cadr minp))
  (setq maxx (car maxp))
  (setq maxy (cadr maxp))

  ;; ------------------------------------------------------
  ;; 8. Wysokosc Z obrysu do testu przeciecia
  ;; ------------------------------------------------------
  ;; Pikiety moga miec Z=134.78, ale obrys zwykle lezy na Z=0.
  ;; Test przeciecia musi isc na Z obrysu.
  (setq start-pt (vlax-curve-getStartPoint obj))

  (if (and start-pt (caddr start-pt))
    (setq test-z (caddr start-pt))
    (setq test-z 0.0)
  )

  ;; ------------------------------------------------------
  ;; 9. Punkt startowy siatki
  ;; ------------------------------------------------------
  (setq basept (getpoint "\nWskaz punkt startowy siatki albo ENTER = automatycznie od lewego dolnego zakresu obrysu: "))

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
  ;; 10. Ostrzezenie przy duzej liczbie kandydatow
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
          (setq *error* olderr)
          (princ)
          (exit)
        )
      )
    )
  )

  ;; ------------------------------------------------------
  ;; 11. Generowanie punktow
  ;; ------------------------------------------------------
  (setq seen '())
  (setq next-nr nil)
  (setq cnt 0)
  (setq cnt-corners 0)
  (setq cnt-border 0)
  (setq cnt-inside 0)

  (vla-StartUndoMark doc)

  ;; ------------------------------------------------------
  ;; 11A. Punkty w naroznikach / zalamaniach
  ;; ------------------------------------------------------
  (if (= add-breakpoints "Tak")
    (progn
      (princ "\nGenerowanie punktow w naroznikach/zalamaniach obrysu...")

      (setq breakpoints (geocad-grid-get-breakpoints obj))

      (foreach bp breakpoints
        (setq res (geocad-grid-insert-unique doc space bp zval seen next-nr cnt))

        (if (> (caddr res) cnt)
          (setq cnt-corners (1+ cnt-corners))
        )

        (setq seen (car res))
        (setq next-nr (cadr res))
        (setq cnt (caddr res))
      )
    )
  )

  ;; ------------------------------------------------------
  ;; 11B. Punkty na obrysie co zadany interwal
  ;; ------------------------------------------------------
  (princ "\nGenerowanie punktow na obrysie...")

  (setq d 0.0)

  (while (< d len)
    (setq p (vlax-curve-getPointAtDist obj d))

    (if p
      (progn
        (setq res (geocad-grid-insert-unique doc space p zval seen next-nr cnt))

        (if (> (caddr res) cnt)
          (setq cnt-border (1+ cnt-border))
        )

        (setq seen (car res))
        (setq next-nr (cadr res))
        (setq cnt (caddr res))
      )
    )

    (setq d (+ d border-step))
  )

  ;; ------------------------------------------------------
  ;; 11C. Punkty wewnatrz obrysu
  ;; ------------------------------------------------------
  (princ "\nGenerowanie punktow wewnatrz obrysu...")

  (setq x start-x)

  (while (<= x (+ maxx 0.0001))
    (setq y start-y)

    (while (<= y (+ maxy 0.0001))
      (setq pt (list x y zval))

      (if (geocad-grid-inside-p obj space pt minx miny maxx maxy test-z)
        (progn
          (setq res (geocad-grid-insert-unique doc space pt zval seen next-nr cnt))

          (if (> (caddr res) cnt)
            (setq cnt-inside (1+ cnt-inside))
          )

          (setq seen (car res))
          (setq next-nr (cadr res))
          (setq cnt (caddr res))
        )
      )

      (setq y (+ y step-y))
    )

    (setq x (+ x step-x))
  )

  ;; ------------------------------------------------------
  ;; 12. Aktualizacja licznika GeoprofiCAD
  ;; ------------------------------------------------------
  (if next-nr
    (progn
      (setq prefix (geocad-get-cfg "PiktPrefix" ""))
      (vlax-ldata-put "GeoLicznik" prefix next-nr)
    )
  )

  (vla-EndUndoMark doc)

  (setq *error* olderr)

  (princ
    (strcat
      "\nSukces. Wygenerowano "
      (itoa cnt)
      " pikiet."
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

  (princ)
)


(princ "\nKomenda wczytana: SIATKA_PUNKTOW")
(princ)