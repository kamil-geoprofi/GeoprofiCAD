(vl-load-com)
(load "gp_Core.lsp" "\nBLAD: Nie znaleziono pliku gp_Core.lsp!")


;; --- FUNKCJA POMOCNICZA: Bezpieczne pobieranie punktu z krzywej po pikietazu ---
(defun get-safe-curve-pt (crv L / end-res total-res total pt-res)
  (setq end-res
    (vl-catch-all-apply
      'vlax-curve-getEndParam
      (list crv)
    )
  )

  (if (or (vl-catch-all-error-p end-res) (not (numberp end-res)))
    nil

    (progn
      (setq total-res
        (vl-catch-all-apply
          'vlax-curve-getDistAtParam
          (list crv end-res)
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
  )
)


;; --- FUNKCJA POMOCNICZA: Dlugosc calej krzywej ---
(defun geocad-multi-curve-total-length (crvObj / end-param-res total-res)
  (setq end-param-res
    (vl-catch-all-apply
      'vlax-curve-getEndParam
      (list crvObj)
    )
  )

  (if (or (vl-catch-all-error-p end-param-res) (not (numberp end-param-res)))
    nil

    (progn
      (setq total-res
        (vl-catch-all-apply
          'vlax-curve-getDistAtParam
          (list crvObj end-param-res)
        )
      )

      (if
        (and
          (not (vl-catch-all-error-p total-res))
          (numberp total-res)
          (> total-res 0.001)
        )
        total-res
        nil
      )
    )
  )
)


;; --- FUNKCJA POMOCNICZA: Czy krzywa jest zamknieta ---
(defun geocad-multi-closed-curve-p (crvObj / typ has-closed-prop res sp ep)
  (setq typ
    (vl-catch-all-apply
      'vla-get-ObjectName
      (list crvObj)
    )
  )

  (setq has-closed-prop
    (vl-catch-all-apply
      'vlax-property-available-p
      (list crvObj 'Closed)
    )
  )

  (cond
    ((and (not (vl-catch-all-error-p typ)) (member typ '("AcDbCircle" "AcDbEllipse")))
      T
    )

    ((and (not (vl-catch-all-error-p has-closed-prop)) has-closed-prop)
      (setq res
        (vl-catch-all-apply
          'vla-get-Closed
          (list crvObj)
        )
      )

      (and
        (not (vl-catch-all-error-p res))
        (or
          (= res :vlax-true)
          (= res -1)
        )
      )
    )

    (T
      (setq sp
        (vl-catch-all-apply
          'vlax-curve-getStartPoint
          (list crvObj)
        )
      )

      (setq ep
        (vl-catch-all-apply
          'vlax-curve-getEndPoint
          (list crvObj)
        )
      )

      (if
        (and
          sp
          ep
          (not (vl-catch-all-error-p sp))
          (not (vl-catch-all-error-p ep))
        )
        (equal sp ep 0.001)
        nil
      )
    )
  )
)


;; --- FUNKCJA POMOCNICZA: Normalizacja pikietazu na zamknietej krzywej ---
(defun geocad-multi-normalize-L (L total-len / result)
  ;; Dla zamknietej osi pozwala liczyc L poza zakresem 0-total,
  ;; a potem zawija go z powrotem na krzywa.
  ;;
  ;; Np. total=100:
  ;; L=103 -> 3
  ;; L=-2  -> 98
  (setq result L)

  (if
    (and
      (numberp result)
      (numberp total-len)
      (> total-len 0.001)
    )
    (progn
      (while (< result 0.0)
        (setq result (+ result total-len))
      )

      (while (> result total-len)
        (setq result (- result total-len))
      )
    )
  )

  result
)


;; --- FUNKCJA POMOCNICZA: Pobranie punktu z krzywej z obsluga zawijania ---
(defun get-safe-curve-pt-wrapped (crvObj L total-len closed-curve / L-real)
  (if
    (and
      closed-curve
      (numberp total-len)
      (> total-len 0.001)
    )
    (setq L-real (geocad-multi-normalize-L L total-len))
    (setq L-real L)
  )

  (get-safe-curve-pt crvObj L-real)
)


;; --- FUNKCJA POMOCNICZA: Budowanie listy wezlow do generowania ---
(defun geocad-multi-build-generation-nodes
  (
    sorted-nodes closed-curve total-len point-mode
    /
    first-node
    a b
    L-a Z-a L-b Z-b
    forward backward
  )

  ;; Wezel ma format:
  ;; (pikietaz rzedna_Z)
  ;;
  ;; Dla otwartej krzywej zostaje stara logika:
  ;; node0 -> node1 -> node2 ...
  ;;
  ;; Dla zamknietej krzywej:
  ;; - automat / wiele wezlow: domykamy trase przez segment ostatni -> pierwszy,
  ;; - recznie i dokladnie 2 wezly: wybieramy krotsza droge miedzy nimi.
  ;;
  ;; Jezeli segment przechodzi przez poczatek/koniec krzywej, drugi wezel
  ;; dostaje techniczny pikietaz L + total-len. Dzieki temu interpolacja Z
  ;; dalej liczy sie po prawdziwej dlugosci segmentu.
  (cond
    ((or
       (not closed-curve)
       (not total-len)
       (<= total-len 0.001)
       (< (length sorted-nodes) 2)
     )
      sorted-nodes
    )

    ((and (= point-mode "Recznie") (= (length sorted-nodes) 2))
      (setq a (nth 0 sorted-nodes))
      (setq b (nth 1 sorted-nodes))

      (setq L-a (car a))
      (setq Z-a (cadr a))
      (setq L-b (car b))
      (setq Z-b (cadr b))

      ;; sorted-nodes jest posortowane, wiec L-b >= L-a.
      (setq forward (- L-b L-a))
      (setq backward (- total-len forward))

      (if (<= forward backward)
        ;; Krotsza droga bez przejscia przez zero.
        (list
          a
          b
        )

        ;; Krotsza droga przez zamkniecie:
        ;; idziemy od B do A + total.
        (list
          b
          (geocad-multi-node-with-L a (+ L-a total-len))
        )
      )
    )

    (T
      ;; Zamknieta krzywa i wiele wezlow:
      ;; generujemy wszystkie segmenty dookola, lacznie z ostatni -> pierwszy.
      (setq first-node (car sorted-nodes))

      (append
        sorted-nodes
        (list
          (geocad-multi-node-with-L
            first-node
            (+ (car first-node) total-len)
          )
        )
      )
    )
  )
)



;; --- FUNKCJA POMOCNICZA: Czy wartosc jest obiektem VLA ---
(defun geocad-multi-vla-object-p (val)
  ;; Lokalny zamiennik kontroli typu bez uzywania vlax-objectp.
  ;; W niektorych srodowiskach AutoLISP symbol vlax-objectp bywa niedostepny.
  (and
    val
    (not (vl-catch-all-error-p val))
    (= (type val) 'VLA-OBJECT)
  )
)


;; --- FUNKCJA POMOCNICZA: Punkt z Z=0 ---
(defun geocad-multi-point-z0 (pt)
  (if
    (and
      pt
      (listp pt)
      (numberp (car pt))
      (numberp (cadr pt))
    )
    (list (car pt) (cadr pt) 0.0)
    pt
  )
)


;; --- FUNKCJA POMOCNICZA: Ustawienie / dodanie kodu DXF ---
(defun geocad-multi-dxf-set (edata code value / pair)
  (setq pair (assoc code edata))

  (if pair
    (subst (cons code value) pair edata)
    (append edata (list (cons code value)))
  )
)


;; --- FUNKCJA POMOCNICZA: Splaszczenie skopiowanej osi do XY ---
(defun geocad-multi-flatten-entity-to-xy
  (
    en
    /
    ed typ
    p10 p11
    v vdata
  )

  ;; Splaszcza tylko kopie robocza osi, nigdy oryginalu uzytkownika.
  ;;
  ;; Obslugiwane typowe przypadki pracy:
  ;; - LINE,
  ;; - ARC,
  ;; - CIRCLE,
  ;; - ELLIPSE,
  ;; - LWPOLYLINE po JOIN, z zachowaniem bulge,
  ;; - POLYLINE / 3DPOLY przez splaszczenie wierzcholkow.
  ;;
  ;; Zwraca T, jezeli wykonano kontrolowane flattenowanie.
  (if (not en)
    nil

    (progn
      (setq ed (entget en))
      (setq typ (cdr (assoc 0 ed)))

      (cond
        ((= typ "LINE")
          (setq p10 (cdr (assoc 10 ed)))
          (setq p11 (cdr (assoc 11 ed)))

          (setq ed (geocad-multi-dxf-set ed 10 (geocad-multi-point-z0 p10)))
          (setq ed (geocad-multi-dxf-set ed 11 (geocad-multi-point-z0 p11)))

          (entmod ed)
          (entupd en)
          T
        )

        ((member typ '("ARC" "CIRCLE"))
          (setq p10 (cdr (assoc 10 ed)))

          (setq ed (geocad-multi-dxf-set ed 10 (geocad-multi-point-z0 p10)))
          (setq ed (geocad-multi-dxf-set ed 210 '(0.0 0.0 1.0)))

          (entmod ed)
          (entupd en)
          T
        )

        ((= typ "ELLIPSE")
          (setq p10 (cdr (assoc 10 ed)))
          (setq p11 (cdr (assoc 11 ed)))

          (setq ed (geocad-multi-dxf-set ed 10 (geocad-multi-point-z0 p10)))
          (setq ed (geocad-multi-dxf-set ed 11 (geocad-multi-point-z0 p11)))
          (setq ed (geocad-multi-dxf-set ed 210 '(0.0 0.0 1.0)))

          (entmod ed)
          (entupd en)
          T
        )

        ((= typ "LWPOLYLINE")
          ;; LWPOLYLINE przechowuje wierzcholki jako XY + elevation.
          ;; Zerujemy elevation i normal, a bulge lukow zostaje bez zmian.
          (setq ed (geocad-multi-dxf-set ed 38 0.0))
          (setq ed (geocad-multi-dxf-set ed 210 '(0.0 0.0 1.0)))

          (entmod ed)
          (entupd en)
          T
        )

        ((= typ "POLYLINE")
          ;; Dla starej POLYLINE / 3DPOLY splaszczamy wszystkie VERTEX.
          (setq ed (geocad-multi-dxf-set ed 10 '(0.0 0.0 0.0)))
          (setq ed (geocad-multi-dxf-set ed 30 0.0))
          (setq ed (geocad-multi-dxf-set ed 210 '(0.0 0.0 1.0)))

          (entmod ed)

          (setq v (entnext en))

          (while
            (and
              v
              (/= (cdr (assoc 0 (entget v))) "SEQEND")
            )
            (setq vdata (entget v))

            (if (= (cdr (assoc 0 vdata)) "VERTEX")
              (progn
                (setq p10 (cdr (assoc 10 vdata)))
                (setq vdata (geocad-multi-dxf-set vdata 10 (geocad-multi-point-z0 p10)))
                (setq vdata (geocad-multi-dxf-set vdata 30 0.0))
                (entmod vdata)
              )
            )

            (setq v (entnext v))
          )

          (entupd en)
          T
        )

        (T
          ;; Nie robimy cichego pseudo-flatten dla nietypowej geometrii,
          ;; bo NIWELACJA_MULTI ma liczyc po pewnej osi 2D.
          nil
        )
      )
    )
  )
)


;; --- FUNKCJA POMOCNICZA: Utworzenie roboczej osi 2D ---
(defun geocad-multi-create-flat-curve-copy
  (
    crvObj
    /
    copy-res flatObj en ok
  )

  ;; Tworzy tymczasowa kopie osi i splaszcza ja do XY.
  ;; Wszystkie rzutowania, pikietaze i dlugosci w NIWELACJA_MULTI
  ;; powinny isc po tej kopii, nie po oryginalnym obiekcie.
  (setq copy-res
    (vl-catch-all-apply
      'vla-Copy
      (list crvObj)
    )
  )

  (if
    (or
      (vl-catch-all-error-p copy-res)
      (not (geocad-multi-vla-object-p copy-res))
    )
    nil

    (progn
      (setq flatObj copy-res)
      (setq en (vlax-vla-object->ename flatObj))
      (setq ok (geocad-multi-flatten-entity-to-xy en))

      (if
        (and
          ok
          (geocad-multi-valid-curve-p flatObj)
        )
        (progn
          ;; Ukrywamy kopie robocza, ale zostaje dostepna dla vlax-curve.
          ;; Jezeli AutoCAD odmowi ukrycia, to i tak zostanie usunieta po komendzie.
          (vl-catch-all-apply 'vla-put-Visible (list flatObj :vlax-false))
          flatObj
        )

        (progn
          (vl-catch-all-apply 'vla-Delete (list flatObj))
          nil
        )
      )
    )
  )
)


;; --- FUNKCJA POMOCNICZA: Usuniecie roboczej osi 2D ---
(defun geocad-multi-delete-flat-curve-copy (flatObj)
  (if (geocad-multi-vla-object-p flatObj)
    (vl-catch-all-apply
      'vla-Delete
      (list flatObj)
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
  ;;
  ;; point-layer zostaje w sygnaturze dla kompatybilnosci z wczesniejsza wersja helpera.
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
  )

  ;; Logika 2D/XY:
  ;; - crvObj powinien byc robocza, splaszczona kopia osi,
  ;; - punkt bazowy jest rzutowany po XY na najblizszy punkt osi,
  ;; - pikietaz L bierzemy z rzutu na osi,
  ;; - rzedna Z zostaje z oryginalnego punktu bazowego.
  ;;
  ;; Celowo NIE ma juz korekty L po stycznej.
  ;; Najkrotsze doklejenie punktu do osi to normalna/rzut XY,
  ;; a nie przesuniecie pikietazu po stycznej.
  (setq p-proj (geocad-multi-safe-closest-point crvObj pt))

  (if p-proj
    (progn
      (setq L-base (geocad-multi-safe-dist-at-point crvObj p-proj))
      (setq gap (geocad-multi-distance-xy pt p-proj))

      (if (and (numberp L-base) (numberp gap))
        ;; Zwracamy:
        ;; (pikietaz_z_rzutu_XY rzedna_Z_z_punktu odleglosc_XY_od_osi)
        (list L-base (caddr pt) gap)
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


;; --- FUNKCJA POMOCNICZA: Rekord wezla do formatu roboczego ---
(defun geocad-multi-record-to-node (rec)
  ;; Rekord ma format:
  ;; (pikietaz rzedna_Z warstwa odleglosc_XY_od_osi)
  ;;
  ;; Wezel roboczy ma format:
  ;; (pikietaz rzedna_Z odleglosc_XY_od_osi)
  ;;
  ;; Trzeci element jest uzywany tylko do decyzji, czy trzeba wstawic
  ;; dodatkowa pikiete-wezel w miejscu rzutu punktu bazowego na os.
  (list (car rec) (cadr rec) (cadddr rec))
)


;; --- FUNKCJA POMOCNICZA: Lista rekordow do starego formatu wezlow ---
(defun geocad-multi-records-to-nodes (records / nodes)
  (setq nodes '())

  (foreach rec records
    (setq nodes
      (cons
        (geocad-multi-record-to-node rec)
        nodes
      )
    )
  )

  (reverse nodes)
)



;; --- FUNKCJA POMOCNICZA: Odleglosc wezla od osi ---
(defun geocad-multi-node-gap (node)
  (if
    (and
      node
      (numberp (nth 2 node))
    )
    (nth 2 node)
    0.0
  )
)


;; --- FUNKCJA POMOCNICZA: Czy wezel powstal z punktu odsunietego od osi ---
(defun geocad-multi-projected-node-p (node)
  ;; Jezeli punkt bazowy lezal dokladnie na osi, nie generujemy jego kopii.
  ;; Jezeli byl odsuniety, wstawiamy nowa pikiete w miejscu rzutu XY na os.
  (> (geocad-multi-node-gap node) 0.001)
)


;; --- FUNKCJA POMOCNICZA: Kopia wezla z nowym pikietazem ---
(defun geocad-multi-node-with-L (node new-L)
  (list
    new-L
    (cadr node)
    (geocad-multi-node-gap node)
  )
)


;; --- FUNKCJA POMOCNICZA: Czy pikietaz byl juz obsluzony ---
(defun geocad-multi-L-already-in-list-p (L L-list / found item)
  (setq found nil)

  (foreach item L-list
    (if (equal L item 0.001)
      (setq found T)
    )
  )

  found
)


;; --- FUNKCJA POMOCNICZA: Wstawienie pikiety-wezla ---
(defun geocad-multi-insert-base-node-if-needed
  (
    node crvObj total-len closed-curve
    batch space
    zlicz inserted-node-Ls insert-base-nodes
    /
    L L-real Z pt-cur should-insert
  )

  ;; Zwraca:
  ;; (batch zlicz inserted-node-Ls)
  ;;
  ;; Zasada:
  ;; - jezeli insert-base-nodes = "Tak", wstawiamy kazdy wezel bazowy,
  ;; - jezeli insert-base-nodes = "Nie", nadal wstawiamy wezly powstale
  ;;   z punktow odsunietych od osi, bo inaczej zniknelaby pikieta w miejscu rzutu XY,
  ;; - inserted-node-Ls zabezpiecza przed dublowaniem wezlow wspolnych segmentow.
  ;;
  ;; Z punktu bazowego bierzemy Z, a XY pikiety wynikowej bierzemy z osi 2D.
  (setq L (car node))
  (setq Z (cadr node))

  (setq should-insert
    (or
      (= insert-base-nodes "Tak")
      (geocad-multi-projected-node-p node)
    )
  )

  (if
    (and
      should-insert
      (numberp L)
      (numberp Z)
    )
    (progn
      (if
        (and
          closed-curve
          (numberp total-len)
          (> total-len 0.001)
        )
        (setq L-real (geocad-multi-normalize-L L total-len))
        (setq L-real L)
      )

      (if (not (geocad-multi-L-already-in-list-p L-real inserted-node-Ls))
        (progn
          (setq pt-cur (get-safe-curve-pt-wrapped crvObj L total-len closed-curve))

          (if pt-cur
            (progn
              (setq batch
                (geocad-pikieta-batch-insert
                  batch
                  space
                  (list
                    (car pt-cur)
                    (cadr pt-cur)
                    Z
                  )
                  nil
                  T
                )
              )

              (setq zlicz (1+ zlicz))
              (setq inserted-node-Ls (cons L-real inserted-node-Ls))
            )
          )
        )
      )
    )
  )

  (list batch zlicz inserted-node-Ls)
)



;; --- FUNKCJA POMOCNICZA: Budowanie rekordow wezlow z selection set ---
(defun geocad-multi-build-valid-node-records-from-ss
  (
    ss crvObj tolerance point-layer
    /
    records omitted
    i en obj pt node gap layer
  )

  ;; tolerance = nil    -> tryb reczny, bez filtrowania po odleglosci od osi.
  ;; tolerance = liczba -> tryb automat, tylko punkty w zadanej odleglosci XY od osi.
  ;;
  ;; Przyjmujemy tylko standardowe zrodla GeoprofiCAD:
  ;; - INSERT bloku Pikieta_Geo niezaleznie od warstwy,
  ;; - POINT na dowolnej zarzadzanej warstwie *_PIKIETY.
  ;;
  ;; Rekord ma format:
  ;; (pikietaz rzedna_Z warstwa odleglosc_XY_od_osi)
  (setq records '())
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
                    (progn
                      (setq layer (geocad-multi-get-object-layer obj))

                      (if (not layer)
                        (setq layer "<BRAK_WARSTWY>")
                      )

                      (setq records
                        (cons
                          (list
                            (car node)
                            (cadr node)
                            layer
                            (caddr node)
                          )
                          records
                        )
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

  (list records omitted)
)


;; --- FUNKCJA POMOCNICZA: Zgodnosc ze starsza wersja helpera ---
(defun geocad-multi-build-valid-nodes-from-ss
  (
    ss crvObj tolerance point-layer
    /
    res records omitted
  )

  (setq res
    (geocad-multi-build-valid-node-records-from-ss
      ss
      crvObj
      tolerance
      point-layer
    )
  )

  (setq records (car res))
  (setq omitted (cadr res))

  (list
    (geocad-multi-records-to-nodes records)
    omitted
  )
)


;; --- FUNKCJA POMOCNICZA: Automatyczne skanowanie calego rysunku ---
(defun geocad-multi-scan-auto-node-records (crvObj tolerance point-layer / ss)
  ;; Skanujemy tylko potencjalne zrodla GeoprofiCAD.
  ;; Dokladny filtr nazwy bloku i warstwy jest robiony pozniej,
  ;; bo ssget nie obsluzy wygodnie warunku:
  ;; (INSERT Pikieta_Geo) albo (POINT na warstwie *_PIKIETY).
  (setq ss (ssget "X" '((0 . "INSERT,POINT"))))

  (geocad-multi-build-valid-node-records-from-ss ss crvObj tolerance point-layer)
)


;; --- FUNKCJA POMOCNICZA: Zgodnosc ze starsza nazwa helpera automatu ---
(defun geocad-multi-scan-auto-nodes (crvObj tolerance point-layer / res records omitted)
  (setq res
    (geocad-multi-scan-auto-node-records
      crvObj
      tolerance
      point-layer
    )
  )

  (setq records (car res))
  (setq omitted (cadr res))

  (list
    (geocad-multi-records-to-nodes records)
    omitted
  )
)


;; --- FUNKCJA POMOCNICZA: Warstwa z rekordu wezla ---
(defun geocad-multi-record-layer (rec)
  (if (and rec (caddr rec))
    (caddr rec)
    "<BRAK_WARSTWY>"
  )
)


;; --- FUNKCJA POMOCNICZA: Unikalne warstwy z rekordow ---
(defun geocad-multi-unique-record-layers (records / layers layer)
  (setq layers '())

  (foreach rec records
    (setq layer (geocad-multi-record-layer rec))

    (if (not (member layer layers))
      (setq layers
        (append
          layers
          (list layer)
        )
      )
    )
  )

  layers
)


;; --- FUNKCJA POMOCNICZA: Liczenie rekordow na warstwie ---
(defun geocad-multi-count-records-on-layer (records layer / cnt rec-layer)
  (setq cnt 0)

  (foreach rec records
    (setq rec-layer (geocad-multi-record-layer rec))

    (if (= (strcase rec-layer) (strcase layer))
      (setq cnt (1+ cnt))
    )
  )

  cnt
)


;; --- FUNKCJA POMOCNICZA: Filtrowanie rekordow po warstwie ---
(defun geocad-multi-filter-records-by-layer (records layer / filtered rec-layer)
  (setq filtered '())

  (foreach rec records
    (setq rec-layer (geocad-multi-record-layer rec))

    (if (= (strcase rec-layer) (strcase layer))
      (setq filtered
        (cons
          rec
          filtered
        )
      )
    )
  )

  (reverse filtered)
)


;; --- FUNKCJA POMOCNICZA: Statystyki rekordow dla jednej warstwy ---
(defun geocad-multi-layer-record-stats
  (
    records layer
    /
    cnt rec rec-layer
    L Z
    min-L max-L min-Z max-Z
  )

  ;; Zwraca:
  ;; (ilosc min-L max-L min-Z max-Z)
  (setq cnt 0)
  (setq min-L nil)
  (setq max-L nil)
  (setq min-Z nil)
  (setq max-Z nil)

  (foreach rec records
    (setq rec-layer (geocad-multi-record-layer rec))

    (if (= (strcase rec-layer) (strcase layer))
      (progn
        (setq L (car rec))
        (setq Z (cadr rec))

        (if (and (numberp L) (numberp Z))
          (progn
            (setq cnt (1+ cnt))

            (if (or (not min-L) (< L min-L))
              (setq min-L L)
            )

            (if (or (not max-L) (> L max-L))
              (setq max-L L)
            )

            (if (or (not min-Z) (< Z min-Z))
              (setq min-Z Z)
            )

            (if (or (not max-Z) (> Z max-Z))
              (setq max-Z Z)
            )
          )
        )
      )
    )
  )

  (list cnt min-L max-L min-Z max-Z)
)


;; --- FUNKCJA POMOCNICZA: Formatowanie zakresu liczbowego ---
(defun geocad-multi-format-range (a b precision)
  (if
    (and
      (numberp a)
      (numberp b)
    )
    (strcat
      (rtos a 2 precision)
      " - "
      (rtos b 2 precision)
    )
    "brak danych"
  )
)


;; --- FUNKCJA POMOCNICZA: Wypis jednej pozycji wyboru warstwy ---
(defun geocad-multi-print-layer-choice-line
  (
    idx records layer
    /
    stats cnt min-L max-L min-Z max-Z
  )

  (setq stats (geocad-multi-layer-record-stats records layer))
  (setq cnt (nth 0 stats))
  (setq min-L (nth 1 stats))
  (setq max-L (nth 2 stats))
  (setq min-Z (nth 3 stats))
  (setq max-Z (nth 4 stats))

  (princ
    (strcat
      "\n\n["
      (itoa idx)
      "] "
      layer
      "\n    punkty: "
      (itoa cnt)
      "\n    pikietaz: "
      (geocad-multi-format-range min-L max-L 3)
      "\n    Z: "
      (geocad-multi-format-range min-Z max-Z 3)
    )
  )
)


;; --- FUNKCJA POMOCNICZA: Wybor warstwy z rekordow ---
(defun geocad-multi-select-records-by-layer
  (
    records source-label
    /
    layers total
    idx layer count
    choice selected-records selected-label
    prompt
  )

  ;; Jezeli znaleziono pikiety na kilku warstwach,
  ;; uzytkownik moze wybrac konkretna warstwe albo wszystkie.
  ;;
  ;; Zwraca:
  ;; (wybrane-rekordy opis-wyboru)
  (setq total (length records))
  (setq layers (geocad-multi-unique-record-layers records))

  (cond
    ((= total 0)
      (list '() "Brak")
    )

    ((<= (length layers) 1)
      (setq selected-label
        (if layers
          (car layers)
          "Brak"
        )
      )

      (princ
        (strcat
          "\n"
          source-label
          ": wykryto jedna warstwe punktow bazowych."
        )
      )

      (if layers
        (geocad-multi-print-layer-choice-line 1 records selected-label)
      )

      (princ
        (strcat
          "\n\nWybor warstwy: "
          selected-label
          " ("
          (itoa total)
          " punktow)."
        )
      )

      (list records selected-label)
    )

    (T
      (princ
        (strcat
          "\n"
          source-label
          ": wykryto punkty bazowe na kilku warstwach:"
        )
      )

      (setq idx 1)

      (foreach layer layers
        (geocad-multi-print-layer-choice-line idx records layer)
        (setq idx (1+ idx))
      )

      (princ
        (strcat
          "\n\n[0] Wszystkie warstwy"
          "\n    punkty: "
          (itoa total)
        )
      )

      (setq prompt
        (strcat
          "\n\nWybierz warstwe punktow bazowych [1-"
          (itoa (length layers))
          "] albo 0 = Wszystkie: "
        )
      )

      (setq choice nil)

      (while (not choice)
        (setq choice (getint prompt))

        (if (not choice)
          (setq choice 0)
        )

        (if
          (not
            (and
              (numberp choice)
              (>= choice 0)
              (<= choice (length layers))
            )
          )
          (progn
            (princ
              (strcat
                "\nNiepoprawny wybor. Wpisz liczbe od 0 do "
                (itoa (length layers))
                "."
              )
            )
            (setq choice nil)
          )
        )
      )

      (if (= choice 0)
        (progn
          (setq selected-records records)
          (setq selected-label "Wszystkie warstwy")
        )

        (progn
          (setq layer (nth (1- choice) layers))
          (setq selected-records
            (geocad-multi-filter-records-by-layer records layer)
          )
          (setq selected-label layer)
        )
      )

      (princ
        (strcat
          "\nWybor warstwy: "
          selected-label
          " ("
          (itoa (length selected-records))
          " punktow)."
        )
      )

      (list selected-records selected-label)
    )
  )
)


;; --- FUNKCJA POMOCNICZA: Wybor warstwy dla automatu ---
(defun geocad-multi-select-auto-records-by-layer (records)
  ;; Zostawione dla zgodnosci z aktualna wersja pliku.
  (geocad-multi-select-records-by-layer records "Automat")
)


(defun c:NIWELACJA_MULTI
  (
    /
    old-err old-osmode old-cmdecho old-clayer
    crvEnt crvObj flatCrvObj ss
    valid-nodes generation-nodes
    auto-valid-records auto-selected-records
    auto-result selected-result manual-result manual-records manual-selected-records
    auto-omitted omitted
    auto-tolerance tol-input point-mode point-layer selected-layer
    closed-curve total-len
    node1 node2 L1 Z1 L2 Z2 dL slope
    node-insert-result inserted-node-Ls insert-base-nodes
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

          (if flatCrvObj
            (progn
              (geocad-multi-delete-flat-curve-copy flatCrvObj)
              (setq flatCrvObj nil)
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

  ;; Do obliczen NIWELACJA_MULTI uzywamy roboczej osi 2D.
  ;; Oryginalny obiekt uzytkownika nie jest modyfikowany.
  (setq flatCrvObj (geocad-multi-create-flat-curve-copy crvObj))

  (if (not flatCrvObj)
    (progn
      (alert
        "Nie udalo sie utworzyc roboczej osi 2D. Wybierz linie, luk albo polilinie mozliwa do splaszczenia."
      )
      (exit)
    )
  )

  ;; Od tego miejsca cala geometria pracuje na splaszczonej kopii XY.
  (setq crvObj flatCrvObj)

  (setq total-len (geocad-multi-curve-total-length crvObj))
  (setq closed-curve (geocad-multi-closed-curve-p crvObj))

  (princ "\nUtworzono robocza os 2D XY do rzutowania i liczenia pikietazu.")

  (if closed-curve
    (princ "\nWykryto zamknieta os trasy - wlaczono obsluge przejscia przez poczatek/koniec polilinii.")
  )


  ;; --- 2. WYBOR / AUTOMATYCZNE WYKRYWANIE PUNKTOW BAZOWYCH ---
  (setq auto-tolerance 0.05)
  (setq point-layer (geocad-multi-get-current-point-layer))
  (setq selected-layer nil)

  (princ
    (strcat
      "\n2. Skanowanie punktow bazowych przy osi..."
      "\nZrodla automatu: Pikieta_Geo oraz POINT na warstwach *_PIKIETY."
    )
  )

  (setq auto-result
    (geocad-multi-scan-auto-node-records crvObj auto-tolerance point-layer)
  )

  (setq auto-valid-records (car auto-result))
  (setq auto-omitted (cadr auto-result))

  (princ
    (strcat
      "\nAutomat: wykryto "
      (itoa (length auto-valid-records))
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
          (geocad-multi-scan-auto-node-records crvObj auto-tolerance point-layer)
        )

        (setq auto-valid-records (car auto-result))
        (setq auto-omitted (cadr auto-result))
        (setq selected-layer nil)

        (princ
          (strcat
            "\nAutomat: wykryto "
            (itoa (length auto-valid-records))
            " punktow bazowych na osi. Tolerancja: "
            (rtos auto-tolerance 2 3)
            " m."
          )
        )

        ;; Wracamy do menu.
        (setq point-mode nil)
      )

      ((= point-mode "Automat")
        (if (< (length auto-valid-records) 2)
          (progn
            (alert
              "Automat znalazl mniej niz 2 poprawne punkty bazowe. Zmien tolerancje albo wybierz punkty recznie."
            )
            (setq point-mode nil)
          )

          (progn
            (setq selected-result
              (geocad-multi-select-auto-records-by-layer auto-valid-records)
            )

            (setq auto-selected-records (car selected-result))
            (setq selected-layer (cadr selected-result))

            (if (< (length auto-selected-records) 2)
              (progn
                (alert
                  "Wybrana warstwa ma mniej niz 2 poprawne punkty bazowe. Wybierz wszystkie warstwy, zmien tolerancje albo wybierz punkty recznie."
                )
                (setq point-mode nil)
              )

              (progn
                (setq valid-nodes
                  (geocad-multi-records-to-nodes auto-selected-records)
                )

                ;; Pominiete = obiekty odrzucone podczas skanowania + poprawne punkty
                ;; z warstw, ktorych uzytkownik nie wybral.
                (setq omitted
                  (+
                    auto-omitted
                    (- (length auto-valid-records) (length auto-selected-records))
                  )
                )
              )
            )
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
              (geocad-multi-build-valid-node-records-from-ss ss crvObj nil point-layer)
            )

            (setq manual-records (car manual-result))
            (setq omitted (cadr manual-result))
            (setq selected-layer nil)

            (if (< (length manual-records) 2)
              (progn
                (alert
                  "Za malo poprawnych punktow! Zaznacz przynajmniej 2 pikiety GeoprofiCAD z rzedna Z."
                )
                (setq point-mode nil)
              )

              (progn
                ;; Rowniez w trybie recznym pilnujemy warstw.
                ;; Jezeli zaznaczono pikiety z kilku warstw, uzytkownik wybiera,
                ;; ktora warstwe uwzglednic albo czy brac wszystkie.
                (setq selected-result
                  (geocad-multi-select-records-by-layer manual-records "Tryb reczny")
                )

                (setq manual-selected-records (car selected-result))
                (setq selected-layer (cadr selected-result))

                (if (< (length manual-selected-records) 2)
                  (progn
                    (alert
                      "Wybrana warstwa ma mniej niz 2 poprawne punkty bazowe. Wybierz wszystkie warstwy albo zaznacz inne punkty."
                    )
                    (setq point-mode nil)
                  )

                  (setq valid-nodes
                    (geocad-multi-records-to-nodes manual-selected-records)
                  )
                )
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

  ;; Sortowanie po pikietazu rzutu XY od najmniejszego do najwiekszego.
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

  (setq generation-nodes
    (geocad-multi-build-generation-nodes
      valid-nodes
      closed-curve
      total-len
      point-mode
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

  (if selected-layer
    (princ
      (strcat
        "\n>>> Warstwy punktow bazowych: "
        selected-layer
        "."
      )
    )
  )

  (if
    (and
      closed-curve
      (> (length generation-nodes) (length valid-nodes))
    )
    (princ "\n>>> Zamknieta os: dodano techniczny segment domykajacy ostatni wezel z pierwszym.")
  )


  ;; --- 4. METODA I OPCJE ---
  ;; Rowna     - rowne odstepy miedzy wierzcholkami/wezlami.
  ;; Odleglosc - sztywny krok co podana odleglosc.
  ;; Podzial   - podana liczba odcinkow miedzy kazda para wezlow.
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


  (initget "Tak Nie")
  (setq insert-base-nodes
    (getkword "\nWstawic pikiety w wezlach bazowych? [Tak/Nie] <Tak>: ")
  )

  (if (not insert-base-nodes)
    (setq insert-base-nodes "Tak")
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
  (setq inserted-node-Ls '())


  ;; Zabezpieczenie pierwszej krawedzi do 3D.
  (setq L1 (car (nth 0 generation-nodes)))
  (setq Z1 (cadr (nth 0 generation-nodes)))
  (setq pt-cur (get-safe-curve-pt-wrapped crvObj L1 total-len closed-curve))

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

  (while (< i (1- (length generation-nodes)))
    (setq node1 (nth i generation-nodes))
    (setq node2 (nth (1+ i) generation-nodes))

    (setq L1 (car node1))
    (setq Z1 (cadr node1))

    (setq L2 (car node2))
    (setq Z2 (cadr node2))

    ;; Wstawienie pikiety-wezla.
    ;; Domyslnie wstawiamy kazdy wezel bazowy, zeby nie bylo dziur
    ;; w modelu w miejscach zalaman / punktow kontrolnych.
    ;; Przy opcji Nie nadal wstawiane sa wezly powstale z rzutu punktow
    ;; odsunietych od osi.
    (setq node-insert-result
      (geocad-multi-insert-base-node-if-needed
        node1
        crvObj
        total-len
        closed-curve
        batch
        space
        zlicz
        inserted-node-Ls
        insert-base-nodes
      )
    )

    (setq batch (nth 0 node-insert-result))
    (setq zlicz (nth 1 node-insert-result))
    (setq inserted-node-Ls (nth 2 node-insert-result))

    (setq dL (- L2 L1))

    ;; Wykonuj tylko, jesli punkty nie sa w tym samym miejscu.
    (if (> dL 0.001)
      (progn
        ;; Najwazniejsza logika wysokosci:
        ;; Z interpolujemy po rzeczywistej dlugosci segmentu L1-L2.
        ;; Dla zamknietej osi L2 moze byc technicznie wieksze od total-len
        ;; wtedy geometria jest zawijana, ale dL pozostaje prawidlowa.
        (setq slope (/ (- Z2 Z1) dL))
        (setq L-cur L1)

        (cond

          ;; ======================================================
          ;; TRYB: Rowna
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
          ;; L1 i L2 sa obslugiwane osobno jako pikiety-wezly, zgodnie z opcja uzytkownika.
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
              (setq pt-cur (get-safe-curve-pt-wrapped crvObj L-cur total-len closed-curve))

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
          ;; TRYB: Odleglosc
          ;;
          ;; Idzie sztywnym krokiem co "step".
          ;; Moze zostawic krotka koncowke przy wezle.
          ;; ======================================================
          ((= mode "Odleglosc")
            (setq L-cur (+ L-cur step))

            (while (< L-cur (- L2 0.01))
              (setq z-cur (+ Z1 (* (- L-cur L1) slope)))
              (setq pt-cur (get-safe-curve-pt-wrapped crvObj L-cur total-len closed-curve))

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
          ;; TRYB: Podzial
          ;;
          ;; Dzieli kazdy segment na podana liczbe odcinkow.
          ;; ======================================================
          ((= mode "Podzial")
            (setq segment-step (/ dL (float num-pts)))
            (setq L-cur (+ L-cur segment-step))

            (repeat (1- num-pts)
              (if (< L-cur (- L2 0.01))
                (progn
                  (setq z-cur (+ Z1 (* (- L-cur L1) slope)))
                  (setq pt-cur (get-safe-curve-pt-wrapped crvObj L-cur total-len closed-curve))

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


    ;; Wstawienie koncowego wezla segmentu.
    ;; Lista inserted-node-Ls zabezpiecza przed dublowaniem wezlow wspolnych.
    (setq node-insert-result
      (geocad-multi-insert-base-node-if-needed
        node2
        crvObj
        total-len
        closed-curve
        batch
        space
        zlicz
        inserted-node-Ls
        insert-base-nodes
      )
    )

    (setq batch (nth 0 node-insert-result))
    (setq zlicz (nth 1 node-insert-result))
    (setq inserted-node-Ls (nth 2 node-insert-result))


    ;; Zamkniecie aktualnego segmentu dla Linii 3D.
    (setq pt-cur (get-safe-curve-pt-wrapped crvObj L2 total-len closed-curve))

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

  (if flatCrvObj
    (progn
      (geocad-multi-delete-flat-curve-copy flatCrvObj)
      (setq flatCrvObj nil)
    )
  )

  (setq *error* old-err)

  (princ
    (strcat
      "\nSukces! Wygenerowano "
      (itoa zlicz)
      " pikiet we wszystkich segmentach."
      "\nOpcja pikiet w wezlach bazowych: "
      insert-base-nodes
      "."
    )
  )

  (princ)
)


(princ "\nKomenda: NIWELACJA_MULTI wczytana.")
(princ)