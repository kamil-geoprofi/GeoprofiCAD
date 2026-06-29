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
            (= (strcase block-name) (strcase *geocad-pikieta-block-name*))
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



;; --- FUNKCJA POMOCNICZA: Porownanie nazw warstw ---
(defun geocad-multi-layer-equal-p (a b)
  (and
    a
    b
    (= (strcase a) (strcase b))
  )
)


;; --- FUNKCJA POMOCNICZA: Czy obiekt jest pikieta na warstwie wynikowej ---
(defun geocad-multi-output-point-object-p
  (
    obj output-layer
    /
    type layer block-name
  )

  ;; Sprawdzamy tylko docelowa warstwe generowania pikiet.
  ;; Pikiety bazowe na innych warstwach maja prawo zostac przepisane
  ;; jako wynikowe pikiety na aktualna warstwe.
  (setq layer (geocad-multi-get-object-layer obj))

  (if (not (geocad-multi-layer-equal-p layer output-layer))
    nil

    (progn
      (setq type
        (vl-catch-all-apply
          'vla-get-ObjectName
          (list obj)
        )
      )

      (if (vl-catch-all-error-p type)
        nil

        (cond
          ((= type "AcDbPoint")
            T
          )

          ((= type "AcDbBlockReference")
            (setq block-name (geocad-multi-get-block-name obj))

            (and
              block-name
              (= (strcase block-name) (strcase *geocad-pikieta-block-name*))
            )
          )

          (T
            nil
          )
        )
      )
    )
  )
)


;; --- FUNKCJA POMOCNICZA: Porownanie punktow pikiet w tolerancji ---
(defun geocad-multi-same-output-point-p
  (
    a b xy-tol z-tol
  )

  ;; Do blokady duplikatu wymagamy zgodnosci XY i Z.
  ;; Nie blokujemy punktu o tym samym XY, ale innej wysokosci,
  ;; bo to moglaby byc realna zmiana rzednej do wygenerowania.
  (and
    (geocad-multi-valid-base-point-pt-p a)
    (geocad-multi-valid-base-point-pt-p b)
    (<= (geocad-multi-distance-xy a b) xy-tol)
    (<= (abs (- (caddr a) (caddr b))) z-tol)
  )
)


;; --- FUNKCJA POMOCNICZA: Czy pikieta wynikowa juz istnieje na warstwie docelowej ---
(defun geocad-multi-existing-output-point-at-p
  (
    target-pt output-layer
    /
    ss i en obj-res obj
    existing-pt
    found
    xy-tol z-tol
  )

  ;; Zwraca T, jezeli na aktualnej warstwie wynikowej istnieje juz:
  ;; - INSERT bloku Pikieta_Geo albo POINT,
  ;; - w tym samym XY,
  ;; - z tym samym Z w malej tolerancji.
  ;;
  ;; Ten test jest celowo uzywany tylko dla pikiet-wezlow,
  ;; zeby nie spowalniac generowania punktow posrednich.
  (setq found nil)
  (setq xy-tol 0.005)
  (setq z-tol 0.005)

  (if
    (and
      (geocad-multi-valid-base-point-pt-p target-pt)
      output-layer
    )
    (progn
      (setq ss
        (ssget
          "X"
          (list
            '(0 . "INSERT,POINT")
            (cons 8 output-layer)
          )
        )
      )

      (if ss
        (progn
          (setq i 0)

          (while
            (and
              (< i (sslength ss))
              (not found)
            )
            (setq en (ssname ss i))

            (setq obj-res
              (vl-catch-all-apply
                'vlax-ename->vla-object
                (list en)
              )
            )

            (if
              (and
                (not (vl-catch-all-error-p obj-res))
                obj-res
                (geocad-multi-output-point-object-p obj-res output-layer)
              )
              (progn
                (setq existing-pt (geocad-multi-safe-get-pt-from-obj obj-res))

                (if
                  (geocad-multi-same-output-point-p
                    target-pt
                    existing-pt
                    xy-tol
                    z-tol
                  )
                  (setq found T)
                )
              )
            )

            (setq i (1+ i))
          )
        )
      )
    )
  )

  found
)



;; --- FUNKCJA POMOCNICZA: Wstawienie pikiety-wezla ---
(defun geocad-multi-insert-base-node-if-needed
  (
    node crvObj total-len closed-curve
    batch space
    zlicz inserted-node-Ls insert-base-nodes output-layer
    /
    L L-real Z pt-cur should-insert target-pt
  )

  ;; Zwraca:
  ;; (batch zlicz inserted-node-Ls)
  ;;
  ;; Zasada:
  ;; - jezeli insert-base-nodes = "Tak", wstawiamy kazdy wezel bazowy,
  ;; - jezeli insert-base-nodes = "Nie", nadal wstawiamy wezly powstale
  ;;   z punktow odsunietych od osi, bo inaczej zniknelaby pikieta w miejscu rzutu XY,
  ;; - inserted-node-Ls zabezpiecza przed dublowaniem wezlow wspolnych segmentow,
  ;; - jezeli pikieta juz istnieje na aktualnej warstwie wynikowej, nie wstawiamy duplikatu.
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
              (setq target-pt
                (list
                  (car pt-cur)
                  (cadr pt-cur)
                  Z
                )
              )

              (if
                (geocad-multi-existing-output-point-at-p
                  target-pt
                  output-layer
                )
                ;; Punkt wynikowy juz istnieje na aktualnej warstwie pikiet.
                ;; Nie dublujemy go, ale oznaczamy L jako obsluzone,
                ;; zeby wspolny wezel segmentow nie byl sprawdzany ponownie.
                (setq inserted-node-Ls (cons L-real inserted-node-Ls))

                (progn
                  (setq batch
                    (geocad-pikieta-batch-insert
                      batch
                      space
                      target-pt
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


;; --- FUNKCJA POMOCNICZA: Filtrowanie rekordow po wielu warstwach ---
(defun geocad-multi-filter-records-by-layers (records selected-layers / filtered rec-layer)
  (setq filtered '())

  (foreach rec records
    (setq rec-layer (geocad-multi-record-layer rec))

    (if (member (strcase rec-layer) selected-layers)
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


;; --- FUNKCJA POMOCNICZA: Etykieta warstwy do okna wyboru ---
(defun geocad-multi-layer-choice-label
  (
    records layer
    /
    stats cnt min-L max-L min-Z max-Z
  )

  (setq stats (geocad-multi-layer-record-stats records layer))
  (setq cnt (nth 0 stats))
  (setq min-L (nth 1 stats))
  (setq max-L (nth 2 stats))
  (setq min-Z (nth 3 stats))
  (setq max-Z (nth 4 stats))

  (strcat
    layer
    " | pkt: "
    (itoa cnt)
    " | pikietaz: "
    (geocad-multi-format-range min-L max-L 3)
    " | Z: "
    (geocad-multi-format-range min-Z max-Z 3)
  )
)


;; --- FUNKCJA POMOCNICZA: Lista indeksow do list_box DCL ---
(defun geocad-multi-listbox-index-string (cnt / idx res)
  (setq idx 0)
  (setq res "")

  (while (< idx cnt)
    (setq res
      (strcat
        res
        (if (= res "") "" " ")
        (itoa idx)
      )
    )

    (setq idx (1+ idx))
  )

  res
)


;; --- FUNKCJA POMOCNICZA: Parsowanie indeksow z list_box DCL ---
(defun geocad-multi-parse-listbox-indexes (txt / idx len ch token indexes)
  (setq idx 1)
  (setq len (strlen txt))
  (setq token "")
  (setq indexes '())

  (while (<= idx len)
    (setq ch (substr txt idx 1))

    (if (= ch " ")
      (progn
        (if (/= token "")
          (progn
            (setq indexes (cons (atoi token) indexes))
            (setq token "")
          )
        )
      )

      (setq token (strcat token ch))
    )

    (setq idx (1+ idx))
  )

  (if (/= token "")
    (setq indexes (cons (atoi token) indexes))
  )

  (reverse indexes)
)


;; --- FUNKCJA POMOCNICZA: Parsowanie listy numerow z tekstu ---
(defun geocad-multi-parse-number-list (txt / idx len ch token nums)
  (setq idx 1)
  (setq len (strlen txt))
  (setq token "")
  (setq nums '())

  (while (<= idx len)
    (setq ch (substr txt idx 1))

    (if (wcmatch ch "#")
      (setq token (strcat token ch))

      (progn
        (if (/= token "")
          (progn
            (setq nums (cons (atoi token) nums))
            (setq token "")
          )
        )
      )
    )

    (setq idx (1+ idx))
  )

  (if (/= token "")
    (setq nums (cons (atoi token) nums))
  )

  (reverse nums)
)


;; --- FUNKCJA POMOCNICZA: Opis listy wybranych warstw ---
(defun geocad-multi-format-layer-selection-label (selected-layers all-layers / label layer)
  (cond
    ((not selected-layers)
      "Anulowano wybor"
    )

    ((= (length selected-layers) (length all-layers))
      "Wszystkie warstwy"
    )

    (T
      (progn
        (setq label "")

        (foreach layer selected-layers
          (setq label
            (strcat
              label
              (if (= label "") "" ", ")
              layer
            )
          )
        )

        label
      )
    )
  )
)


;; --- FUNKCJA POMOCNICZA: Wybor wielu warstw w oknie DCL ---
(defun geocad-multi-select-layers-dialog
  (
    records layers source-label
    /
    dcl-file dcl-fn dcl-id status
    idx layer selected-indexes selected-layers
  )

  ;; Zwraca:
  ;; - liste nazw warstw,
  ;; - ("__CANCEL__"), jezeli anulowano,
  ;; - ("__DCL_FAILED__"), jezeli okna nie udalo sie wczytac.
  (setq selected-layers '("__DCL_FAILED__"))
  (setq dcl-file (vl-filename-mktemp "geocad_multi_layers.dcl"))
  (setq dcl-fn (open dcl-file "w"))

  (if dcl-fn
    (progn
      (write-line "GeoMultiLayerSelect : dialog { label = \"NIWELACJA_MULTI - wybor warstw\";" dcl-fn)
      (write-line "  : boxed_column { label = \"Wykryte warstwy punktow bazowych\";" dcl-fn)
      (write-line "    : text { label = \"Zaznacz jedna lub kilka warstw do obliczen.\"; }" dcl-fn)
      (write-line "    : list_box { key = \"layers\"; width = 95; height = 14; multiple_select = true; }" dcl-fn)
      (write-line "  }" dcl-fn)
      (write-line "  : row { alignment = centered;" dcl-fn)
      (write-line "    : button { key = \"all\"; label = \"Zaznacz wszystkie\"; }" dcl-fn)
      (write-line "    : button { key = \"none\"; label = \"Odznacz wszystkie\"; }" dcl-fn)
      (write-line "  }" dcl-fn)
      (write-line "  : boxed_column { label = \"Status\";" dcl-fn)
      (write-line "    : text { key = \"status\"; label = \"Domyslnie zaznaczono wszystkie warstwy.\"; }" dcl-fn)
      (write-line "  }" dcl-fn)
      (write-line "  ok_cancel;" dcl-fn)
      (write-line "}" dcl-fn)
      (close dcl-fn)

      (setq dcl-id (load_dialog dcl-file))

      (if
        (and
          dcl-id
          (new_dialog "GeoMultiLayerSelect" dcl-id)
        )
        (progn
          (start_list "layers")
          (foreach layer layers
            (add_list (geocad-multi-layer-choice-label records layer))
          )
          (end_list)

          ;; Domyslnie bierzemy wszystkie warstwy, zeby zachowac szybki automat.
          (set_tile "layers" (geocad-multi-listbox-index-string (length layers)))
          (set_tile "status" (strcat source-label ": wybierz warstwy i kliknij OK."))

          (action_tile
            "all"
            "(set_tile \"layers\" (geocad-multi-listbox-index-string (length layers))) (set_tile \"status\" \"Zaznaczono wszystkie warstwy.\")"
          )

          (action_tile
            "none"
            "(set_tile \"layers\" \"\") (set_tile \"status\" \"Odznaczono wszystkie warstwy - wybierz przynajmniej jedna.\")"
          )

          (action_tile
            "accept"
            "(if (= (get_tile \"layers\") \"\") (set_tile \"status\" \"BLAD - wybierz przynajmniej jedna warstwe.\") (progn (setq selected-indexes (geocad-multi-parse-listbox-indexes (get_tile \"layers\"))) (done_dialog 1)))"
          )

          (action_tile "cancel" "(done_dialog 0)")
          (setq status (start_dialog))
        )

        (setq status -1)
      )

      (if dcl-id
        (unload_dialog dcl-id)
      )

      (vl-file-delete dcl-file)

      (cond
        ((= status 1)
          (progn
            (setq selected-layers '())

            (foreach idx selected-indexes
              (setq layer (nth idx layers))

              (if layer
                (setq selected-layers
                  (append
                    selected-layers
                    (list layer)
                  )
                )
              )
            )
          )
        )

        ((= status 0)
          (setq selected-layers '("__CANCEL__"))
        )

        ((= status -1)
          (setq selected-layers '("__DCL_FAILED__"))
        )
      )
    )
  )

  selected-layers
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


;; --- FUNKCJA POMOCNICZA: Wybor warstw z rekordow ---
(defun geocad-multi-select-records-by-layer
  (
    records source-label
    /
    layers total
    idx layer
    choice choices choice-valid num
    selected-records selected-label selected-layers selected-layer-keys dialog-result selection-cancelled
    prompt
  )

  ;; Jezeli znaleziono pikiety na kilku warstwach,
  ;; uzytkownik moze wybrac jedna, kilka albo wszystkie warstwy.
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
      ;; Najpierw probujemy wygodne okno dialogowe z wielokrotnym wyborem.
      (setq dialog-result
        (geocad-multi-select-layers-dialog records layers source-label)
      )
      (setq selection-cancelled nil)

      (cond
        ((member "__CANCEL__" dialog-result)
          (setq selected-layers '())
          (setq selection-cancelled T)
        )

        ((member "__DCL_FAILED__" dialog-result)
          (setq selected-layers nil)
        )

        (T
          (setq selected-layers dialog-result)
        )
      )

      ;; Fallback konsolowy tylko wtedy, gdy DCL nie jest dostepny.
      (if
        (and
          (not selected-layers)
          (not selection-cancelled)
        )
        (progn
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
              "\n\nWybierz warstwy punktow bazowych, np. 1,3 albo 0 = Wszystkie [0-"
              (itoa (length layers))
              "]: "
            )
          )

          (setq choices nil)

          (while (not choices)
            (setq choice (getstring T prompt))

            (if (= choice "")
              (setq choice "0")
            )

            (setq choices (geocad-multi-parse-number-list choice))

            (if (not choices)
              (setq choices '(0))
            )

            (setq choice-valid T)

            (foreach num choices
              (if
                (not
                  (and
                    (numberp num)
                    (>= num 0)
                    (<= num (length layers))
                  )
                )
                (setq choice-valid nil)
              )
            )

            (if (not choice-valid)
              (progn
                (princ
                  (strcat
                    "\nNiepoprawny wybor. Wpisz numery warstw od 1 do "
                    (itoa (length layers))
                    " rozdzielone przecinkami albo 0 = Wszystkie."
                  )
                )
                (setq choices nil)
              )
            )
          )

          (if (member 0 choices)
            (setq selected-layers layers)

            (progn
              (setq selected-layers '())

              (foreach num choices
                (setq layer (nth (1- num) layers))

                (if
                  (and
                    layer
                    (not (member layer selected-layers))
                  )
                  (setq selected-layers
                    (append
                      selected-layers
                      (list layer)
                    )
                  )
                )
              )
            )
          )
        )
      )

      (setq selected-layer-keys
        (mapcar
          'strcase
          selected-layers
        )
      )

      (setq selected-records
        (geocad-multi-filter-records-by-layers records selected-layer-keys)
      )

      (setq selected-label
        (geocad-multi-format-layer-selection-label selected-layers layers)
      )

      (princ
        (strcat
          "\nWybor warstw: "
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



;; --- FUNKCJA POMOCNICZA: Czy punkt bazowy ma poprawna geometrie i Z ---
(defun geocad-multi-valid-base-point-pt-p (pt)
  (and
    pt
    (listp pt)
    (numberp (car pt))
    (numberp (cadr pt))
    (numberp (caddr pt))
    (/= (caddr pt) 0.0)
  )
)


;; --- FUNKCJA POMOCNICZA: Klasyfikacja pierwszego wyboru MULTI ---
(defun geocad-multi-classify-first-selection
  (
    obj point-layer
    /
    supported-pt
  )

  ;; Zwraca:
  ;; "Curve" - klasyczna os: linia / polilinia / luk itd.
  ;; "Point" - pierwsza pikieta bazowa dla trybu osi z pikiet.
  ;; nil     - obiekt nieobslugiwany.
  ;;
  ;; Najpierw sprawdzamy punkt bazowy, bo INSERT Pikieta_Geo nie jest krzywa.
  (setq supported-pt (geocad-multi-supported-base-object-p obj point-layer))

  (cond
    (supported-pt
      "Point"
    )

    ((geocad-multi-valid-curve-p obj)
      "Curve"
    )

    (T
      nil
    )
  )
)


;; --- FUNKCJA POMOCNICZA: Pobranie poprawnego punktu bazowego z obiektu ---
(defun geocad-multi-base-point-from-object
  (
    obj point-layer
    /
    pt
  )

  (if
    (and
      obj
      (geocad-multi-supported-base-object-p obj point-layer)
    )
    (progn
      (setq pt (geocad-multi-safe-get-pt-from-obj obj))

      (if (geocad-multi-valid-base-point-pt-p pt)
        pt
        nil
      )
    )

    nil
  )
)


;; --- FUNKCJA POMOCNICZA: Czy kolejny punkt osi z pikiet jest poprawnie oddalony ---
(defun geocad-multi-point-far-enough-from-previous-p
  (
    pts pt
    /
    prev
  )

  ;; Chroni przed przypadkowym kliknieciem tej samej pikiety dwa razy
  ;; i przed segmentem o zerowej dlugosci.
  ;;
  ;; W AutoLISP (last pts) zwraca ostatni ELEMENT listy, a nie ostatnia pare.
  ;; Dla listy punktow:
  ;; ((x1 y1 z1) (x2 y2 z2))
  ;; dostajemy:
  ;; (x2 y2 z2)
  ;;
  ;; Nie wolno tu robic dodatkowego (car prev), bo wtedy prev stalby sie sama
  ;; wspolrzedna X i geocad-multi-distance-xy dostalby liczbe zamiast punktu.
  (if (not pts)
    T

    (progn
      (setq prev (last pts))

      (if
        (and
          (listp prev)
          (listp pt)
          (numberp (car prev))
          (numberp (cadr prev))
          (numberp (car pt))
          (numberp (cadr pt))
        )
        (> (geocad-multi-distance-xy prev pt) 0.001)
        nil
      )
    )
  )
)


;; --- FUNKCJA POMOCNICZA: Zebranie pikiet bazowych po kolei ---
(defun geocad-multi-collect-ordered-base-points
  (
    firstObj point-layer
    /
    pts pt
    ent obj-res obj
    done
  )

  ;; Tryb bez narysowanej osi:
  ;; uzytkownik wskazuje pikiety bazowe w kolejnosci przebiegu trasy.
  ;; XY tych pikiet buduje tymczasowa os 2D, a Z zostaje rzedna wezla.
  (setq pts '())

  (setq pt
    (geocad-multi-base-point-from-object firstObj point-layer)
  )

  (if pt
    (setq pts (append pts (list pt)))
  )

  (princ
    "\nTryb: os z kolejno wskazanych pikiet bazowych."
  )

  (princ
    "\nWskazuj kolejne pikiety bazowe w kolejnosci przebiegu trasy. Enter = koniec wyboru."
  )

  (setq done nil)

  (while (not done)
    (setq ent
      (car
        (entsel "\nWskaz kolejna pikiete bazowa albo Enter = koniec: ")
      )
    )

    (if (not ent)
      (setq done T)

      (progn
        (setq obj-res
          (vl-catch-all-apply
            'vlax-ename->vla-object
            (list ent)
          )
        )

        (if
          (or
            (vl-catch-all-error-p obj-res)
            (not obj-res)
          )
          (princ "\nPominieto: nie udalo sie odczytac obiektu.")

          (progn
            (setq obj obj-res)
            (setq pt
              (geocad-multi-base-point-from-object obj point-layer)
            )

            (cond
              ((not pt)
                (princ "\nPominieto: wskazany obiekt nie jest poprawna pikieta bazowa GeoprofiCAD z rzedna Z.")
              )

              ((not (geocad-multi-point-far-enough-from-previous-p pts pt))
                (princ "\nPominieto: punkt jest zbyt blisko poprzedniego wezla.")
              )

              (T
                (setq pts
                  (append
                    pts
                    (list pt)
                  )
                )

                (princ
                  (strcat
                    "\nDodano wezel "
                    (itoa (length pts))
                    "."
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  pts
)


;; --- FUNKCJA POMOCNICZA: Lista punktow do tymczasowej LWPOLYLINE 2D ---
(defun geocad-multi-points-to-lwpolyline-dxf (pts / dxf pt)
  (setq dxf
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      (cons 8 "0")
      '(100 . "AcDbPolyline")
      (cons 90 (length pts))
      '(70 . 0)
      '(38 . 0.0)
      '(210 0.0 0.0 1.0)
    )
  )

  (foreach pt pts
    (setq dxf
      (append
        dxf
        (list
          (cons 10 (list (car pt) (cadr pt)))
        )
      )
    )
  )

  dxf
)


;; --- FUNKCJA POMOCNICZA: Utworzenie tymczasowej osi 2D z pikiet ---
(defun geocad-multi-create-temp-axis-from-points
  (
    pts
    /
    en obj-res
  )

  ;; Tworzy tymczasowa otwarta LWPOLYLINE 2D po XY wskazanych pikiet.
  ;; Nie zostaje w rysunku po zakonczeniu komendy.
  (if (< (length pts) 2)
    nil

    (progn
      (setq en
        (entmakex
          (geocad-multi-points-to-lwpolyline-dxf pts)
        )
      )

      (if (not en)
        nil

        (progn
          (setq obj-res
            (vl-catch-all-apply
              'vlax-ename->vla-object
              (list en)
            )
          )

          (if
            (or
              (vl-catch-all-error-p obj-res)
              (not obj-res)
              (not (geocad-multi-valid-curve-p obj-res))
            )
            (progn
              (if obj-res
                (vl-catch-all-apply 'vla-Delete (list obj-res))
              )
              nil
            )

            (progn
              (vl-catch-all-apply 'vla-put-Visible (list obj-res :vlax-false))
              obj-res
            )
          )
        )
      )
    )
  )
)


;; --- FUNKCJA POMOCNICZA: Punkty wskazane po kolei do wezlow roboczych ---
(defun geocad-multi-ordered-points-to-nodes
  (
    pts
    /
    nodes
    prev pt
    L
  )

  ;; Wezel roboczy:
  ;; (pikietaz rzedna_Z odleglosc_XY_od_osi)
  ;;
  ;; Dla osi z pikiet gap = 0.0, bo sama os przechodzi przez wskazane pikiety.
  (setq nodes '())
  (setq prev nil)
  (setq L 0.0)

  (foreach pt pts
    (if prev
      (setq L
        (+
          L
          (geocad-multi-distance-xy prev pt)
        )
      )
    )

    (setq nodes
      (append
        nodes
        (list
          (list
            L
            (caddr pt)
            0.0
          )
        )
      )
    )

    (setq prev pt)
  )

  nodes
)



(defun c:NIWELACJA_MULTI
  (
    /
    old-err old-osmode old-cmdecho old-clayer
    crvEnt crvObj flatCrvObj ss
    first-mode axis-from-points ordered-base-points
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

  ;; --- 1. WYBOR OSI ALBO PIERWSZEJ PIKIETY BAZOWEJ ---
  (setq point-layer (geocad-multi-get-current-point-layer))
  (setq axis-from-points nil)

  (setq crvEnt
    (car
      (entsel "\n1. Wybierz os trasy (Linia/Polilinia/Luk) albo pierwsza pikiete bazowa: ")
    )
  )

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
    )
    (progn
      (alert "Nie udalo sie odczytac wybranego obiektu.")
      (exit)
    )
  )

  (setq first-mode
    (geocad-multi-classify-first-selection crvObj point-layer)
  )

  (cond
    ((= first-mode "Curve")
      ;; Klasyczny tryb: uzytkownik wskazal os liniowa.
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

      (princ "\nTryb: os z wybranego obiektu liniowego.")
      (princ "\nUtworzono robocza os 2D XY do rzutowania i liczenia pikietazu.")

      (if closed-curve
        (princ "\nWykryto zamknieta os trasy - wlaczono obsluge przejscia przez poczatek/koniec polilinii.")
      )
    )

    ((= first-mode "Point")
      ;; Nowy tryb: uzytkownik nie rysuje osi.
      ;; Kolejno wskazane pikiety bazowe buduja tymczasowa otwarta os 2D.
      (setq axis-from-points T)
      (setq point-mode "Pikiety")

      (setq ordered-base-points
        (geocad-multi-collect-ordered-base-points crvObj point-layer)
      )

      (if (< (length ordered-base-points) 2)
        (progn
          (alert "Tryb osi z pikiet wymaga przynajmniej 2 poprawnych pikiet bazowych.")
          (exit)
        )
      )

      (setq flatCrvObj
        (geocad-multi-create-temp-axis-from-points ordered-base-points)
      )

      (if (not flatCrvObj)
        (progn
          (alert "Nie udalo sie utworzyc tymczasowej osi 2D z wybranych pikiet.")
          (exit)
        )
      )

      (setq crvObj flatCrvObj)
      (setq valid-nodes
        (geocad-multi-ordered-points-to-nodes ordered-base-points)
      )

      (setq total-len (geocad-multi-curve-total-length crvObj))
      (setq closed-curve nil)
      (setq omitted 0)
      (setq selected-layer "Kolejnosc wskazanych pikiet")

      (princ
        (strcat
          "\n>>> Tryb osi z pikiet: utworzono tymczasowa os 2D z "
          (itoa (length ordered-base-points))
          " wezlow."
        )
      )
    )

    (T
      (alert
        "Wybrany obiekt nie jest ani poprawna osia trasy, ani pikieta bazowa GeoprofiCAD."
      )
      (exit)
    )
  )


  ;; --- 2. WYBOR / AUTOMATYCZNE WYKRYWANIE PUNKTOW BAZOWYCH ---
  (setq auto-tolerance 0.05)

  (if (not axis-from-points)
    (setq selected-layer nil)
  )

  (if (not axis-from-points)
    (progn
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
        point-layer
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
        point-layer
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
      (if axis-from-points
        "\nTryb wejscia: os z kolejno wskazanych pikiet."
        "\nTryb wejscia: os z obiektu liniowego."
      )
    )
  )

  (princ)
)


(princ "\nKomenda: NIWELACJA_MULTI wczytana.")
(princ)
