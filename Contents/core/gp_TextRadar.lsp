;; ======================================================
;; GEOPROFICAD - TEXT RADAR
;; ======================================================
;;
;; Wspolne helpery do szybkiego parowania punktow z tekstami.
;; Logika bazuje na szybkim radarze z gp_Export.lsp:
;; - teksty sa zbierane raz do listy,
;; - odleglosc liczona jest do bounding boxa tekstu,
;; - srodek bounding boxa jest tie-breakerem.
;; ======================================================

(setq *geocad-module-textradar-loaded* T)

(defun geocad-dist-2d (p1 p2)
  (distance
    (list (car p1) (cadr p1))
    (list (car p2) (cadr p2))
  )
)

(defun geocad-text-radar-categorize (txt / norm-txt is-num has-sep)
  ;; Zwraca:
  ;; - "Z"  dla tekstu wygladajacego jak rzedna,
  ;; - "ID" dla pozostalych opisow/numerow.
  (setq norm-txt
    (vl-string-trim
      " mM\r\n\t"
      (vl-string-translate "," "." (if txt txt ""))
    )
  )
  (setq is-num (distof norm-txt))
  (setq has-sep
    (or
      (vl-string-search "." norm-txt)
      (vl-string-search "," (if txt txt ""))
    )
  )
  (if (and is-num has-sep) "Z" "ID")
)


(defun geocad-text-radar-z-value (txt / norm-txt val)
  ;; Zwraca wartosc liczbowa tekstu rzednej albo nil.
  ;; Uzywamy tej samej normalizacji co kategoryzacja radaru,
  ;; zeby eksport nie odczytywal Z innym algorytmem niz raport.
  (setq norm-txt
    (vl-string-trim
      " mM\r\n\t"
      (vl-string-translate "," "." (if txt txt ""))
    )
  )
  (setq val (distof norm-txt))
  val
)

(defun geocad-text-radar-object-string (obj / val)
  (setq val "")
  (if obj
    (progn
      (setq val
        (vl-catch-all-apply
          'vla-get-TextString
          (list obj)
        )
      )
      (if (vl-catch-all-error-p val)
        (setq val "")
      )
    )
  )
  (if val val "")
)

(defun geocad-text-radar-item-from-object
  (obj / minPt maxPt res txt cat)
  ;; Item ma strukture zgodna z dotychczasowa logika eksportu:
  ;; (minPt maxPt text category object)
  (setq res nil)
  (if obj
    (progn
      (setq res
        (vl-catch-all-apply
          'vla-GetBoundingBox
          (list obj 'minPt 'maxPt)
        )
      )
      (if (not (vl-catch-all-error-p res))
        (progn
          (setq txt (geocad-text-radar-object-string obj))
          (setq cat (geocad-text-radar-categorize txt))
          (setq res
            (list
              (vlax-safearray->list minPt)
              (vlax-safearray->list maxPt)
              txt
              cat
              obj
            )
          )
        )
        (setq res nil)
      )
    )
  )
  res
)

(defun geocad-text-radar-collect
  (filter / ss i obj item result)
  ;; Zbiera TEXT/MTEXT raz do listy radarowej.
  ;; filter to zwykly filtr ssget, np. '((0 . "TEXT,MTEXT")) albo
  ;; (list '(0 . "TEXT,MTEXT") (cons 8 layname)).
  (setq result '())
  (setq ss (ssget "_X" filter))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq obj (vlax-ename->vla-object (ssname ss i)))
        (setq item (geocad-text-radar-item-from-object obj))
        (if item
          (setq result (cons item result))
        )
        (setq i (1+ i))
      )
    )
  )
  result
)

(defun geocad-text-radar-collect-layer (layname)
  (if
    (and
      layname
      (/= layname "")
      (tblsearch "LAYER" layname)
    )
    (geocad-text-radar-collect
      (list
        '(0 . "TEXT,MTEXT")
        (cons 8 layname)
      )
    )
    '()
  )
)

(defun geocad-text-radar-distance
  (pt item / px py rMinX rMaxX rMinY rMaxY cX cY d-edge cx-center cy-center d-center)
  ;; Zwraca (distance-to-edge distance-to-center).
  ;; Edge decyduje, czy tekst wpada w radar.
  ;; Center rozstrzyga, ktory tekst jest najlepszy.
  (setq px (car pt))
  (setq py (cadr pt))
  (setq rMinX (min (caar item) (caadr item)))
  (setq rMaxX (max (caar item) (caadr item)))
  (setq rMinY (min (cadar item) (cadadr item)))
  (setq rMaxY (max (cadar item) (cadadr item)))

  (setq cX (max rMinX (min px rMaxX)))
  (setq cY (max rMinY (min py rMaxY)))
  (setq d-edge (geocad-dist-2d pt (list cX cY)))

  (setq cx-center (/ (+ rMinX rMaxX) 2.0))
  (setq cy-center (/ (+ rMinY rMaxY) 2.0))
  (setq d-center (geocad-dist-2d pt (list cx-center cy-center)))

  (list d-edge d-center)
)

(defun geocad-text-radar-find-nearest
  (pt items radius category / item dists d-edge d-center best best-center cat)
  ;; Szuka najlepszego tekstu z gotowej listy.
  ;; Nie wykonuje ssget.
  ;; category moze byc nil, "ID" albo "Z".
  (setq best nil)
  (setq best-center nil)

  (foreach item items
    (setq cat (nth 3 item))
    (if
      (or
        (not category)
        (= cat category)
      )
      (progn
        (setq dists (geocad-text-radar-distance pt item))
        (setq d-edge (car dists))
        (setq d-center (cadr dists))
        (if
          (and
            (<= d-edge radius)
            (or
              (not best-center)
              (< d-center best-center)
            )
          )
          (progn
            (setq best item)
            (setq best-center d-center)
          )
        )
      )
    )
  )

  best
)

(defun geocad-text-radar-item-object (item)
  (nth 4 item)
)

(defun geocad-text-radar-item-text (item)
  (if item (nth 2 item) "")
)

(defun geocad-text-radar-remove-item (item items)
  (if item
    (vl-remove item items)
    items
  )
)

(princ)
