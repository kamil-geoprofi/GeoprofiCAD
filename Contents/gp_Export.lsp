(vl-load-com)
(load "gp_Core.lsp" "\nBLAD: Nie znaleziono pliku gp_Core.lsp!")

;; ======================================================
;; GEOPROFICAD - EKSPORT PIKIET V23
;; ======================================================
;;
;; Najwazniejsze zalozenia:
;; - jedna analiza zasila raport i finalny eksport,
;; - kazdy typ obiektu mozna wlaczyc lub wylaczyc,
;; - raport pokazuje zrodlo ID i Z dla kazdego typu,
;; - bloki sa czytane kolejno z:
;;     atrybutow -> geometrii -> tekstow w definicji -> radaru,
;; - opcjonalnie jeden tekst radaru moze zostac uzyty tylko raz,
;; - TEXT/MTEXT sa danymi pomocniczymi i nie sa eksportowane jako punkty.
;;
;; Komendy:
;;   EKSPORT_PIKIET_V23
;;   EKSPORT_PIKIET_V22  (alias zgodnosci)
;; ======================================================

(setq *gp-exp-block-text-cache* '())

;; ======================================================
;; PODSTAWOWE HELPERY
;; ======================================================

(defun gp-exp-dist-2d (p1 p2)
  (geocad-dist-2d p1 p2)
)

(defun gp-exp-format-coord (val)
  (vl-string-translate "," "." (rtos val 2 3))
)

(defun gp-exp-trim (txt)
  (vl-string-trim " \t\r\n" (if txt txt ""))
)

(defun gp-exp-nonempty-p (txt)
  (/= (gp-exp-trim txt) "")
)

(defun gp-exp-has-digit-p (txt / i ch found)
  (setq txt (if txt txt "")
        i 1
        found nil)
  (while (and (not found) (<= i (strlen txt)))
    (setq ch (ascii (substr txt i 1)))
    (if (and (>= ch 48) (<= ch 57))
      (setq found T)
    )
    (setq i (1+ i))
  )
  found
)

(defun gp-exp-safe-object-name (obj / res)
  (setq res (vl-catch-all-apply 'vla-get-ObjectName (list obj)))
  (if (vl-catch-all-error-p res) "" res)
)

(defun gp-exp-safe-text-string (obj / res)
  (setq res (vl-catch-all-apply 'vla-get-TextString (list obj)))
  (if (vl-catch-all-error-p res) "" res)
)

(defun gp-exp-safe-tag-string (obj / res)
  (setq res (vl-catch-all-apply 'vla-get-TagString (list obj)))
  (if (vl-catch-all-error-p res) "" (strcase res))
)

(defun gp-exp-safe-handle (obj / res)
  (setq res (vl-catch-all-apply 'vla-get-Handle (list obj)))
  (if (vl-catch-all-error-p res) "" res)
)

(defun gp-exp-parse-tags (txt)
  (mapcar 'strcase (geocad-parse-tags txt))
)

(defun gp-exp-parse-number (txt / norm val)
  ;; Akceptuje liczby calkowite i dziesietne, z kropka albo przecinkiem.
  (setq norm (vl-string-translate "," "." (gp-exp-trim txt)))
  (if (= norm "")
    nil
    (progn
      (setq val (distof norm))
      val
    )
  )
)

(defun gp-exp-text-category (txt)
  (geocad-text-radar-categorize txt)
)

(defun gp-exp-record-get (record key)
  (cdr (assoc key record))
)

(defun gp-exp-map-get (map key)
  (cdr (assoc key map))
)

(defun gp-exp-kind-label (kind)
  (cond
    ((= kind "POINT") "POINT")
    ((= kind "INSERT") "INSERT")
    ((= kind "LINE") "LINE")
    ((= kind "POLYLINE") "POLYLINE")
    ((= kind "ARC") "ARC")
    ((= kind "CIRCLE") "CIRCLE")
    ((= kind "SOLID") "SOLID")
    (T kind)
  )
)

(defun gp-exp-kind-tile (kind)
  (cond
    ((= kind "POINT") "use_point")
    ((= kind "INSERT") "use_insert")
    ((= kind "LINE") "use_line")
    ((= kind "POLYLINE") "use_polyline")
    ((= kind "ARC") "use_arc")
    ((= kind "CIRCLE") "use_circle")
    ((= kind "SOLID") "use_solid")
    (T "")
  )
)

(defun gp-exp-source-label (source)
  (cond
    ((= source "ATTR") "atrybut")
    ((= source "BLOCK_TEXT") "tekst-bloku")
    ((= source "GEOM") "geometria")
    ((= source "RADAR") "tekst-obok")
    ((= source "AUTO") "auto")
    ((= source "ZERO") "zero")
    (T "brak")
  )
)

(defun gp-exp-list-inc (alist key / pair)
  (setq pair (assoc key alist))
  (if pair
    (subst (cons key (1+ (cdr pair))) pair alist)
    (cons (cons key 1) alist)
  )
)

(defun gp-exp-count-get (alist key)
  (if (assoc key alist) (cdr (assoc key alist)) 0)
)

(defun gp-exp-unique-cons (value values)
  (if (member value values) values (cons value values))
)

;; ======================================================
;; OBSLUGA BLEDOW I DIALOGU ZAPISU
;; ======================================================

(defun gp-exp-error (msg)
  (if f
    (vl-catch-all-apply 'close (list f))
  )
  (if dcl-id
    (vl-catch-all-apply 'unload_dialog (list dcl-id))
  )
  (if (and dcl-file (findfile dcl-file))
    (vl-file-delete dcl-file)
  )
  (setq *error* old-err)
  (princ
    (if (member msg '("Function cancelled" "quit / exit abort"))
      "\nPrzerwano."
      (strcat "\nBlad: " msg)
    )
  )
  (princ)
)

(defun gp-exp-save-file-dialog (out-format / wsh tmp-file shell-cmd file-handle result filter title ext)
  (if (= out-format "pts")
    (setq filter "Chmura punktow PTS (*.pts)|*.pts|Pliki tekstowe (*.txt)|*.txt|Wszystkie pliki (*.*)|*.*"
          title "Zapisz chmure punktow PTS"
          ext "pts")
    (setq filter "Pliki tekstowe (*.txt)|*.txt|Wszystkie pliki (*.*)|*.*"
          title "Zapisz pikiety TXT"
          ext "txt")
  )

  (setq wsh (vlax-create-object "WScript.Shell")
        tmp-file (vl-filename-mktemp "geoprofi_export_path.txt"))

  (setq shell-cmd
    (strcat
      "powershell.exe -WindowStyle Hidden -Command \"& {"
      "Add-Type -AssemblyName System.Windows.Forms;"
      "$d = New-Object System.Windows.Forms.SaveFileDialog;"
      "$d.Filter = '" filter "';"
      "$d.DefaultExt = '" ext "';"
      "$d.AddExtension = $true;"
      "$d.Title = '" title "';"
      "if($d.ShowDialog() -eq 'OK') { "
      "[System.IO.File]::WriteAllText('"
      (vl-string-translate "\\" "/" tmp-file)
      "', $d.FileName) }}\""
    )
  )

  (vlax-invoke-method wsh 'Run shell-cmd 0 :vlax-true)
  (vlax-release-object wsh)

  (if (findfile tmp-file)
    (progn
      (setq file-handle (open tmp-file "r"))
      (setq result (read-line file-handle))
      (close file-handle)
      (vl-file-delete tmp-file)
    )
  )
  result
)

;; ======================================================
;; UKLAD WSPOLRZEDNYCH
;; ======================================================

(defun gp-exp-detect-epsg (pt / x y result)
  (if (not pt)
    "Brak"
    (progn
      (setq x (car pt)
            y (cadr pt)
            result "Uklad Lokalny")
      (if (and (> y 4900000) (< y 6100000))
        (cond
          ((and (> x 5300000) (< x 5900000)) (setq result "Uklad 2000 (S5)"))
          ((and (> x 6300000) (< x 6900000)) (setq result "Uklad 2000 (S6)"))
          ((and (> x 7300000) (< x 7900000)) (setq result "Uklad 2000 (S7)"))
          ((and (> x 8300000) (< x 8900000)) (setq result "Uklad 2000 (S8)"))
        )
      )
      (if
        (and
          (= result "Uklad Lokalny")
          (> x 3000000) (< x 6000000)
          (> y 3000000) (< y 6000000)
        )
        (setq result "Uklad 1965 (?)")
      )
      result
    )
  )
)

(defun gp-exp-detect-record-system (records / count p1 p2 p3 r1 r2 r3)
  (setq count (length records))
  (if (= count 0)
    (list "Brak punktow" nil)
    (progn
      (setq p1 (gp-exp-record-get (nth 0 records) 'pt)
            p2 (gp-exp-record-get (nth (fix (/ count 2)) records) 'pt)
            p3 (gp-exp-record-get (car (last records)) 'pt)
            r1 (gp-exp-detect-epsg p1)
            r2 (gp-exp-detect-epsg p2)
            r3 (gp-exp-detect-epsg p3))
      (if (and (= r1 r2) (= r2 r3))
        (list r1 nil)
        (list "ALARM: NIEZGODNOSC UKLADOW!" T)
      )
    )
  )
)

;; ======================================================
;; WYCIAGANIE PUNKTOW Z OBIEKTOW
;; ======================================================

(defun gp-exp-point-from-variant (variant)
  (vlax-safearray->list (vlax-variant-value variant))
)

(defun gp-exp-extract-points (obj / type result start-p end-p i ename data)
  (setq type (gp-exp-safe-object-name obj)
        result '())

  (cond
    ((= type "AcDbPoint")
     (setq ename (vlax-vla-object->ename obj)
           data (entget ename))
     (if (assoc 10 data)
       (setq result
         (list
           (trans (cdr (assoc 10 data)) ename 0)
         )
       )
     )
    )

    ((= type "AcDbBlockReference")
     (setq result
       (list
         (gp-exp-point-from-variant (vla-get-InsertionPoint obj))
       )
     )
    )

    ((= type "AcDbCircle")
     (setq result
       (list
         (gp-exp-point-from-variant (vla-get-Center obj))
       )
     )
    )

    ((= type "AcDbLine")
     (setq result
       (list
         (vlax-curve-getStartPoint obj)
         (vlax-curve-getEndPoint obj)
       )
     )
    )

    ((= type "AcDbArc")
     (setq start-p (vlax-curve-getStartParam obj)
           end-p (vlax-curve-getEndParam obj))
     (setq result
       (list
         (vlax-curve-getPointAtParam obj start-p)
         (vlax-curve-getPointAtParam obj (+ start-p (/ (- end-p start-p) 2.0)))
         (vlax-curve-getPointAtParam obj end-p)
       )
     )
    )

    ((member type '("AcDbPolyline" "AcDb2dPolyline" "AcDb3dPolyline"))
     (setq start-p (fix (vlax-curve-getStartParam obj))
           end-p (fix (vlax-curve-getEndParam obj))
           i start-p)
     (while (<= i end-p)
       (setq result
         (cons (vlax-curve-getPointAtParam obj i) result)
       )
       (setq i (1+ i))
     )
     (setq result (reverse result))
    )

    ((= type "AcDbSolid")
     (setq ename (vlax-vla-object->ename obj)
           data (entget ename))
     (foreach code '(10 11 12 13)
       (if (assoc code data)
         (setq result
           (append result
             (list (trans (cdr (assoc code data)) ename 0))
           )
         )
       )
     )
    )
  )
  result
)

(defun gp-exp-kind-from-object-name (type)
  (cond
    ((= type "AcDbPoint") "POINT")
    ((= type "AcDbBlockReference") "INSERT")
    ((= type "AcDbLine") "LINE")
    ((member type '("AcDbPolyline" "AcDb2dPolyline" "AcDb3dPolyline")) "POLYLINE")
    ((= type "AcDbArc") "ARC")
    ((= type "AcDbCircle") "CIRCLE")
    ((= type "AcDbSolid") "SOLID")
    (T nil)
  )
)

;; ======================================================
;; TEKSTY ZEWNATRZNE - RADAR
;; ======================================================

(defun gp-exp-text-item-from-object (obj tid / min-pt max-pt result txt cat)
  (setq result
    (vl-catch-all-apply
      'vla-GetBoundingBox
      (list obj 'min-pt 'max-pt)
    )
  )
  (if (vl-catch-all-error-p result)
    nil
    (progn
      (setq txt (gp-exp-safe-text-string obj)
            cat (gp-exp-text-category txt))
      (list
        (vlax-safearray->list min-pt)
        (vlax-safearray->list max-pt)
        txt
        cat
        obj
        tid
      )
    )
  )
)

(defun gp-exp-text-item-id (item)
  (nth 5 item)
)

(defun gp-exp-text-item-object (item)
  (nth 4 item)
)

(defun gp-exp-text-item-value (item)
  (nth 2 item)
)

(defun gp-exp-nearest-text (pt items radius category / item dists best best-center)
  (setq best nil
        best-center nil)
  (foreach item items
    (if (= (nth 3 item) category)
      (progn
        (setq dists (geocad-text-radar-distance pt item))
        (if
          (and
            (<= (car dists) radius)
            (or (not best-center) (< (cadr dists) best-center))
          )
          (setq best item
                best-center (cadr dists))
        )
      )
    )
  )
  best
)

(defun gp-exp-near-text-count (pt items radius category / count item dists)
  (setq count 0)
  (foreach item items
    (if (= (nth 3 item) category)
      (progn
        (setq dists (geocad-text-radar-distance pt item))
        (if (<= (car dists) radius)
          (setq count (1+ count))
        )
      )
    )
  )
  count
)

;; ======================================================
;; ATRYBUTY BLOKOW
;; ======================================================

(defun gp-exp-value-to-list (value / raw)
  (cond
    ((vl-catch-all-error-p value) '())
    ((= (type value) 'VARIANT)
     (setq raw (vlax-variant-value value))
     (gp-exp-value-to-list raw)
    )
    ((= (type value) 'SAFEARRAY)
     (vlax-safearray->list value)
    )
    ((listp value) value)
    ((not value) '())
    (T (list value))
  )
)

(defun gp-exp-safe-invoke-list (obj method / result)
  (setq result
    (vl-catch-all-apply
      'vlax-invoke
      (list obj method)
    )
  )
  (gp-exp-value-to-list result)
)

(defun gp-exp-block-attribute-data (obj id-tags z-tags / refs ref tag txt id-value z-value)
  ;; Wynik:
  ;; ((id . "...") (z . 123.45))
  ;; Odczytuje atrybuty edytowalne i stale.
  (setq id-value nil
        z-value nil
        refs
          (append
            (gp-exp-safe-invoke-list obj 'GetAttributes)
            (gp-exp-safe-invoke-list obj 'GetConstantAttributes)
          )
  )

  (foreach ref refs
    (setq tag (gp-exp-safe-tag-string ref)
          txt (gp-exp-safe-text-string ref))

    (if
      (and
        (not id-value)
        (member tag id-tags)
        (gp-exp-nonempty-p txt)
      )
      (setq id-value (gp-exp-trim txt))
    )

    (if
      (and
        (not z-value)
        (member tag z-tags)
        (gp-exp-parse-number txt)
      )
      (setq z-value (gp-exp-parse-number txt))
    )
  )

  (list
    (cons 'id id-value)
    (cons 'z z-value)
  )
)

;; ======================================================
;; TEKSTY WEWNATRZ DEFINICJI BLOKU
;; ======================================================

(defun gp-exp-label-text-p (txt / upper)
  (setq upper (strcase (gp-exp-trim txt)))
  (member upper
    '(
      "NR" "NUMER" "ID" "PKT" "PUNKT"
      "H" "Z" "RZEDNA" "RZĘDNA"
      "WYS" "WYS." "WYSOKOSC" "WYSOKOŚĆ"
    )
  )
)

(defun gp-exp-collect-block-definition-texts
  (doc block-name visited depth / blocks block-def ent type txt nested-name result)
  (setq result '())

  (if
    (and
      doc
      block-name
      (< depth 5)
      (not (member (strcase block-name) visited))
    )
    (progn
      (setq blocks (vla-get-Blocks doc))
      (setq block-def
        (vl-catch-all-apply
          'vla-Item
          (list blocks block-name)
        )
      )

      (if (not (vl-catch-all-error-p block-def))
        (progn
          (setq visited (cons (strcase block-name) visited))

          (vlax-for ent block-def
            (setq type (gp-exp-safe-object-name ent))

            (cond
              ((member type '("AcDbText" "AcDbMText"))
               (setq txt (gp-exp-safe-text-string ent))
               (if (gp-exp-nonempty-p txt)
                 (setq result (cons (gp-exp-trim txt) result))
               )
              )

              ;; Rekurencja dla blokow zagniezdzonych.
              ((= type "AcDbBlockReference")
               (setq nested-name
                 (vl-catch-all-apply
                   'vla-get-Name
                   (list ent)
                 )
               )
               (if (not (vl-catch-all-error-p nested-name))
                 (setq result
                   (append
                     (gp-exp-collect-block-definition-texts
                       doc nested-name visited (1+ depth)
                     )
                     result
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
  (reverse result)
)

(defun gp-exp-block-internal-text-data (obj / name cache-pair doc texts txt z id numeric)
  ;; Wynik:
  ;; ((id . "...") (z . 123.45))
  ;;
  ;; Analizuje TEXT/MTEXT zapisane w definicji bloku bez EXPLODE.
  ;; Wynik jest cache'owany po nazwie rzeczywistej definicji bloku.
  (setq name
    (vl-catch-all-apply
      'vla-get-Name
      (list obj)
    )
  )

  (if (vl-catch-all-error-p name)
    (list (cons 'id nil) (cons 'z nil))
    (progn
      (setq cache-pair (assoc name *gp-exp-block-text-cache*))
      (if cache-pair
        (cdr cache-pair)
        (progn
          (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
                texts (gp-exp-collect-block-definition-texts doc name '() 0)
                id nil
                z nil)

          ;; Najpierw szukamy rzędnej: tekst sklasyfikowany jako Z.
          (foreach txt texts
            (if
              (and
                (not z)
                (= (gp-exp-text-category txt) "Z")
                (gp-exp-parse-number txt)
              )
              (setq z (gp-exp-parse-number txt))
            )
          )

          ;; ID: preferuj tekst z cyfra, ale nie etykiete typu "NR".
          (foreach txt texts
            (if
              (and
                (not id)
                (= (gp-exp-text-category txt) "ID")
                (not (gp-exp-label-text-p txt))
                (gp-exp-has-digit-p txt)
              )
              (setq id txt)
            )
          )

          ;; Awaryjnie dowolny niepusty tekst ID niebedacy etykieta.
          (if (not id)
            (foreach txt texts
              (if
                (and
                  (not id)
                  (= (gp-exp-text-category txt) "ID")
                  (not (gp-exp-label-text-p txt))
                )
                (setq id txt)
              )
            )
          )

          (setq cache-pair
            (list
              (cons 'id id)
              (cons 'z z)
            )
          )
          (setq *gp-exp-block-text-cache*
            (cons (cons name cache-pair) *gp-exp-block-text-cache*)
          )
          cache-pair
        )
      )
    )
  )
)

;; ======================================================
;; KOLEKCJA OBIEKTOW I REKORDOW EKSPORTU
;; ======================================================

(defun gp-exp-make-record (rid kind pt obj subindex)
  (list
    (cons 'rid rid)
    (cons 'kind kind)
    (cons 'pt pt)
    (cons 'obj obj)
    (cons 'handle (gp-exp-safe-handle obj))
    (cons 'subindex subindex)
  )
)

(defun gp-exp-collect-selection
  (ss / i ent obj type kind points p subindex rid tid item records texts object-count point-count)
  ;; Wynik:
  ;; (records texts object-count point-count)
  (setq i 0
        rid 1
        tid 1
        records '()
        texts '()
        object-count '()
        point-count '())

  (while (< i (sslength ss))
    (setq ent (ssname ss i)
          obj (vlax-ename->vla-object ent)
          type (gp-exp-safe-object-name obj))

    (cond
      ((member type '("AcDbText" "AcDbMText"))
       (setq item (gp-exp-text-item-from-object obj tid))
       (if item
         (progn
           (setq texts (cons item texts))
           (setq tid (1+ tid))
         )
       )
      )

      ((setq kind (gp-exp-kind-from-object-name type))
       (setq object-count (gp-exp-list-inc object-count kind))
       (setq points (gp-exp-extract-points obj)
             subindex 1)

       (foreach p points
         (setq records
           (cons
             (gp-exp-make-record rid kind p obj subindex)
             records
           )
         )
         (setq point-count (gp-exp-list-inc point-count kind)
               rid (1+ rid)
               subindex (1+ subindex))
       )
      )
    )
    (setq i (1+ i))
  )

  (list
    (reverse records)
    (reverse texts)
    object-count
    point-count
  )
)

;; ======================================================
;; FILTROWANIE TYPOW I DUPLIKATOW
;; ======================================================

(defun gp-exp-kind-enabled-p (kind enabled-kinds)
  (member kind enabled-kinds)
)

(defun gp-exp-filter-records (records enabled-kinds solid-mode / result record kind subindex)
  (setq result '())
  (foreach record records
    (setq kind (gp-exp-record-get record 'kind)
          subindex (gp-exp-record-get record 'subindex))
    (if
      (and
        (gp-exp-kind-enabled-p kind enabled-kinds)
        (or
          (/= kind "SOLID")
          (= solid-mode "4")
          (= subindex 1)
        )
      )
      (setq result (cons record result))
    )
  )
  (reverse result)
)

(defun gp-exp-remove-geometry-duplicates (records mode tolerance / accepted result record pt duplicate)
  (if (/= mode "rem")
    records
    (progn
      (setq accepted '()
            result '())
      (foreach record records
        (setq pt (gp-exp-record-get record 'pt)
              duplicate
                (vl-some
                  '(lambda (other) (< (gp-exp-dist-2d pt other) tolerance))
                  accepted
                ))
        (if (not duplicate)
          (progn
            (setq accepted (cons pt accepted))
            (setq result (cons record result))
          )
        )
      )
      (reverse result)
    )
  )
)

;; ======================================================
;; DANE BAZOWE ID / Z
;; ======================================================

(defun gp-exp-base-data-for-record (record id-tags z-tags / kind obj pt attrs inside id z id-source z-source)
  (setq kind (gp-exp-record-get record 'kind)
        obj (gp-exp-record-get record 'obj)
        pt (gp-exp-record-get record 'pt)
        id nil
        z nil
        id-source nil
        z-source nil)

  (if (= kind "INSERT")
    (progn
      (setq attrs (gp-exp-block-attribute-data obj id-tags z-tags)
            inside (gp-exp-block-internal-text-data obj))

      ;; ID: atrybut -> tekst w definicji.
      (cond
        ((cdr (assoc 'id attrs))
         (setq id (cdr (assoc 'id attrs))
               id-source "ATTR")
        )
        ((cdr (assoc 'id inside))
         (setq id (cdr (assoc 'id inside))
               id-source "BLOCK_TEXT")
        )
      )

      ;; Z: atrybut -> geometria -> tekst w definicji.
      (cond
        ((cdr (assoc 'z attrs))
         (setq z (cdr (assoc 'z attrs))
               z-source "ATTR")
        )
        ((and (caddr pt) (> (abs (caddr pt)) 0.001))
         (setq z (caddr pt)
               z-source "GEOM")
        )
        ((cdr (assoc 'z inside))
         (setq z (cdr (assoc 'z inside))
               z-source "BLOCK_TEXT")
        )
      )
    )

    ;; Pozostale typy: Z z geometrii, ID brak.
    (if (and (caddr pt) (> (abs (caddr pt)) 0.001))
      (setq z (caddr pt)
            z-source "GEOM")
    )
  )

  (list
    (cons 'id id)
    (cons 'id-source id-source)
    (cons 'z z)
    (cons 'z-source z-source)
  )
)

;; ======================================================
;; GLOBALNE DOPASOWANIE TEKSTOW 1:1
;; ======================================================

(defun gp-exp-candidate-less-p (a b)
  ;; Kandydat:
  ;; (d-center d-edge rid tid item)
  (if (= (car a) (car b))
    (< (cadr a) (cadr b))
    (< (car a) (car b))
  )
)

(defun gp-exp-build-radar-candidates
  (records texts radius category base-map z-mode / result record rid pt base item dists allow)
  (setq result '())

  (foreach record records
    (setq rid (gp-exp-record-get record 'rid)
          pt (gp-exp-record-get record 'pt)
          base (gp-exp-map-get base-map rid)
          allow nil)

    (if (= category "ID")
      (setq allow (not (cdr (assoc 'id base))))
      (setq allow
        (or
          (= z-mode "z_text")
          (not (cdr (assoc 'z base)))
        )
      )
    )

    (if allow
      (foreach item texts
        (if (= (nth 3 item) category)
          (progn
            (setq dists (geocad-text-radar-distance pt item))
            (if (<= (car dists) radius)
              (setq result
                (cons
                  (list
                    (cadr dists)
                    (car dists)
                    rid
                    (gp-exp-text-item-id item)
                    item
                  )
                  result
                )
              )
            )
          )
        )
      )
    )
  )

  (vl-sort result 'gp-exp-candidate-less-p)
)

(defun gp-exp-assign-radar-unique
  (records texts radius category base-map z-mode / candidates assigned-rids used-tids result candidate rid tid item value)
  (setq candidates
    (gp-exp-build-radar-candidates records texts radius category base-map z-mode)
  )
  (setq assigned-rids '()
        used-tids '()
        result '())

  (foreach candidate candidates
    (setq rid (nth 2 candidate)
          tid (nth 3 candidate)
          item (nth 4 candidate))

    (if
      (and
        (not (member rid assigned-rids))
        (not (member tid used-tids))
      )
      (progn
        (setq value
          (if (= category "Z")
            (gp-exp-parse-number (gp-exp-text-item-value item))
            (gp-exp-trim (gp-exp-text-item-value item))
          )
        )

        (if value
          (progn
            (setq result
              (cons
                (cons rid
                  (list
                    (cons 'value value)
                    (cons 'source "RADAR")
                    (cons 'text-object (gp-exp-text-item-object item))
                  )
                )
                result
              )
            )
            (setq assigned-rids (cons rid assigned-rids)
                  used-tids (cons tid used-tids))
          )
        )
      )
    )
  )
  result
)

(defun gp-exp-assign-radar-reusable
  (records texts radius category base-map z-mode / result record rid pt base item value)
  (setq result '())
  (foreach record records
    (setq rid (gp-exp-record-get record 'rid)
          pt (gp-exp-record-get record 'pt)
          base (gp-exp-map-get base-map rid))

    (if
      (if (= category "ID")
        (not (cdr (assoc 'id base)))
        (or (= z-mode "z_text") (not (cdr (assoc 'z base))))
      )
      (progn
        (setq item (gp-exp-nearest-text pt texts radius category))
        (if item
          (progn
            (setq value
              (if (= category "Z")
                (gp-exp-parse-number (gp-exp-text-item-value item))
                (gp-exp-trim (gp-exp-text-item-value item))
              )
            )
            (if value
              (setq result
                (cons
                  (cons rid
                    (list
                      (cons 'value value)
                      (cons 'source "RADAR")
                      (cons 'text-object (gp-exp-text-item-object item))
                    )
                  )
                  result
                )
              )
            )
          )
        )
      )
    )
  )
  result
)

;; ======================================================
;; ROZWIAZANIE ID / Z I STATYSTYKI
;; ======================================================

(defun gp-exp-auto-id (prefix number)
  (strcat prefix (itoa number))
)

(defun gp-exp-resolve-records
  (
    records texts radius id-tags z-tags unique-texts z-mode
    renum-all fix-dupes auto-prefix auto-start
    /
    base-map record rid base
    radar-id-map radar-z-map
    final-id-map final-z-map
    used-ids next-auto raw-id raw-source radar-entry
    raw-z z-source z-entry id-value
    conflicts conflict-count pt obj nearest-z
  )

  ;; 1. Dane bazowe dla kazdego rekordu.
  (setq base-map '())
  (foreach record records
    (setq rid (gp-exp-record-get record 'rid))
    (setq base-map
      (cons
        (cons rid (gp-exp-base-data-for-record record id-tags z-tags))
        base-map
      )
    )
  )

  ;; 2. Radar ID i Z.
  (if unique-texts
    (progn
      (setq radar-id-map
        (gp-exp-assign-radar-unique records texts radius "ID" base-map z-mode)
      )
      (setq radar-z-map
        (gp-exp-assign-radar-unique records texts radius "Z" base-map z-mode)
      )
    )
    (progn
      (setq radar-id-map
        (gp-exp-assign-radar-reusable records texts radius "ID" base-map z-mode)
      )
      (setq radar-z-map
        (gp-exp-assign-radar-reusable records texts radius "Z" base-map z-mode)
      )
    )
  )

  ;; 3. Finalne ID: baza/radar -> auto -> naprawa duplikatow.
  (setq final-id-map '()
        used-ids '()
        next-auto auto-start)

  (foreach record records
    (setq rid (gp-exp-record-get record 'rid)
          base (gp-exp-map-get base-map rid)
          radar-entry (gp-exp-map-get radar-id-map rid)
          raw-id nil
          raw-source nil)

    (if (= renum-all "1")
      (setq raw-id nil)
      (cond
        ((cdr (assoc 'id base))
         (setq raw-id (cdr (assoc 'id base))
               raw-source (cdr (assoc 'id-source base)))
        )
        (radar-entry
         (setq raw-id (cdr (assoc 'value radar-entry))
               raw-source "RADAR")
        )
      )
    )

    (if
      (or
        (not (gp-exp-nonempty-p raw-id))
        (and (= fix-dupes "1") (member raw-id used-ids))
      )
      (progn
        (setq id-value (gp-exp-auto-id auto-prefix next-auto))
        (setq next-auto (1+ next-auto))
        (while (member id-value used-ids)
          (setq id-value (gp-exp-auto-id auto-prefix next-auto))
          (setq next-auto (1+ next-auto))
        )
        (setq raw-id id-value
              raw-source "AUTO")
      )
    )

    (setq used-ids (cons raw-id used-ids))
    (setq final-id-map
      (cons
        (cons rid
          (list
            (cons 'value raw-id)
            (cons 'source raw-source)
            (cons 'text-object
              (if radar-entry (cdr (assoc 'text-object radar-entry)) nil)
            )
          )
        )
        final-id-map
      )
    )
  )

  ;; 4. Finalne Z.
  (setq final-z-map '())
  (foreach record records
    (setq rid (gp-exp-record-get record 'rid)
          base (gp-exp-map-get base-map rid)
          z-entry (gp-exp-map-get radar-z-map rid)
          raw-z nil
          z-source nil)

    (cond
      ((and (= z-mode "z_text") z-entry)
       (setq raw-z (cdr (assoc 'value z-entry))
             z-source "RADAR")
      )
      ((cdr (assoc 'z base))
       (setq raw-z (cdr (assoc 'z base))
             z-source (cdr (assoc 'z-source base)))
      )
      (z-entry
       (setq raw-z (cdr (assoc 'value z-entry))
             z-source "RADAR")
      )
      (T
       (setq raw-z 0.0
             z-source "ZERO")
      )
    )

    (setq final-z-map
      (cons
        (cons rid
          (list
            (cons 'value raw-z)
            (cons 'source z-source)
            (cons 'text-object
              (if z-entry (cdr (assoc 'text-object z-entry)) nil)
            )
          )
        )
        final-z-map
      )
    )
  )

  ;; 5. Konflikty Z do raportu.
  (setq conflicts '())
  (foreach record records
    (setq rid (gp-exp-record-get record 'rid)
          pt (gp-exp-record-get record 'pt)
          obj (gp-exp-record-get record 'obj)
          base (gp-exp-map-get base-map rid)
          conflict-count (gp-exp-near-text-count pt texts radius "Z")
          nearest-z (gp-exp-nearest-text pt texts radius "Z"))

    (if
      (and
        nearest-z
        (or
          (cdr (assoc 'z base))
          (> conflict-count 1)
        )
      )
      (setq conflicts
        (cons
          (list
            pt
            (strcat
              (gp-exp-kind-label (gp-exp-record-get record 'kind))
              " | X=" (rtos (car pt) 2 3)
              " Y=" (rtos (cadr pt) 2 3)
              " | Z obiektu="
              (if (cdr (assoc 'z base))
                (rtos (cdr (assoc 'z base)) 2 3)
                "brak"
              )
              " | najblizszy tekst="
              (gp-exp-text-item-value nearest-z)
              " | tekstow Z=" (itoa conflict-count)
            )
            obj
            (gp-exp-text-item-object nearest-z)
          )
          conflicts
        )
      )
    )
  )

  (list
    (cons 'records records)
    (cons 'base-map base-map)
    (cons 'id-map final-id-map)
    (cons 'z-map final-z-map)
    (cons 'conflicts (reverse conflicts))
    (cons 'next-auto next-auto)
  )
)

(defun gp-exp-count-source-for-kind (records map kind source / count record rid entry)
  (setq count 0)
  (foreach record records
    (if (= (gp-exp-record-get record 'kind) kind)
      (progn
        (setq rid (gp-exp-record-get record 'rid)
              entry (gp-exp-map-get map rid))
        (if (= (cdr (assoc 'source entry)) source)
          (setq count (1+ count))
        )
      )
    )
  )
  count
)

(defun gp-exp-record-count-kind (records kind / count record)
  (setq count 0)
  (foreach record records
    (if (= (gp-exp-record-get record 'kind) kind)
      (setq count (1+ count))
    )
  )
  count
)

(defun gp-exp-object-count-kind (records kind / handles record handle)
  (setq handles '())
  (foreach record records
    (if (= (gp-exp-record-get record 'kind) kind)
      (progn
        (setq handle (gp-exp-record-get record 'handle))
        (if (not (member handle handles))
          (setq handles (cons handle handles))
        )
      )
    )
  )
  (length handles)
)

(defun gp-exp-stat-line (records id-map z-map kind / obj-count point-count ia ib ir iauto za zb zg zr zz)
  (setq obj-count (gp-exp-object-count-kind records kind)
        point-count (gp-exp-record-count-kind records kind)

        ia (gp-exp-count-source-for-kind records id-map kind "ATTR")
        ib (gp-exp-count-source-for-kind records id-map kind "BLOCK_TEXT")
        ir (gp-exp-count-source-for-kind records id-map kind "RADAR")
        iauto (gp-exp-count-source-for-kind records id-map kind "AUTO")

        za (gp-exp-count-source-for-kind records z-map kind "ATTR")
        zb (gp-exp-count-source-for-kind records z-map kind "BLOCK_TEXT")
        zg (gp-exp-count-source-for-kind records z-map kind "GEOM")
        zr (gp-exp-count-source-for-kind records z-map kind "RADAR")
        zz (gp-exp-count-source-for-kind records z-map kind "ZERO")
  )

  (strcat
    (gp-exp-kind-label kind)
    " | obiekty=" (itoa obj-count)
    " punkty=" (itoa point-count)
    " | ID: attr=" (itoa ia)
    " blok=" (itoa ib)
    " tekst=" (itoa ir)
    " auto=" (itoa iauto)
    " | Z: attr=" (itoa za)
    " blok=" (itoa zb)
    " geom=" (itoa zg)
    " tekst=" (itoa zr)
    " zero=" (itoa zz)
  )
)

(defun gp-exp-total-source-summary (records id-map z-map / ia ib ir iauto za zb zg zr zz kind)
  (setq ia (gp-exp-count-source-for-kind records id-map "INSERT" "ATTR")
        ib (gp-exp-count-source-for-kind records id-map "INSERT" "BLOCK_TEXT")
        ir 0
        iauto 0
        za (gp-exp-count-source-for-kind records z-map "INSERT" "ATTR")
        zb (gp-exp-count-source-for-kind records z-map "INSERT" "BLOCK_TEXT")
        zg 0
        zr 0
        zz 0)

  (foreach kind '("POINT" "INSERT" "LINE" "POLYLINE" "ARC" "CIRCLE" "SOLID")
    (setq ir (+ ir (gp-exp-count-source-for-kind records id-map kind "RADAR"))
          iauto (+ iauto (gp-exp-count-source-for-kind records id-map kind "AUTO"))
          zg (+ zg (gp-exp-count-source-for-kind records z-map kind "GEOM"))
          zr (+ zr (gp-exp-count-source-for-kind records z-map kind "RADAR"))
          zz (+ zz (gp-exp-count-source-for-kind records z-map kind "ZERO")))
  )

  (strcat
    "Razem punktow: " (itoa (length records))
    " | ID: atrybut=" (itoa ia)
    " tekst-bloku=" (itoa ib)
    " tekst-obok=" (itoa ir)
    " auto=" (itoa iauto)
    " | Z: atrybut=" (itoa za)
    " tekst-bloku=" (itoa zb)
    " geometria=" (itoa zg)
    " tekst-obok=" (itoa zr)
    " zero=" (itoa zz)
  )
)

;; ======================================================
;; PODGLAD KONFLIKTOW
;; ======================================================

(defun gp-exp-show-point (pt / acad margin p1 p2)
  (if pt
    (progn
      (setq acad (vlax-get-acad-object)
            margin 5.0
            p1 (list (- (car pt) margin) (- (cadr pt) margin) 0.0)
            p2 (list (+ (car pt) margin) (+ (cadr pt) margin) 0.0))
      (vl-catch-all-apply
        'vla-ZoomWindow
        (list acad (vlax-3d-point p1) (vlax-3d-point p2))
      )
    )
  )
)

(defun gp-exp-show-conflict (conflict / pt source-object text-object ss ename obj)
  (if conflict
    (progn
      (setq pt (nth 0 conflict)
            source-object (nth 2 conflict)
            text-object (nth 3 conflict)
            ss (ssadd))

      (foreach obj (list source-object text-object)
        (if obj
          (progn
            (setq ename
              (vl-catch-all-apply
                'vlax-vla-object->ename
                (list obj)
              )
            )
            (if
              (and
                (not (vl-catch-all-error-p ename))
                ename
                (entget ename)
              )
              (ssadd ename ss)
            )
          )
        )
      )

      (if (> (sslength ss) 0)
        (sssetfirst nil ss)
      )
      (gp-exp-show-point pt)
    )
  )
)

;; ======================================================
;; DCL
;; ======================================================

(defun gp-exp-write-type-toggle (file key label object-count point-count)
  (write-line
    (strcat
      "      : toggle { key = \"" key "\"; label = \""
      label
      " | obiekty=" (itoa object-count)
      " punkty=" (itoa point-count)
      "\"; value = \"" (if (> point-count 0) "1" "0") "\";"
      (if (> point-count 0) "" " is_enabled = false;")
      " }"
    )
    file
  )
)

(defun gp-exp-build-dcl (object-count point-count / path file)
  (setq path (vl-filename-mktemp "geoprofi_export_v23.dcl")
        file (open path "w"))

  (write-line "GeoExportV23 : dialog {" file)
  (write-line "  label = \"Eksport pikiet V23 - kontrola typow i zrodel danych\";" file)
  (write-line "  : row {" file)

  ;; LEWA KOLUMNA
  (write-line "    : column {" file)

  (write-line "      : boxed_column { label = \"Typy obiektow do eksportu\";" file)
  (gp-exp-write-type-toggle file "use_point" "Natywne POINT"
    (gp-exp-count-get object-count "POINT")
    (gp-exp-count-get point-count "POINT"))
  (gp-exp-write-type-toggle file "use_insert" "Bloki INSERT"
    (gp-exp-count-get object-count "INSERT")
    (gp-exp-count-get point-count "INSERT"))
  (gp-exp-write-type-toggle file "use_line" "Linie"
    (gp-exp-count-get object-count "LINE")
    (gp-exp-count-get point-count "LINE"))
  (gp-exp-write-type-toggle file "use_polyline" "Polilinie"
    (gp-exp-count-get object-count "POLYLINE")
    (gp-exp-count-get point-count "POLYLINE"))
  (gp-exp-write-type-toggle file "use_arc" "Luki"
    (gp-exp-count-get object-count "ARC")
    (gp-exp-count-get point-count "ARC"))
  (gp-exp-write-type-toggle file "use_circle" "Okregi - srodek"
    (gp-exp-count-get object-count "CIRCLE")
    (gp-exp-count-get point-count "CIRCLE"))
  (gp-exp-write-type-toggle file "use_solid" "SOLID"
    (gp-exp-count-get object-count "SOLID")
    (gp-exp-count-get point-count "SOLID"))
  (write-line "      }" file)

  (write-line "      : boxed_column { label = \"Radar tekstow i bloki\";" file)
  (write-line "        : row { : edit_box { key = \"t_r\"; label = \"Promien [m]:\"; edit_width = 8; value = \"1.5\"; }" file)
  (write-line "                : toggle { key = \"unique_texts\"; label = \"Jeden tekst tylko raz\"; value = \"1\"; } }" file)
  (write-line "        : row { : edit_box { key = \"b_t\"; label = \"Tagi ID:\"; edit_width = 18; value = \"NR, ID\"; }" file)
  (write-line "                : edit_box { key = \"z_t\"; label = \"Tagi Z:\"; edit_width = 18; value = \"H, Z, RZEDNA\"; } }" file)
  (write-line "        : text { label = \"Dla INSERT: atrybut -> geometria -> tekst w bloku -> tekst obok.\"; }" file)
  (write-line "      }" file)

  (write-line "      : boxed_column { label = \"Duplikaty i numeracja\";" file)
  (write-line "        : radio_row { key = \"d_m\";" file)
  (write-line "          : radio_button { key = \"rem\"; label = \"Usun duplikaty XY\"; value = \"1\"; }" file)
  (write-line "          : radio_button { key = \"keep\"; label = \"Zostaw wszystkie\"; }" file)
  (write-line "        }" file)
  (write-line "        : edit_box { key = \"d_tol\"; label = \"Tolerancja XY [m]:\"; edit_width = 8; value = \"0.01\"; }" file)
  (write-line "        : toggle { key = \"renum_all\"; label = \"Nowa numeracja wszystkich\"; value = \"0\"; }" file)
  (write-line "        : toggle { key = \"fix_dupes\"; label = \"Napraw duplikaty ID\"; value = \"1\"; }" file)
  (write-line "        : row { : edit_box { key = \"a_p\"; label = \"Prefiks:\"; edit_width = 10; value = \"P_\"; }" file)
  (write-line "                : edit_box { key = \"a_s\"; label = \"Start:\"; edit_width = 8; value = \"1\"; } }" file)
  (write-line "      }" file)

  (write-line
    (strcat
      "      : boxed_radio_row { label = \"SOLID\"; key = \"s_m\";"
      (if (> (gp-exp-count-get point-count "SOLID") 0)
        ""
        " is_enabled = false;"
      )
    )
    file
  )
  (write-line "        : radio_button { key = \"1\"; label = \"Tylko P1\"; value = \"1\"; }" file)
  (write-line "        : radio_button { key = \"4\"; label = \"P1-P4\"; }" file)
  (write-line "      }" file)

  (write-line "      : boxed_radio_row { label = \"Konflikt Z obiekt / tekst\"; key = \"z_conf_mode\";" file)
  (write-line "        : radio_button { key = \"z_keep\"; label = \"Zostaw Z obiektu\"; value = \"1\"; }" file)
  (write-line "        : radio_button { key = \"z_text\"; label = \"Nadpisz tekstem\"; }" file)
  (write-line "      }" file)

  (write-line "      : boxed_column { label = \"Plik wynikowy\";" file)
  (write-line "        : boxed_radio_row { label = \"Format\"; key = \"out_fmt\";" file)
  (write-line "          : radio_button { key = \"txt\"; label = \"TXT pikiety\"; value = \"1\"; }" file)
  (write-line "          : radio_button { key = \"pts\"; label = \"PTS chmura\"; }" file)
  (write-line "        }" file)
  (write-line "        : boxed_radio_row { label = \"Kolumny\"; key = \"g_m\";" file)
  (write-line "          : radio_button { key = \"geo\"; label = \"Geodezja N,E,H\"; value = \"1\"; }" file)
  (write-line "          : radio_button { key = \"cad\"; label = \"CAD E,N,H\"; }" file)
  (write-line "        }" file)
  (write-line "        : edit_box { key = \"z_off\"; label = \"Offset Z [m]:\"; edit_width = 10; value = \"0.000\"; }" file)
  (write-line "      }" file)

  (write-line "    }" file)

  ;; PRAWA KOLUMNA
  (write-line "    : column {" file)
  (write-line "      : boxed_column { label = \"Raport wedlug typu i zrodla danych\";" file)
  (write-line "        : list_box { key = \"type_stats\"; width = 116; height = 15; }" file)
  (write-line "        : text { key = \"summary\"; value = \"...\"; width = 116; }" file)
  (write-line "        : text { key = \"sys_info\"; value = \"...\"; width = 116; }" file)
  (write-line "        : button { key = \"recalc\"; label = \"Odswiez raport\"; }" file)
  (write-line "      }" file)

  (write-line "      : boxed_column { label = \"Konflikty Z\";" file)
  (write-line "        : text { label = \"Wlasne Z i tekst Z obok albo kilka tekstow Z przy jednym punkcie.\"; }" file)
  (write-line "        : list_box { key = \"z_conflicts\"; width = 116; height = 11; }" file)
  (write-line "        : button { key = \"zoom_conflict\"; label = \"Pokaz miejsce\"; }" file)
  (write-line "      }" file)
  (write-line "    }" file)

  (write-line "  }" file)
  (write-line "  ok_cancel;" file)
  (write-line "}" file)

  (close file)
  path
)

;; ======================================================
;; ODCZYT USTAWIEN DCL
;; ======================================================

(defun gp-exp-enabled-kinds-from-dcl (/ result kind tile)
  (setq result '())
  (foreach kind '("POINT" "INSERT" "LINE" "POLYLINE" "ARC" "CIRCLE" "SOLID")
    (setq tile (gp-exp-kind-tile kind))
    (if (= (get_tile tile) "1")
      (setq result (cons kind result))
    )
  )
  (reverse result)
)

(defun gp-exp-read-live-options (/ radius tolerance auto-start enabled)
  (setq radius (atof (get_tile "t_r")))
  (if (<= radius 0.0) (setq radius 1.5))

  (setq tolerance (atof (get_tile "d_tol")))
  (if (< tolerance 0.0) (setq tolerance 0.01))

  (setq auto-start (atoi (get_tile "a_s")))
  (if (<= auto-start 0) (setq auto-start 1))

  (list
    (cons 'enabled-kinds (gp-exp-enabled-kinds-from-dcl))
    (cons 'radius radius)
    (cons 'unique-texts (= (get_tile "unique_texts") "1"))
    (cons 'id-tags (gp-exp-parse-tags (get_tile "b_t")))
    (cons 'z-tags (gp-exp-parse-tags (get_tile "z_t")))
    (cons 'duplicate-mode (get_tile "d_m"))
    (cons 'duplicate-tolerance tolerance)
    (cons 'solid-mode (if (get_tile "s_m") (get_tile "s_m") "1"))
    (cons 'z-mode (get_tile "z_conf_mode"))
    (cons 'renum-all (get_tile "renum_all"))
    (cons 'fix-dupes (get_tile "fix_dupes"))
    (cons 'auto-prefix (get_tile "a_p"))
    (cons 'auto-start auto-start)
    (cons 'out-format (get_tile "out_fmt"))
    (cons 'geo-mode (get_tile "g_m"))
    (cons 'z-offset
      (if (distof (vl-string-translate "," "." (get_tile "z_off")))
        (distof (vl-string-translate "," "." (get_tile "z_off")))
        0.0
      )
    )
  )
)

(defun gp-exp-option (options key)
  (cdr (assoc key options))
)

;; ======================================================
;; URUCHOMIENIE ANALIZY DLA UI LUB EKSPORTU
;; ======================================================

(defun gp-exp-run-resolution (all-records texts options / records)
  (setq records
    (gp-exp-filter-records
      all-records
      (gp-exp-option options 'enabled-kinds)
      (gp-exp-option options 'solid-mode)
    )
  )

  (setq records
    (gp-exp-remove-geometry-duplicates
      records
      (gp-exp-option options 'duplicate-mode)
      (gp-exp-option options 'duplicate-tolerance)
    )
  )

  (gp-exp-resolve-records
    records
    texts
    (gp-exp-option options 'radius)
    (gp-exp-option options 'id-tags)
    (gp-exp-option options 'z-tags)
    (gp-exp-option options 'unique-texts)
    (gp-exp-option options 'z-mode)
    (gp-exp-option options 'renum-all)
    (gp-exp-option options 'fix-dupes)
    (gp-exp-option options 'auto-prefix)
    (gp-exp-option options 'auto-start)
  )
)

(defun gp-exp-update-report-ui (resolution / records id-map z-map conflicts systems kind line)
  (setq records (cdr (assoc 'records resolution))
        id-map (cdr (assoc 'id-map resolution))
        z-map (cdr (assoc 'z-map resolution))
        conflicts (cdr (assoc 'conflicts resolution))
        systems (gp-exp-detect-record-system records))

  (start_list "type_stats")
  (if records
    (foreach kind '("POINT" "INSERT" "LINE" "POLYLINE" "ARC" "CIRCLE" "SOLID")
      (if (> (gp-exp-record-count-kind records kind) 0)
        (add_list (gp-exp-stat-line records id-map z-map kind))
      )
    )
    (add_list "Brak wlaczonych punktow do eksportu.")
  )
  (end_list)

  (set_tile "summary" (gp-exp-total-source-summary records id-map z-map))
  (set_tile "sys_info"
    (strcat
      "Uklad: " (car systems)
      (if (cadr systems) " | UWAGA: niespojne wspolrzedne" "")
    )
  )

  (start_list "z_conflicts")
  (if conflicts
    (foreach line conflicts
      (add_list (cadr line))
    )
    (add_list "Brak konfliktow Z dla aktualnych ustawien.")
  )
  (end_list)
  (set_tile "z_conflicts" "0")
  resolution
)

;; ======================================================
;; ZAPIS WYNIKU
;; ======================================================

(defun gp-exp-build-output-lines (resolution options / records id-map z-map format geo-mode offset lines record rid pt id-entry z-entry id z x y line)
  (setq records (cdr (assoc 'records resolution))
        id-map (cdr (assoc 'id-map resolution))
        z-map (cdr (assoc 'z-map resolution))
        format (gp-exp-option options 'out-format)
        geo-mode (gp-exp-option options 'geo-mode)
        offset (gp-exp-option options 'z-offset)
        lines '())

  (foreach record records
    (setq rid (gp-exp-record-get record 'rid)
          pt (gp-exp-record-get record 'pt)
          id-entry (gp-exp-map-get id-map rid)
          z-entry (gp-exp-map-get z-map rid)
          id (cdr (assoc 'value id-entry))
          z (+ (cdr (assoc 'value z-entry)) offset)
          x (car pt)
          y (cadr pt))

    (if (= format "pts")
      (if (= geo-mode "geo")
        (setq line
          (strcat
            (gp-exp-format-coord y) " "
            (gp-exp-format-coord x) " "
            (gp-exp-format-coord z)
          )
        )
        (setq line
          (strcat
            (gp-exp-format-coord x) " "
            (gp-exp-format-coord y) " "
            (gp-exp-format-coord z)
          )
        )
      )
      (if (= geo-mode "geo")
        (setq line
          (strcat
            id " "
            (gp-exp-format-coord y) " "
            (gp-exp-format-coord x) " "
            (gp-exp-format-coord z)
          )
        )
        (setq line
          (strcat
            id " "
            (gp-exp-format-coord x) " "
            (gp-exp-format-coord y) " "
            (gp-exp-format-coord z)
          )
        )
      )
    )

    (setq lines (cons line lines))
  )
  (reverse lines)
)

(defun gp-exp-write-output (filename lines format / file line)
  (setq file (open filename "w"))
  (if (= format "pts")
    (write-line (itoa (length lines)) file)
  )
  (foreach line lines
    (write-line line file)
  )
  (close file)
  (length lines)
)

;; ======================================================
;; GLOWNA KOMENDA
;; ======================================================

(defun c:EKSPORT_PIKIET_V23
  (
    /
    old-err f dcl-id dcl-file
    ss collected all-records texts object-count point-count
    run-analysis options resolution conflict-items conflict-index
    status filename output-lines output-count systems
  )

  (setq old-err *error*
        *error* gp-exp-error
        f nil
        dcl-id nil
        dcl-file nil
        *gp-exp-block-text-cache* '())

  (setq ss
    (ssget
      '(
        (0 . "POINT,INSERT,TEXT,MTEXT,LINE,LWPOLYLINE,POLYLINE,SOLID,ARC,CIRCLE")
      )
    )
  )

  (if (not ss)
    (progn
      (setq *error* old-err)
      (princ "\nNie wybrano obiektow.")
      (princ)
    )
    (progn
      (princ "\nAnaliza geometrii i tekstow...")

      (setq collected (gp-exp-collect-selection ss)
            all-records (nth 0 collected)
            texts (nth 1 collected)
            object-count (nth 2 collected)
            point-count (nth 3 collected))

      (setq dcl-file (gp-exp-build-dcl object-count point-count)
            dcl-id (load_dialog dcl-file))

      (if (not (new_dialog "GeoExportV23" dcl-id))
        (progn
          (alert "Nie udalo sie otworzyc dialogu eksportu.")
          (unload_dialog dcl-id)
          (vl-file-delete dcl-file)
          (setq *error* old-err)
        )
        (progn
          (setq conflict-index 0)

          (setq run-analysis
            (lambda (/ current-options current-resolution)
              (setq current-options (gp-exp-read-live-options))
              (setq current-resolution
                (gp-exp-run-resolution all-records texts current-options)
              )
              (setq resolution
                (gp-exp-update-report-ui current-resolution)
              )
              (setq conflict-items
                (cdr (assoc 'conflicts resolution))
              )
              (princ)
            )
          )

          (run-analysis)

          (action_tile
            "recalc"
            "(run-analysis)"
          )

          (action_tile
            "z_conflicts"
            "(setq conflict-index (atoi $value)) (if (= $reason 4) (if (and conflict-items (nth conflict-index conflict-items)) (gp-exp-show-conflict (nth conflict-index conflict-items))))"
          )

          (action_tile
            "zoom_conflict"
            "(if (and conflict-items (nth conflict-index conflict-items)) (gp-exp-show-conflict (nth conflict-index conflict-items)))"
          )

          (action_tile
            "accept"
            "(setq options (gp-exp-read-live-options)) (done_dialog 1)"
          )

          (setq status (start_dialog))
          (unload_dialog dcl-id)
          (vl-file-delete dcl-file)
          (setq dcl-id nil
                dcl-file nil)

          (if (= status 1)
            (progn
              ;; Analiza finalna jest wykonywana jeszcze raz na zaakceptowanych
              ;; ustawieniach, wiec wynik nie zalezy od klikniecia Odswiez.
              (setq resolution
                (gp-exp-run-resolution all-records texts options)
              )

              (if (= (length (cdr (assoc 'records resolution))) 0)
                (alert "Brak punktow do eksportu po zastosowaniu filtrow.")
                (progn
                  (setq filename
                    (gp-exp-save-file-dialog
                      (gp-exp-option options 'out-format)
                    )
                  )

                  (if filename
                    (progn
                      (setq output-lines
                        (gp-exp-build-output-lines resolution options)
                      )
                      (setq output-count
                        (gp-exp-write-output
                          filename
                          output-lines
                          (gp-exp-option options 'out-format)
                        )
                      )
                      (setq systems
                        (gp-exp-detect-record-system
                          (cdr (assoc 'records resolution))
                        )
                      )

                      (alert
                        (strcat
                          "Zapis zakonczony."
                          "\nPunkty: " (itoa output-count)
                          "\nFormat: " (strcase (gp-exp-option options 'out-format))
                          "\nUklad: " (car systems)
                          "\nOffset Z: "
                          (rtos (gp-exp-option options 'z-offset) 2 3)
                          " m"
                        )
                      )
                    )
                  )
                )
              )
            )
          )

          (setq *error* old-err)
          (princ)
        )
      )
    )
  )
)

(defun c:EKSPORT_PIKIET_V22 ()
  (c:EKSPORT_PIKIET_V23)
)

(princ "\nKomendy: EKSPORT_PIKIET_V23, EKSPORT_PIKIET_V22")
(princ)
