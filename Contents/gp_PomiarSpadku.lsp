(vl-load-com)
(load "gp_Core.lsp" "\nBLAD: Nie znaleziono pliku gp_Core.lsp!")

;; ======================================================
;; GEOPROFICAD - POMIAR SPADKU MIEDZY 2 PUNKTAMI
;;
;; Komendy:
;; - POMIERZ_SPADEK
;; - POMIAR_SPADKU
;; - SPADEK_2PKT
;;
;; Funkcja:
;; - wskazujesz 2 obiekty/pikiety,
;; - skrypt pobiera XY i Z,
;; - liczy odleglosc 2D, roznice wysokosci, spadek %, spadek promil,
;; - pokazuje wynik w oknie,
;; - opcjonalnie wstawia opis na rysunku.
;;
;; Obslugiwane:
;; - blok Pikieta_Geo z atrybutem H/Z/RZEDNA,
;; - POINT,
;; - TEXT/MTEXT z liczba w tresci,
;; - CIRCLE jako XY srodka, ale Z trzeba zwykle podac recznie,
;; - zwykly BLOCK bez atrybutu: XY z insertion point, Z recznie.
;; ======================================================


(defun geocad-spadek-parse-number (str / chars ch started token has-digit code cleaned val)
  ;; Wyciaga pierwsza liczbe z tekstu.
  ;; Dziala dla: "134.78", "134,78", "H=134.78", "Rz. 134,78 m".
  (setq token "")
  (setq started nil)
  (setq has-digit nil)

  (if str
    (progn
      (setq chars (vl-string->list str))

      (while chars
        (setq ch (car chars))
        (setq code ch)

        (cond
          ;; Start liczby: cyfra albo znak +/-
          ((and
             (not started)
             (or
               (and (>= code 48) (<= code 57))
               (= code 45)
               (= code 43)
             )
           )
            (setq started T)
            (setq token (strcat token (chr code)))

            (if (and (>= code 48) (<= code 57))
              (setq has-digit T)
            )
          )

          ;; Kontynuacja liczby: cyfry, kropka, przecinek
          ((and
             started
             (or
               (and (>= code 48) (<= code 57))
               (= code 46)
               (= code 44)
             )
           )
            (setq token (strcat token (chr code)))

            (if (and (>= code 48) (<= code 57))
              (setq has-digit T)
            )
          )

          ;; Koniec liczby
          (started
            (setq chars nil)
          )
        )

        (if chars
          (setq chars (cdr chars))
        )
      )
    )
  )

  (if has-digit
    (progn
      (setq cleaned (vl-string-translate "," "." token))
      (setq val (distof cleaned))
      val
    )
    nil
  )
)


(defun geocad-spadek-get-attr-z (obj / z-tags att tag val parsed found)
  ;; Pobiera Z z atrybutow bloku.
  ;; Korzysta z ustawien ZTags z gp_Core.lsp.
  (setq found nil)
  (setq z-tags (geocad-parse-tags (geocad-get-cfg "ZTags" "H,Z,RZEDNA")))

  (if (= (vla-get-HasAttributes obj) :vlax-true)
    (foreach att (vlax-invoke obj 'GetAttributes)
      (setq tag (strcase (vla-get-TagString att)))

      (if (and (member tag z-tags) (not found))
        (progn
          (setq val (vla-get-TextString att))
          (setq parsed (geocad-spadek-parse-number val))

          (if parsed
            (setq found parsed)
          )
        )
      )
    )
  )

  found
)


(defun geocad-spadek-point-from-object (obj label / type pt z parsed str src)
  ;; Zwraca liste:
  ;; (punkt-3d zrodlo-z)
  ;;
  ;; Jezeli nie znajdzie Z, pyta uzytkownika recznie.
  (setq type (vla-get-ObjectName obj))
  (setq pt nil)
  (setq z nil)
  (setq src "")

  (cond
    ;; AutoCAD POINT - Z z geometrii punktu
    ((= type "AcDbPoint")
      (setq pt (vlax-safearray->list (vlax-variant-value (vla-get-Coordinates obj))))
      (setq z (caddr pt))
      (setq src "Z geometrii POINT")
    )

    ;; Blok - XY z insertion point, Z z atrybutu H/Z/RZEDNA, ewentualnie recznie
    ((= type "AcDbBlockReference")
      (setq pt (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint obj))))
      (setq parsed (geocad-spadek-get-attr-z obj))

      (if parsed
        (progn
          (setq z parsed)
          (setq src "Z z atrybutu bloku")
        )
        (progn
          ;; Nie ufamy insertion Z bloku jako rzednej, bo czesto bywa przypadkowe.
          (setq z nil)
          (setq src "Z recznie - brak atrybutu")
        )
      )
    )

    ;; Tekst / MText - XY z insertion point, Z z tresci tekstu
    ((member type '("AcDbText" "AcDbMText"))
      (setq pt (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint obj))))
      (setq str (vla-get-TextString obj))
      (setq parsed (geocad-spadek-parse-number str))

      (if parsed
        (progn
          (setq z parsed)
          (setq src "Z z tresci tekstu")
        )
        (progn
          (setq z nil)
          (setq src "Z recznie - tekst bez liczby")
        )
      )
    )

    ;; Okrag - XY ze srodka, Z recznie
    ((= type "AcDbCircle")
      (setq pt (vlax-safearray->list (vlax-variant-value (vla-get-Center obj))))

      ;; Dla CIRCLE nie ufamy Center Z, bo po obcych DWG czesto jest przypadkowe.
      ;; Skrypt bierze tylko XY ze srodka okregu i pyta o Z.
      (setq z nil)
      (setq src "Z recznie - CIRCLE")
    )

    ;; Inne obiekty
    (T
      (setq pt nil)
      (setq z nil)
      (setq src "Nieobslugiwany obiekt")
    )
  )

  (if (not pt)
    nil
    (progn
      (if (not z)
        (progn
          (setq z
            (getreal
              (strcat
                "\nNie odczytano rzednej dla "
                label
                ". Podaj Z recznie: "
              )
            )
          )

          (if (not z)
            nil
            (progn
              (setq pt (list (car pt) (cadr pt) z))
              (list pt src)
            )
          )
        )
        (progn
          (setq pt (list (car pt) (cadr pt) z))
          (list pt src)
        )
      )
    )
  )
)


(defun geocad-spadek-format-sign (val prec / s)
  ;; Formatowanie z plusem dla wartosci dodatnich.
  (setq s (rtos val 2 prec))

  (if (> val 0.0)
    (strcat "+" s)
    s
  )
)


(defun geocad-spadek-make-layer (doc layname color / layers lay res)
  ;; Tworzy warstwe, jesli nie istnieje.
  ;; Jesli istnieje, tylko ja zwraca i ustawia kolor.
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

  layname
)


(defun geocad-spadek-insert-label
  (
    doc space pt1 pt2 dist2 dz slope promil
    /
    mid txt-h prefix lay txt width mtxt
  )

  ;; Wstawia opis pomiaru spadku jako MTEXT.
  (setq txt-h (atof (geocad-get-cfg "TxtH" "1.0")))

  (if (<= txt-h 0.0)
    (setq txt-h 1.0)
  )

  (setq prefix (geocad-get-cfg "Prefix" "POMIAR"))
  (setq lay (geocad-spadek-make-layer doc (strcat prefix "_POMIAR_SPADKU") 2))

  (setq mid
    (list
      (/ (+ (car pt1) (car pt2)) 2.0)
      (/ (+ (cadr pt1) (cadr pt2)) 2.0)
      0.0
    )
  )

  (setq txt
    (strcat
      "i = " (geocad-spadek-format-sign slope 2) "%"
      "\\P"
      "L = " (rtos dist2 2 2) " m"
      ", dH = " (geocad-spadek-format-sign dz 3) " m"
      "\\P"
      "i = " (geocad-spadek-format-sign promil 1) " promil"
    )
  )

  (setq width (* txt-h 35.0))

  (setq mtxt
    (vla-AddMText
      space
      (vlax-3d-point mid)
      width
      txt
    )
  )

  (vla-put-Height mtxt txt-h)
  (vla-put-Layer mtxt lay)

  mtxt
)


(defun geocad-run-pomiar-spadku
  (
    /
    old-err
    doc space
    ent1 ent2
    obj1 obj2
    data1 data2
    pt1 pt2
    z1 z2
    dist2 dz slope promil ratio kier
    ask-label
    result-msg
  )

  (setq old-err *error*)

  (setq *error*
    (lambda (msg)
      (setq *error* old-err)

      (if msg
        (princ (strcat "\nPrzerwano: " msg))
      )

      (princ)
    )
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq space (vla-get-ModelSpace doc))

  (princ "\n==============================================")
  (princ "\nGEOPROFICAD - POMIAR SPADKU 2PKT")
  (princ "\n==============================================")

  ;; ------------------------------------------------------
  ;; Punkt 1
  ;; ------------------------------------------------------
  (setq ent1 (car (entsel "\nWybierz pierwszy punkt/pikiete/tekst: ")))

  (if (not ent1)
    (progn
      (princ "\nNie wybrano pierwszego obiektu.")
      (setq *error* old-err)
      (princ)
      (exit)
    )
  )

  (setq obj1 (vlax-ename->vla-object ent1))
  (setq data1 (geocad-spadek-point-from-object obj1 "punktu 1"))

  (if (not data1)
    (progn
      (alert "Nie udalo sie odczytac punktu 1.")
      (setq *error* old-err)
      (princ)
      (exit)
    )
  )

  ;; ------------------------------------------------------
  ;; Punkt 2
  ;; ------------------------------------------------------
  (setq ent2 (car (entsel "\nWybierz drugi punkt/pikiete/tekst: ")))

  (if (not ent2)
    (progn
      (princ "\nNie wybrano drugiego obiektu.")
      (setq *error* old-err)
      (princ)
      (exit)
    )
  )

  (setq obj2 (vlax-ename->vla-object ent2))
  (setq data2 (geocad-spadek-point-from-object obj2 "punktu 2"))

  (if (not data2)
    (progn
      (alert "Nie udalo sie odczytac punktu 2.")
      (setq *error* old-err)
      (princ)
      (exit)
    )
  )

  ;; ------------------------------------------------------
  ;; Obliczenia
  ;; ------------------------------------------------------
  (setq pt1 (car data1))
  (setq pt2 (car data2))

  (setq z1 (caddr pt1))
  (setq z2 (caddr pt2))

  ;; Odleglosc tylko po XY, bez Z.
  (setq dist2
    (distance
      (list (car pt1) (cadr pt1) 0.0)
      (list (car pt2) (cadr pt2) 0.0)
    )
  )

  (if (<= dist2 0.000001)
    (progn
      (alert "Punkty maja taka sama pozycje XY. Nie mozna policzyc spadku.")
      (setq *error* old-err)
      (princ)
      (exit)
    )
  )

  (setq dz (- z2 z1))
  (setq slope (* (/ dz dist2) 100.0))
  (setq promil (* (/ dz dist2) 1000.0))

  (cond
    ((> dz 0.0005)
      (setq kier "w gore od punktu 1 do punktu 2")
    )

    ((< dz -0.0005)
      (setq kier "w dol od punktu 1 do punktu 2")
    )

    (T
      (setq kier "poziomo")
    )
  )

  (if (> (abs dz) 0.000001)
    (setq ratio (/ dist2 (abs dz)))
    (setq ratio nil)
  )

  ;; ------------------------------------------------------
  ;; Wynik w oknie
  ;; ------------------------------------------------------
  (setq result-msg
    (strcat
      "POMIAR SPADKU 2PKT"
      "\n\n"

      "P1:"
      "\nZ = " (rtos z1 2 3) " m"
      "\nX = " (rtos (car pt1) 2 3)
      "\nY = " (rtos (cadr pt1) 2 3)
      "\nZrodlo: " (cadr data1)

      "\n\n"

      "P2:"
      "\nZ = " (rtos z2 2 3) " m"
      "\nX = " (rtos (car pt2) 2 3)
      "\nY = " (rtos (cadr pt2) 2 3)
      "\nZrodlo: " (cadr data2)

      "\n\n"

      "WYNIK:"
      "\nOdleglosc 2D: " (rtos dist2 2 3) " m"
      "\ndH = Z2 - Z1: " (geocad-spadek-format-sign dz 3) " m"
      "\nSpadek: " (geocad-spadek-format-sign slope 3) " %"
      "\nSpadek: " (geocad-spadek-format-sign promil 2) " promil"

      (if ratio
        (strcat "\nNachylenie: 1:" (rtos ratio 2 2))
        "\nNachylenie: brak, dH = 0"
      )

      "\nKierunek: " kier
    )
  )

  (alert result-msg)

  ;; Krotki slad w konsoli, bez dlugiego raportu.
  (princ
    (strcat
      "\nPomiar spadku: "
      (geocad-spadek-format-sign slope 3)
      "%, dH="
      (geocad-spadek-format-sign dz 3)
      " m, L="
      (rtos dist2 2 3)
      " m."
    )
  )

  ;; ------------------------------------------------------
  ;; Opcjonalny opis na rysunku
  ;; ------------------------------------------------------
  (initget "Tak Nie")
  (setq ask-label
    (getkword "\nWstawic opis spadku na rysunku? [Tak/Nie] <Nie>: ")
  )

  (if (= ask-label "Tak")
    (progn
      (geocad-spadek-insert-label doc space pt1 pt2 dist2 dz slope promil)
      (princ "\nWstawiono opis spadku.")
    )
  )

  (setq *error* old-err)
  (princ)
)


(defun c:POMIERZ_SPADEK ()
  (geocad-run-pomiar-spadku)
)


(defun c:POMIAR_SPADKU ()
  (geocad-run-pomiar-spadku)
)


(defun c:SPADEK_2PKT ()
  (geocad-run-pomiar-spadku)
)


(princ "\nKomendy wczytane: POMIERZ_SPADEK, POMIAR_SPADKU, SPADEK_2PKT")
(princ)