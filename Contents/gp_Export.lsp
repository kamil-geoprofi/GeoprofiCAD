;; ======================================================
;; GEOPROFICAD - GP_EXPORT V23
;; AUTO DLA BRAKUJACYCH ID - PAKIET FUNKCJI DO PODMIANY
;; Baza: commit 7990493790f4e7f9d23cde29ff33205ff43cf7ae
;; ======================================================
;;
;; Zmiana ograniczona do obslugi ID i UI:
;; - nowa opcja: "Numeruj punkty bez znalezionego ID",
;; - Prefiks/Start sa aktywne tylko gdy sa potrzebne,
;; - "Nowa numeracja wszystkich" ma pierwszenstwo,
;; - przy wylaczonym AUTO brak ID jest raportowany jako MISSING,
;; - TXT z brakujacym ID jest blokowany (PTS nie wymaga ID),
;; - radar, Z, geometria i dopasowanie 1:1 nie sa redefiniowane.
;;
;; Ten plik mozna:
;; 1) potraktowac jako zestaw pelnych funkcji do podmiany w gp_Export.lsp,
;; 2) albo testowo zaladowac PO aktualnym gp_Export.lsp - definicje ponizej
;;    nadpisza tylko funkcje zwiazane z numeracja/UI.
;; ======================================================

(defun gp-exp-source-label (source)
  (cond
    ((= source "ATTR") "atrybut")
    ((= source "BLOCK_TEXT") "tekst-bloku")
    ((= source "GEOM") "geometria")
    ((= source "RADAR") "tekst-obok")
    ((= source "AUTO") "auto")
    ((= source "MISSING") "brak-id")
    ((= source "ZERO") "zero")
    (T "brak")
  )
)


(defun gp-exp-resolve-records
  (
    records texts radius id-tags z-tags unique-texts z-mode
    renum-all auto-missing fix-dupes auto-prefix auto-start
    /
    base-map record rid base
    radar-id-map radar-z-map radar-z-candidate-map
    raw-id-map final-id-map final-z-map
    raw-id raw-source radar-entry raw-entry
    id-index id-counts id-labels occupied-id-keys
    duplicate-groups duplicate-records duplicate-next-map missing-id-records
    key count base-id suffix candidate candidate-key
    allocation next-auto id-value
    raw-z z-source z-entry
    conflicts conflict-count pt obj nearest-z
    z-texts candidates near-info
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
  ;; Przy wymuszonej nowej numeracji nie budujemy w ogole grafu ID.
  (if (= renum-all "1")
    (setq radar-id-map '())
    (if unique-texts
      (setq radar-id-map
        (gp-exp-assign-radar-unique records texts radius "ID" base-map z-mode)
      )
      (setq radar-id-map
        (gp-exp-assign-radar-reusable records texts radius "ID" base-map z-mode)
      )
    )
  )

  (if unique-texts
    (setq radar-z-map
      (gp-exp-assign-radar-unique records texts radius "Z" base-map z-mode)
    )
    (setq radar-z-map
      (gp-exp-assign-radar-reusable records texts radius "Z" base-map z-mode)
    )
  )
  (setq radar-z-candidate-map *gp-exp-last-radar-candidate-map*)

  ;; 3. Najpierw zbieramy surowe ID wszystkich rekordow.
  ;; Niczego jeszcze nie numerujemy AUTO - dzieki temu AUTO zna wszystkie
  ;; prawdziwe nazwy i nie zabierze numeru znalezionemu pozniej ID.
  (setq raw-id-map '())

  (foreach record records
    (setq rid (gp-exp-record-get record 'rid)
          base (gp-exp-map-get base-map rid)
          radar-entry (gp-exp-map-get radar-id-map rid)
          raw-id nil
          raw-source nil)

    (if (/= renum-all "1")
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

    (setq raw-id-map
      (gp-exp-map-set
        raw-id-map
        rid
        (gp-exp-make-id-entry raw-id raw-source radar-entry)
      )
    )
  )

  ;; 4. Finalne ID.
  ;; - nowa numeracja wszystkich: AUTO dla kazdego rekordu,
  ;; - normalny tryb: najpierw rozrozniamy powtarzajace sie ID,
  ;;   potem - tylko gdy auto-missing=1 - nadajemy AUTO rekordom bez ID.
  (setq final-id-map '()
        next-auto auto-start
        duplicate-groups 0
        duplicate-records 0
        missing-id-records 0)

  (if (= renum-all "1")
    (progn
      (setq occupied-id-keys '())

      (foreach record records
        (setq rid (gp-exp-record-get record 'rid)
              allocation
                (gp-exp-allocate-auto-id
                  auto-prefix
                  next-auto
                  occupied-id-keys)
              id-value (nth 0 allocation)
              next-auto (nth 1 allocation)
              occupied-id-keys (nth 2 allocation))

        (setq final-id-map
          (gp-exp-map-set
            final-id-map
            rid
            (gp-exp-make-id-entry id-value "AUTO" nil)
          )
        )
      )
    )

    (progn
      ;; Indeks powstaje ze wszystkich ID z atrybutow, tekstow blokow i radaru.
      (setq id-index (gp-exp-build-id-index records raw-id-map)
            id-counts (nth 0 id-index)
            id-labels (nth 1 id-index)
            occupied-id-keys (nth 2 id-index)
            duplicate-groups (nth 3 id-index)
            duplicate-records (nth 4 id-index)
            duplicate-next-map '())

      ;; 4a. Rozroznienie prawdziwych duplikatow:
      ;; P12, P12, P12 -> P12(1), P12(2), P12(3).
      ;; Kolejnosc jest zgodna z kolejnoscia rekordow.
      (foreach record records
        (setq rid (gp-exp-record-get record 'rid)
              raw-entry (gp-exp-map-get raw-id-map rid)
              raw-id (if raw-entry (cdr (assoc 'value raw-entry)) nil)
              key (gp-exp-id-key raw-id)
              count (if key (gp-exp-map-get id-counts key) nil))

        (if
          (and
            (= fix-dupes "1")
            key
            count
            (> count 1)
          )
          (progn
            (setq base-id (gp-exp-map-get id-labels key)
                  suffix (gp-exp-map-get duplicate-next-map key))
            (if (not suffix) (setq suffix 1))

            (setq candidate
                    (strcat base-id "(" (itoa suffix) ")")
                  candidate-key (gp-exp-id-key candidate))

            ;; Jezeli np. P12(1) juz istnieje jako prawdziwe ID,
            ;; omijamy je i bierzemy pierwszy wolny suffix.
            (while (member candidate-key occupied-id-keys)
              (setq suffix (1+ suffix)
                    candidate
                      (strcat base-id "(" (itoa suffix) ")")
                    candidate-key (gp-exp-id-key candidate))
            )

            (setq duplicate-next-map
              (gp-exp-map-set duplicate-next-map key (1+ suffix))
            )
            (setq occupied-id-keys
              (cons candidate-key occupied-id-keys)
            )

            (setq raw-entry
              (gp-exp-map-set raw-entry 'original-id raw-id)
            )
            (setq raw-entry
              (gp-exp-map-set raw-entry 'duplicate-renamed T)
            )
            (setq raw-entry
              (gp-exp-map-set raw-entry 'value candidate)
            )
          )
        )

        (setq final-id-map
          (gp-exp-map-set final-id-map rid raw-entry)
        )
      )

      ;; 4b. Fallback dla rekordow bez znalezionego ID.
      ;;
      ;; auto-missing = "1":
      ;;   nadaj AUTO z Prefiks/Start i omijaj wszystkie zajete ID.
      ;;
      ;; auto-missing = "0":
      ;;   zachowaj rekord, ale ID pozostaw puste. Zrodlo MISSING pozwala
      ;;   pokazac taki przypadek jawnie w raporcie i bezpiecznie zapisac TXT.
      (foreach record records
        (setq rid (gp-exp-record-get record 'rid)
              raw-entry (gp-exp-map-get final-id-map rid)
              raw-id (if raw-entry (cdr (assoc 'value raw-entry)) nil))

        (if (not (gp-exp-nonempty-p raw-id))
          (if (= auto-missing "1")
            (progn
              (setq allocation
                (gp-exp-allocate-auto-id
                  auto-prefix
                  next-auto
                  occupied-id-keys)
              )
              (setq id-value (nth 0 allocation)
                    next-auto (nth 1 allocation)
                    occupied-id-keys (nth 2 allocation))

              (setq final-id-map
                (gp-exp-map-set
                  final-id-map
                  rid
                  (gp-exp-make-id-entry id-value "AUTO" nil)
                )
              )
            )
            (progn
              (setq missing-id-records (1+ missing-id-records))
              (setq final-id-map
                (gp-exp-map-set
                  final-id-map
                  rid
                  (gp-exp-make-id-entry "" "MISSING" nil)
                )
              )
            )
          )
        )
      )
    )
  )

  ;; 5. Finalne Z.
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
              (if (= z-source "RADAR")
                (cdr (assoc 'text-object z-entry))
                nil
              )
            )
            (cons 'ambiguous
              (if (= z-source "RADAR")
                (cdr (assoc 'ambiguous z-entry))
                nil
              )
            )
            (cons 'reassigned
              (if (= z-source "RADAR")
                (cdr (assoc 'reassigned z-entry))
                nil
              )
            )
            (cons 'candidate-count
              (if (= z-source "RADAR")
                (cdr (assoc 'candidate-count z-entry))
                0
              )
            )
            (cons 'match-score
              (if (= z-source "RADAR")
                (cdr (assoc 'match-score z-entry))
                nil
              )
            )
          )
        )
        final-z-map
      )
    )
  )

  ;; 6. Konflikty Z do raportu.
  ;; Dla rekordow bez wlasnego Z wykorzystujemy juz zbudowana liste
  ;; kandydatow. Dodatkowy skan wykonujemy tylko dla obiektow z wlasnym Z
  ;; w trybie z_keep, bo takie rekordy nie uczestnicza w grafie radaru Z.
  (setq conflicts '()
        z-texts (gp-exp-filter-texts-category texts "Z"))

  (foreach record records
    (setq rid (gp-exp-record-get record 'rid)
          pt (gp-exp-record-get record 'pt)
          obj (gp-exp-record-get record 'obj)
          base (gp-exp-map-get base-map rid)
          candidates (gp-exp-map-get radar-z-candidate-map rid)
          conflict-count 0
          nearest-z nil)

    (cond
      (candidates
       (setq conflict-count (length candidates)
             nearest-z (nth 5 (car candidates)))
      )

      ((and (cdr (assoc 'z base)) (= z-mode "z_keep"))
       (setq near-info (gp-exp-near-text-info pt z-texts radius)
             conflict-count (car near-info)
             nearest-z (cadr near-info))
      )
    )

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
              " | tekstow Z="
              (if
                (>= conflict-count *gp-exp-radar-max-candidates*)
                (strcat ">=" (itoa *gp-exp-radar-max-candidates*))
                (itoa conflict-count)
              )
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
    (cons 'missing-id-records missing-id-records)
    (cons 'duplicate-id-groups duplicate-groups)
    (cons 'duplicate-id-records duplicate-records)
    (cons 'duplicate-id-renamed
      (if (= fix-dupes "1") duplicate-records 0))
  )
)


(defun gp-exp-stat-line
  (records id-map z-map kind /
    obj-count point-count
    ia ib ir iauto imissing
    za zb zg zr zz
    amb-id amb-z
  )
  (setq obj-count (gp-exp-object-count-kind records kind)
        point-count (gp-exp-record-count-kind records kind)

        ia (gp-exp-count-source-for-kind records id-map kind "ATTR")
        ib (gp-exp-count-source-for-kind records id-map kind "BLOCK_TEXT")
        ir (gp-exp-count-source-for-kind records id-map kind "RADAR")
        iauto (gp-exp-count-source-for-kind records id-map kind "AUTO")
        imissing (gp-exp-count-source-for-kind records id-map kind "MISSING")

        za (gp-exp-count-source-for-kind records z-map kind "ATTR")
        zb (gp-exp-count-source-for-kind records z-map kind "BLOCK_TEXT")
        zg (gp-exp-count-source-for-kind records z-map kind "GEOM")
        zr (gp-exp-count-source-for-kind records z-map kind "RADAR")
        zz (gp-exp-count-source-for-kind records z-map kind "ZERO")

        amb-id (gp-exp-count-ambiguous-for-kind records id-map kind)
        amb-z (gp-exp-count-ambiguous-for-kind records z-map kind)
  )

  (strcat
    (gp-exp-kind-label kind)
    " | obiekty=" (itoa obj-count)
    " punkty=" (itoa point-count)
    " | ID: attr=" (itoa ia)
    " blok=" (itoa ib)
    " tekst=" (itoa ir)
    " auto=" (itoa iauto)
    " brak=" (itoa imissing)
    " | Z: attr=" (itoa za)
    " blok=" (itoa zb)
    " geom=" (itoa zg)
    " tekst=" (itoa zr)
    " zero=" (itoa zz)
    " | niepewne: ID=" (itoa amb-id)
    " Z=" (itoa amb-z)
  )
)


(defun gp-exp-total-source-summary
  (records id-map z-map /
    ia ib ir iauto imissing
    za zb zg zr zz
    kind amb-id amb-z
  )
  (setq ia (gp-exp-count-source-for-kind records id-map "INSERT" "ATTR")
        ib (gp-exp-count-source-for-kind records id-map "INSERT" "BLOCK_TEXT")
        ir 0
        iauto 0
        imissing 0
        za (gp-exp-count-source-for-kind records z-map "INSERT" "ATTR")
        zb (gp-exp-count-source-for-kind records z-map "INSERT" "BLOCK_TEXT")
        zg 0
        zr 0
        zz 0)

  (foreach kind '("POINT" "INSERT" "LINE" "POLYLINE" "ARC" "CIRCLE" "SOLID")
    (setq ir (+ ir (gp-exp-count-source-for-kind records id-map kind "RADAR"))
          iauto (+ iauto (gp-exp-count-source-for-kind records id-map kind "AUTO"))
          imissing (+ imissing (gp-exp-count-source-for-kind records id-map kind "MISSING"))
          zg (+ zg (gp-exp-count-source-for-kind records z-map kind "GEOM"))
          zr (+ zr (gp-exp-count-source-for-kind records z-map kind "RADAR"))
          zz (+ zz (gp-exp-count-source-for-kind records z-map kind "ZERO")))
  )

  (setq amb-id (gp-exp-count-ambiguous-total id-map)
        amb-z (gp-exp-count-ambiguous-total z-map))

  (strcat
    "Razem punktow: " (itoa (length records))
    " | ID: atrybut=" (itoa ia)
    " tekst-bloku=" (itoa ib)
    " tekst-obok=" (itoa ir)
    " auto=" (itoa iauto)
    " brak=" (itoa imissing)
    " | Z: atrybut=" (itoa za)
    " tekst-bloku=" (itoa zb)
    " geometria=" (itoa zg)
    " tekst-obok=" (itoa zr)
    " zero=" (itoa zz)
    " | NIEPEWNE: ID=" (itoa amb-id)
    " Z=" (itoa amb-z)
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
  (write-line "        : text { label = \"Dla szybkosci LINE/POLYLINE/ARC/SOLID sa domyslnie wylaczone.\"; }" file)
  (write-line "      }" file)

  (write-line "      : boxed_column { label = \"Radar tekstow i bloki\";" file)
  (write-line "        : row { : edit_box { key = \"t_r\"; label = \"Promien [m]:\"; edit_width = 8; value = \"1.5\"; }" file)
  (write-line "                : toggle { key = \"unique_texts\"; label = \"Jeden tekst tylko raz\"; value = \"1\"; } }" file)
  (write-line "        : row { : edit_box { key = \"b_t\"; label = \"Tagi ID:\"; edit_width = 18; value = \"NR, ID\"; }" file)
  (write-line "                : edit_box { key = \"z_t\"; label = \"Tagi Z:\"; edit_width = 18; value = \"H, Z, RZEDNA\"; } }" file)
  (write-line "        : text { label = \"Dla INSERT: atrybut -> geometria -> tekst w bloku -> tekst obok.\"; }" file)
  (write-line "      }" file)

  (write-line "      : boxed_column { label = \"Duplikaty geometrii\";" file)
  (write-line "        : radio_column { key = \"d_m\";" file)
  (write-line "          : radio_button { key = \"keep\"; label = \"Zostaw wszystkie\"; value = \"1\"; }" file)
  (write-line "          : radio_button { key = \"rem\"; label = \"Usun duplikaty XY\"; }" file)
  (write-line "        }" file)
  (write-line "        : edit_box { key = \"d_tol\"; label = \"Tolerancja XY [m]:\"; edit_width = 8; value = \"0.01\"; is_enabled = false; }" file)
  (write-line "      }" file)

  (write-line "      : boxed_column { label = \"Numeracja ID\";" file)
  (write-line "        : toggle { key = \"auto_missing\"; label = \"Numeruj punkty bez znalezionego ID\"; value = \"1\"; }" file)
  (write-line "        : row { : edit_box { key = \"a_p\"; label = \"Prefiks:\"; edit_width = 10; value = \"P_\"; }" file)
  (write-line "                : edit_box { key = \"a_s\"; label = \"Start:\"; edit_width = 8; value = \"1\"; } }" file)
  (write-line "        : toggle { key = \"renum_all\"; label = \"Nowa numeracja wszystkich\"; value = \"0\"; }" file)
  (write-line "        : text { label = \"Nowa numeracja wszystkich uzywa tego samego Prefiks/Start.\"; }" file)
  (write-line "        : toggle { key = \"fix_dupes\"; label = \"Rozroznij powtarzajace sie ID\"; value = \"1\"; }" file)
  (write-line "        : text { label = \"Np. P12 + P12 -> P12(1), P12(2).\"; }" file)
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
  (write-line "        : button { key = \"recalc\"; label = \"Analizuj / odswiez raport\"; }" file)
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


(defun gp-exp-update-id-numbering-tiles (/ renum-all auto-missing)
  ;; UI:
  ;; - "Nowa numeracja wszystkich" ma pierwszenstwo nad pozostala logika ID,
  ;; - Prefiks/Start sa aktywne, gdy numerujemy braki albo wszystkie punkty,
  ;; - przy pelnej renumeracji rozroznianie duplikatow nie ma znaczenia.
  (setq renum-all (= (get_tile "renum_all") "1")
        auto-missing (= (get_tile "auto_missing") "1"))

  (if renum-all
    (progn
      (mode_tile "auto_missing" 1)
      (mode_tile "fix_dupes" 1)
      (mode_tile "a_p" 0)
      (mode_tile "a_s" 0)
    )
    (progn
      (mode_tile "auto_missing" 0)
      (mode_tile "fix_dupes" 0)

      (if auto-missing
        (progn
          (mode_tile "a_p" 0)
          (mode_tile "a_s" 0)
        )
        (progn
          (mode_tile "a_p" 1)
          (mode_tile "a_s" 1)
        )
      )
    )
  )
  (princ)
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
    (cons 'auto-missing (get_tile "auto_missing"))
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
    (gp-exp-option options 'auto-missing)
    (gp-exp-option options 'fix-dupes)
    (gp-exp-option options 'auto-prefix)
    (gp-exp-option options 'auto-start)
  )
)


(defun gp-exp-build-output-lines
  (resolution options /
    records id-map z-map format geo-mode offset
    lines record rid pt id-entry z-entry id z x y line
  )
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
          id (if id-entry (cdr (assoc 'value id-entry)) "")
          z (+ (cdr (assoc 'value z-entry)) offset)
          x (car pt)
          y (cadr pt))

    ;; Przy wylaczonym AUTO brakujace ID jest jawnie pustym tekstem.
    ;; Chroni to TXT przed bledem STRCAT na NIL; PTS i tak nie zapisuje ID.
    (if (not (gp-exp-nonempty-p id))
      (setq id "")
    )

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


(defun c:EKSPORT_PIKIET_V23
  (
    /
    old-err f dcl-id dcl-file
    ss collected all-records texts object-count point-count
    run-analysis options last-options resolution conflict-items conflict-index
    status filename output-lines output-count systems missing-id-count
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
      (princ "\nPrzygotowanie zaznaczonych obiektow...")

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
              (setq last-options current-options)
              (setq conflict-items
                (cdr (assoc 'conflicts resolution))
              )
              (princ)
            )
          )

          ;; Dialog ma najpierw wejsc w aktywna petle zdarzen.
          ;; Pelna analiza jest uruchamiana dopiero przez przycisk raportu
          ;; albo po zatwierdzeniu ustawien.
          (setq resolution nil
                last-options nil
                conflict-items nil)

          (start_list "type_stats")
          (add_list "Kliknij Analizuj / odswiez raport.")
          (end_list)
          (set_tile "summary" "Radar nie zostal jeszcze uruchomiony.")
          (set_tile "sys_info" "Wybierz typy i ustawienia, potem uruchom analize.")
          (start_list "z_conflicts")
          (add_list "Brak analizy.")
          (end_list)

          ;; Ustaw stan pol numeracji zgodnie z wartosciami domyslnymi.
          (gp-exp-update-id-numbering-tiles)

          ;; Tolerancja XY ma znaczenie tylko przy usuwaniu duplikatow.
          (action_tile
            "keep"
            "(mode_tile \"d_tol\" 1)"
          )
          (action_tile
            "rem"
            "(mode_tile \"d_tol\" 0)"
          )

          ;; Prefiks/Start sa aktywne dla numeracji brakow albo pelnej
          ;; renumeracji. Przy renum_all pozostale opcje ID sa tylko informacyjne.
          (action_tile
            "auto_missing"
            "(gp-exp-update-id-numbering-tiles)"
          )
          (action_tile
            "renum_all"
            "(gp-exp-update-id-numbering-tiles)"
          )

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
              ;; Gdy ustawienia nie zmienily sie od ostatniego raportu,
              ;; wykorzystujemy gotowy wynik zamiast ponownie liczyc radar.
              (if (or (not resolution) (not (equal options last-options)))
                (setq resolution
                  (gp-exp-run-resolution all-records texts options)
                )
              )

              (if (= (length (cdr (assoc 'records resolution))) 0)
                (alert "Brak punktow do eksportu po zastosowaniu filtrow.")
                (progn
                  (setq missing-id-count
                    (cdr (assoc 'missing-id-records resolution))
                  )
                  (if (not missing-id-count)
                    (setq missing-id-count 0)
                  )

                  ;; TXT wymaga kolumny ID. Jezeli uzytkownik swiadomie
                  ;; wylaczy AUTO dla brakow, zatrzymujemy zapis zamiast
                  ;; tworzyc niejednoznaczny plik z przesunietymi kolumnami.
                  ;; PTS nie zapisuje ID, wiec ten warunek go nie dotyczy.
                  (if
                    (and
                      (= (gp-exp-option options 'out-format) "txt")
                      (> missing-id-count 0)
                    )
                    (alert
                      (strcat
                        "Nie mozna zapisac TXT: "
                        (itoa missing-id-count)
                        " punktow nie ma ID."
                        "\nWlacz 'Numeruj punkty bez znalezionego ID'"
                        "\nalbo popraw dane i uruchom analize ponownie."
                      )
                    )
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
            )
          )

          (setq *error* old-err)
          (princ)
        )
      )
    )
  )
)

