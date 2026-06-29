(vl-load-com)   
(load "gp_Core.lsp" "\nBLAD: Nie znaleziono pliku gp_Core.lsp!") 
 
(defun geocad-err (msg)
  ;; Jezeli import zostal przerwany po czesciowym wstawieniu pikiet,
  ;; domykamy batch. Dla importu z numerami z pliku batch zwykle nie
  ;; zapisze GeoLicznik, ale domkniecie jest bezpieczne i spojne.
  (if batch
    (progn
      (setq batch (geocad-pikieta-batch-end batch))
      (setq batch nil)
    )
  )

  (if file
    (close file)
  )

  (if doc
    (vla-EndUndoMark doc)
  )

  (if old-cmdecho
    (setvar "CMDECHO" old-cmdecho)
  )

  (if old-attmode
    (setvar "ATTMODE" old-attmode)
  )

  (if old-dimzin
    (setvar "DIMZIN" old-dimzin)
  )

  (if old-pdmode
    (setvar "PDMODE" old-pdmode)
  )

  (if old-pdsize
    (setvar "PDSIZE" old-pdsize)
  )

  (if old-osmode
    (setvar "OSMODE" old-osmode)
  )

  (setq *error* old-err)

  (if (not (member msg '("Function cancelled" "quit / exit abort")))
    (princ (strcat "\nBlad skryptu: " msg))
    (princ "\nPrzerwano (ESC).")
  )

  (princ)
)
 
(defun get-native-windows-file ( / wsh tmpFile shellCmd f res )  
  (setq wsh (vlax-create-object "WScript.Shell") tmpFile (vl-filename-mktemp "file_res.txt"))  
  (setq shellCmd (strcat "powershell.exe -WindowStyle Hidden -Command \"& { Add-Type -AssemblyName System.Windows.Forms; $d = New-Object System.Windows.Forms.OpenFileDialog; $d.Filter = 'Pliki geodezyjne (*.txt;*.xyz;*.csv)|*.txt;*.xyz;*.csv|Wszystkie pliki (*.*)|*.*'; $d.Title = 'Wybierz plik pikiety'; if($d.ShowDialog() -eq 'OK') { [System.IO.File]::WriteAllText('" (vl-string-translate "\\" "/" tmpFile) "', $d.FileName) } }\""))  
  (vlax-invoke-method wsh 'Run shellCmd 0 :vlax-true) (vlax-release-object wsh)  
  (if (findfile tmpFile) (progn (setq f (open tmpFile "r")) (if f (progn (setq res (read-line f)) (close f))) (vl-file-delete tmpFile))) res  
)  
  
(defun clean-raw-line (str) (if str (vl-string-trim " \t\r\n" (vl-string-translate (chr 160) (chr 32) str)) nil)) 
(defun count-char (str ch / count) (setq count 0) (foreach c (vl-string->list str) (if (= c ch) (setq count (1+ count)))) count) 
(defun detect-delim (filename line / ext) (setq ext (strcase (vl-filename-extension filename))) (cond ((= ext ".CSV") (if (> (count-char line 59) 0) 59 44)) (t 32))) 
(defun safe-tokenize (line delim-code / res tmp) (setq res '() tmp "") (if (= delim-code 32) (foreach char (vl-string->list (strcat line " ")) (if (member char '(32 9)) (if (/= tmp "") (setq res (cons tmp res) tmp "")) (setq tmp (strcat tmp (chr char))))) (foreach char (vl-string->list (strcat line (chr delim-code))) (if (= char delim-code) (progn (setq tmp (vl-string-trim " \t" tmp)) (setq res (cons tmp res) tmp "")) (setq tmp (strcat tmp (chr char)))))) (reverse res)) 
(defun safe-atof (str) (if str (atof (vl-string-translate "," "." (vl-princ-to-string str))) 0.0)) 
(defun clean-sample-for-dcl (line delim-code / tokens res) (if (and line (/= line "")) (progn (setq tokens (safe-tokenize line delim-code)) (setq res "") (foreach tkn tokens (setq res (strcat res "[" tkn "]   "))) (setq res (vl-string-trim " " res)) (setq res (vl-string-translate "\"" "'" res)) (if (> (strlen res) 85) (setq res (strcat (substr res 1 85) "..."))) res) ""))  
(defun get-system-info (c1 c2 / info) (setq info "Lokalny / Inny") (if (and (> c1 1000.0) (> c2 1000.0)) (cond ((and (>= c1 6000000.0) (< c1 9000000.0) (< c2 6200000.0)) (setq info (strcat "2000/" (itoa (fix (/ c1 1000000.0)))))) ((and (>= c2 6000000.0) (< c2 9000000.0) (< c1 6200000.0)) (setq info (strcat "2000/" (itoa (fix (/ c2 1000000.0)))))) ((and (>= c1 5300000.0) (< c1 6100000.0) (>= c2 5300000.0) (< c2 6100000.0)) (setq info "2000/5")) ((and (< c1 1000000.0) (> c1 100000.0) (< c2 1000000.0) (> c2 100000.0)) (setq info "PL-1992")) (t (setq info "Prawdopodobnie PL")))) info)  
(defun guess-single-line (tokens / len has-nr has-z c1 c2 guess-xy) (setq len (length tokens)) (if (< len 2) nil (progn (if (or (>= len 4) (not (vl-string-search "." (vl-princ-to-string (nth 0 tokens)))) (not (distof (vl-string-translate "," "." (vl-princ-to-string (nth 0 tokens)))))) (setq has-nr T) (setq has-nr nil)) (if has-nr (progn (setq c1 (safe-atof (nth 1 tokens)) c2 (safe-atof (nth 2 tokens))) (if (>= len 4) (setq has-z T) (setq has-z nil))) (progn (setq c1 (safe-atof (nth 0 tokens)) c2 (safe-atof (nth 1 tokens))) (if (>= len 3) (setq has-z T) (setq has-z nil)))) (if (and (> c1 1000.0) (> c2 1000.0)) (progn (cond ((> c1 6200000.0) (setq guess-xy "1")) ((> c2 6200000.0) (setq guess-xy "2")) (t (if (< c1 c2) (setq guess-xy "1") (setq guess-xy "2")))) (cond ((and has-nr has-z (= guess-xy "1")) "1") ((and has-nr has-z (= guess-xy "2")) "2") ((and (not has-nr) has-z (= guess-xy "1")) "3") ((and (not has-nr) has-z (= guess-xy "2")) "4") ((and has-nr (not has-z) (= guess-xy "1")) "5") ((and has-nr (not has-z) (= guess-xy "2")) "6") (t guess-xy))) (cond ((and has-nr has-z) "1") ((and (not has-nr) has-z) "3") ((and has-nr (not has-z)) "5") (t "1"))))))  
(defun guess-format-multi (lines delim-code / guesses vote max-vote best-guess count) (setq guesses '()) (foreach line lines (setq vote (guess-single-line (safe-tokenize line delim-code))) (if vote (setq guesses (cons vote guesses)))) (if guesses (progn (setq max-vote 0 best-guess "1") (foreach u '("1" "2" "3" "4" "5" "6") (setq count (length (vl-remove-if-not '(lambda (x) (= x u)) guesses))) (if (> count max-vote) (progn (setq max-vote count) (setq best-guess u)))) best-guess) "1"))  
(defun round-to (val prec) (if (and val prec) (/ (fix (+ (* val (expt 10.0 prec)) 0.5)) (float (expt 10.0 prec))) val))

(defun geocad-import-layer-token-from-file
  (filename / base raw result i code ch prev-us)
  ;; Nazwa grupy imported pochodzi z nazwy pliku, ale musi byc bezpieczna
  ;; jako prefix warstw AutoCAD. Zostawiamy litery/cyfry, reszta -> "_".
  (setq base (vl-filename-base filename))
  (setq raw (strcase (if base base "IMPORT")))
  (setq result "")
  (setq prev-us nil)
  (setq i 1)

  (while (<= i (strlen raw))
    (setq ch (substr raw i 1))
    (setq code (ascii ch))
    (if
      (or
        (and (>= code 48) (<= code 57))
        (and (>= code 65) (<= code 90))
      )
      (progn
        (setq result (strcat result ch))
        (setq prev-us nil)
      )
      (if (not prev-us)
        (progn
          (setq result (strcat result "_"))
          (setq prev-us T)
        )
      )
    )
    (setq i (1+ i))
  )

  (setq result (vl-string-trim "_" result))
  (if (= result "")
    (setq result "IMPORT")
  )
  result
)


(defun geocad-import-format-has-nr-p (fmt c-nr-val / col)
  (cond
    ((member fmt '("1" "2" "5" "6")) T)
    ((= fmt "7")
      (setq col (atoi (vl-princ-to-string c-nr-val)))
      (> col 0)
    )
    (T nil)
  )
)

(defun geocad-import-update-use-file-nr-tile (fmt c-nr-val / can-use)
  (setq can-use (geocad-import-format-has-nr-p fmt c-nr-val))
  (if can-use
    (mode_tile "use_file_nr" 0)
    (progn
      (set_tile "use_file_nr" "0")
      (mode_tile "use_file_nr" 1)
    )
  )
  can-use
)

(defun geocad-import-group-exists-p (group / pref)
  (setq pref (geocad-normalize-layer-prefix group))
  (and
    (/= pref "")
    (or
      (member pref (geocad-get-saved-prefixes))
      (> (geocad-count-objects-in-group pref) 0)
    )
  )
)

(defun geocad-import-ensure-txt-suffix (group / pref)
  (setq pref (geocad-normalize-layer-prefix group))
  (if (not (geocad-string-ends-with-p pref "_TXT"))
    (setq pref (strcat pref "_TXT"))
  )
  pref
)

(defun geocad-import-resolve-imported-group
  (filename / default-group group dcl-file dcl-fn dcl-id status raw msg)
  ;; Jezeli grupa z nazwy pliku juz istnieje, uzytkownik musi podac inna.
  (setq default-group
    (geocad-import-ensure-txt-suffix
      (geocad-import-layer-token-from-file filename)
    )
  )
  (setq group default-group)
  (setq status 1)

  (while (and (= status 1) (geocad-import-group-exists-p group))
    (setq dcl-file (vl-filename-mktemp "geo_import_group_name.dcl"))
    (setq dcl-fn (open dcl-file "w"))
    (write-line "GeoImportGroupName : dialog { label = \"Import TXT - nazwa grupy\";" dcl-fn)
    (write-line "  : boxed_column { label = \"Grupa imported juz istnieje\";" dcl-fn)
    (write-line (strcat "    : text { label = \"Grupa " group " juz istnieje.\"; }") dcl-fn)
    (write-line "    : text { label = \"Zmien nazwe grupy dla tego importu.\"; }" dcl-fn)
    (write-line "    : edit_box { key = \"group_name\"; label = \"Nazwa grupy:\"; edit_width = 32; }" dcl-fn)
    (write-line "    : text { key = \"status\"; label = \"Dopisz np. _ETAP2 albo _POPRAWKA.\"; }" dcl-fn)
    (write-line "  }" dcl-fn)
    (write-line "  ok_cancel;" dcl-fn)
    (write-line "}" dcl-fn)
    (close dcl-fn)

    (setq dcl-id (load_dialog dcl-file))
    (if (new_dialog "GeoImportGroupName" dcl-id)
      (progn
        (set_tile "group_name" group)
        (action_tile "accept" "(setq raw (get_tile \"group_name\")) (done_dialog 1)")
        (action_tile "cancel" "(done_dialog 0)")
        (setq status (start_dialog))
      )
      (setq status 0)
    )
    (if dcl-id (unload_dialog dcl-id))
    (vl-file-delete dcl-file)

    (if (= status 1)
      (progn
        (setq group (geocad-normalize-layer-prefix (geocad-import-layer-token-from-file raw)))
        (if (= group "")
          (setq group default-group)
        )
        (if (geocad-import-group-exists-p group)
          (alert (strcat "Grupa " group " tez juz istnieje. Podaj inna nazwe."))
        )
      )
    )
  )

  (if (= status 0)
    nil
    group
  )
)

(defun geocad-import-prepare-imported-group
  (doc filename group / kolor txt-h z-prec styl display)
  ;; Import z nazwami z pliku dostaje wlasna grupe <NAZWA_PLIKU>_TXT.
  ;; W tej grupie nie uzywamy prefixu numeracji pikiet - NR/H sa z pliku.
  (setq group (geocad-normalize-layer-prefix group))

  (setq kolor (geocad-get-cfg "Color" "3"))
  (setq txt-h (geocad-get-cfg "TxtH" "1.0"))
  (setq z-prec (geocad-get-cfg "Prec" "2"))
  (setq styl (geocad-get-cfg "Styl" "Blok"))
  (setq display (geocad-get-cfg "Display" "Oba"))

  (geocad-set-cfg "Prefix" group)
  (geocad-set-cfg "PiktPrefix" "")
  (geocad-save-group-settings
    group
    kolor
    ""
    styl
    display
    txt-h
    z-prec
    (geocad-get-cfg "ZTags" "H,Z,RZEDNA")
  )
  (geocad-group-cfg-write group "GroupType" "imported")
  (geocad-group-cfg-write group "ImportFile" (vl-filename-base filename))

  group
)


(defun c:IMPORT_POINTS_V3_7 ( / filename file line raw-line sample-lines format-choice is-flat final-delim  
                            px py pz pz-geom nr count valid acadObj doc mspace prec-geom-str prec-geom tokens  
                            total-valid current-valid idx-first idx-mid idx-last delim-code current-delim line-first line-mid line-last c1 c2 sys-info len current-format temp-delim dialog-running 
                            dcl-file dcl-fn dcl-id status minX minY maxX maxY dXX dYY margX margY marg p1 p2 
                            c-nr c-x c-y c-z delim-str do-zoom use-file-nr file-has-nr old-err old-cmdecho old-attmode old-dimzin old-pdmode old-pdsize old-osmode show-z batch import-group can-use-file-nr)
  
  (setq old-err *error* *error* geocad-err) 
  (setq old-cmdecho (getvar "CMDECHO") old-attmode (getvar "ATTMODE") old-dimzin (getvar "DIMZIN") old-pdmode (getvar "PDMODE") old-pdsize (getvar "PDSIZE") old-osmode (getvar "OSMODE"))  
 
  (setvar "CMDECHO" 0) (setvar "ATTMODE" 1) (setvar "DIMZIN" 0) (setvar "PDMODE" 32) (setvar "PDSIZE" 0.2)       
  (setq acadObj (vlax-get-acad-object) doc (vla-get-ActiveDocument acadObj) mspace (vla-get-ModelSpace doc))      
  
  (setq filename (get-native-windows-file))      
  (if (or (not filename) (= filename "")) (progn (setq *error* old-err) (exit)))      
  
  (setq file (open filename "r") total-valid 0)   
  (if (not file) (progn (alert "BLAD: Plik zablokowany!") (setq *error* old-err) (exit)))   
  (while (setq raw-line (read-line file)) (setq line (clean-raw-line raw-line)) (if (> (strlen line) 0) (setq total-valid (1+ total-valid))))  
  (close file)  
  (if (= total-valid 0) (progn (alert "Plik jest pusty!") (setq *error* old-err) (exit)))  
  
  (cond ((= total-valid 1) (setq idx-first 1 idx-mid 0 idx-last 0)) ((= total-valid 2) (setq idx-first 1 idx-mid 0 idx-last 2)) (t (setq idx-first 1 idx-mid (fix (/ total-valid 2)) idx-last total-valid)))  
  
  (setq file (open filename "r") current-valid 0 sample-lines '() line-first "" line-mid "" line-last "")  
  (while (setq raw-line (read-line file))  
    (setq line (clean-raw-line raw-line)) 
    (if (> (strlen line) 0) (progn (setq current-valid (1+ current-valid)) (if (<= current-valid 10) (setq sample-lines (cons line sample-lines))) (if (= current-valid idx-first) (setq line-first line)) (if (and (> idx-mid 0) (= current-valid idx-mid)) (setq line-mid line)) (if (and (> idx-last 0) (= current-valid idx-last)) (setq line-last line))))  
  )  
  (close file)  
  (setq sample-lines (reverse sample-lines) current-delim (detect-delim filename line-first) current-format (guess-format-multi sample-lines current-delim))
  (setq is-flat "0" do-zoom "1" use-file-nr "1" c-nr "1" c-x "3" c-y "2" c-z "4" prec-geom-str "" dialog-running T)
 
  (while dialog-running 
    (setq tokens (safe-tokenize line-first current-delim) len (length tokens))  
    (if (or (>= len 4) (not (vl-string-search "." (vl-princ-to-string (nth 0 tokens)))) (not (distof (vl-string-translate "," "." (vl-princ-to-string (nth 0 tokens)))))) (setq c1 (safe-atof (nth 1 tokens)) c2 (safe-atof (nth 2 tokens))) (setq c1 (safe-atof (nth 0 tokens)) c2 (safe-atof (nth 1 tokens))))  
    (setq sys-info (get-system-info c1 c2))  
    (if (or (and (>= len 4) (member current-format '("3" "4" "5" "6" "0"))) (and (= len 3) (member current-format '("1" "2" "0")))) (setq current-format (guess-format-multi sample-lines current-delim))) 
    (if (< len 3) (setq current-format "0")) 
    (cond ((= current-delim 32) (setq delim-str "Spacja / Tabulator")) ((= current-delim 59) (setq delim-str "Srednik (;)")) ((= current-delim 44) (setq delim-str "Przecinek (,)")) (t (setq delim-str "Spacja"))) 
 
    (setq dcl-file (vl-filename-mktemp "geo_fmt.dcl") dcl-fn (open dcl-file "w"))  
    (write-line "GeoFormat : dialog { label = \"Import Pikiet (100% Okienkowy)\";" dcl-fn)  
    (write-line "  : boxed_column { label = \"Podglad pliku (Kolumny rozdzielone [ ])\";" dcl-fn)  
    (write-line (strcat "    : text { value = \"[1]  " (clean-sample-for-dcl line-first current-delim) "\"; }") dcl-fn)  
    (if (/= line-mid "") (write-line (strcat "    : text { value = \"[S]  " (clean-sample-for-dcl line-mid current-delim) "\"; }") dcl-fn))  
    (if (/= line-last "") (write-line (strcat "    : text { value = \"[K]  " (clean-sample-for-dcl line-last current-delim) "\"; }") dcl-fn))  
    (write-line "    spacer;" dcl-fn)  
    (write-line (strcat "    : text { value = \"Rozpoznano: " sys-info " | Separator: " delim-str "\"; }") dcl-fn)  
    (write-line "  }" dcl-fn)  
    (write-line "  : boxed_radio_column { label = \"Uklad kolumn\"; key = \"fmt_choice\";" dcl-fn)  
    (cond ((>= len 4) (write-line "    : radio_button { key = \"1\"; label = \"[1] Numer | X (Wschod) | Y (Polnoc) | Z   (Standard CAD)\"; }" dcl-fn) (write-line "    : radio_button { key = \"2\"; label = \"[2] Numer | X (Polnoc) | Y (Wschod) | Z   (Geodezja PL)\"; }" dcl-fn)) ((= len 3) (write-line "    : radio_button { key = \"3\"; label = \"[3] X (Wschod) | Y (Polnoc) | Z           (Brak numeru)\"; }" dcl-fn) (write-line "    : radio_button { key = \"4\"; label = \"[4] X (Polnoc) | Y (Wschod) | Z           (Brak numeru, Geodezja PL)\"; }" dcl-fn) (write-line "    : radio_button { key = \"5\"; label = \"[5] Numer | X (Wschod) | Y (Polnoc)       (Plaskie, brak Z)\"; }" dcl-fn) (write-line "    : radio_button { key = \"6\"; label = \"[6] Numer | X (Polnoc) | Y (Wschod)       (Plaskie, brak Z, Geodezja PL)\"; }" dcl-fn)) ((< len 3) (write-line "    : radio_button { key = \"0\"; label = \"[!] BLAD: Zbyt malo kolumn. Zmien separator!\"; }" dcl-fn))) 
    (if (>= len 3) (write-line "    : radio_button { key = \"7\"; label = \"[7] Wlasny uklad (Ustaw nr kolumn ponizej)\"; }" dcl-fn)) 
    (write-line "  }" dcl-fn)  
    (write-line "  : boxed_row { label = \"Wlasny uklad (Dla opcji [7])\";" dcl-fn) 
    (write-line (strcat "    : edit_box { key = \"col_nr\"; label = \"Nr:\"; edit_width = 3; value = \"" c-nr "\"; }") dcl-fn) (write-line (strcat "    : edit_box { key = \"col_x\"; label = \"X (CAD):\"; edit_width = 3; value = \"" c-x "\"; }") dcl-fn) (write-line (strcat "    : edit_box { key = \"col_y\"; label = \"Y (CAD):\"; edit_width = 3; value = \"" c-y "\"; }") dcl-fn) (write-line (strcat "    : edit_box { key = \"col_z\"; label = \"Z:\"; edit_width = 3; value = \"" c-z "\"; }") dcl-fn) 
    (write-line "  }" dcl-fn) 
    (write-line "  : boxed_column { label = \"Numeracja importowanych pikiet\";" dcl-fn)
    (write-line "    : toggle { key = \"use_file_nr\"; label = \"Uzyj numerow/nazw pikiet z pliku (jesli sa w kolumnach)\"; value = \"1\"; }" dcl-fn)
    (write-line "    : text { value = \"Odznacz, aby pominac numery z TXT/CSV i numerowac wg biezacej numeracji rysunku.\"; }" dcl-fn)
    (write-line "  }" dcl-fn)
    (write-line "  : boxed_radio_row { label = \"Wymus separator\"; key = \"delim_choice\";" dcl-fn)  
    (write-line "    : radio_button { key = \"32\"; label = \"Spacja/Tab\"; }" dcl-fn) (write-line "    : radio_button { key = \"59\"; label = \"Srednik (;)\"; }" dcl-fn) (write-line "    : radio_button { key = \"44\"; label = \"Przecinek (,)\"; }" dcl-fn)  
    (write-line "  }" dcl-fn)  
    (write-line "  spacer;" dcl-fn) 
    (write-line "  : row {" dcl-fn)
    (write-line "    : toggle { key = \"flat_2d\"; label = \"Splaszcz 2D (Geometria Z=0.0)\"; value = \"0\"; }" dcl-fn) 
    (write-line "    : edit_box { key = \"prec_geom\"; label = \"Precyzja geometrii Z (puste=oryginal):\"; edit_width = 4; }" dcl-fn)
    (write-line "  }" dcl-fn)
    (write-line "  : toggle { key = \"auto_zoom\"; label = \"Automatyczny Zoom po imporcie\"; value = \"1\"; }" dcl-fn) 
    (write-line "  ok_cancel;" dcl-fn) 
    (write-line "}" dcl-fn) (close dcl-fn)  
      
    (setq dcl-id (load_dialog dcl-file)) (if (not (new_dialog "GeoFormat" dcl-id)) (progn (alert "Blad okna.") (exit)))  
    (set_tile "fmt_choice" current-format) (set_tile "delim_choice" (itoa current-delim)) (set_tile "flat_2d" is-flat) (set_tile "prec_geom" prec-geom-str) (set_tile "auto_zoom" do-zoom) (set_tile "use_file_nr" use-file-nr) (if (< len 3) (mode_tile "accept" 1))
    (geocad-import-update-use-file-nr-tile current-format c-nr)
    (action_tile "fmt_choice" "(geocad-import-update-use-file-nr-tile $value (get_tile \"col_nr\"))")
    (action_tile "col_nr" "(geocad-import-update-use-file-nr-tile (get_tile \"fmt_choice\") $value)")
    (action_tile "delim_choice" "(setq temp-delim (atoi $value) current-format (get_tile \"fmt_choice\") is-flat (get_tile \"flat_2d\") prec-geom-str (get_tile \"prec_geom\") do-zoom (get_tile \"auto_zoom\") use-file-nr (get_tile \"use_file_nr\") c-nr (get_tile \"col_nr\") c-x (get_tile \"col_x\") c-y (get_tile \"col_y\") c-z (get_tile \"col_z\")) (done_dialog 2)")
    (action_tile "accept" "(setq current-format (get_tile \"fmt_choice\") is-flat (get_tile \"flat_2d\") prec-geom-str (get_tile \"prec_geom\") do-zoom (get_tile \"auto_zoom\") use-file-nr (get_tile \"use_file_nr\") final-delim current-delim c-nr (get_tile \"col_nr\") c-x (get_tile \"col_x\") c-y (get_tile \"col_y\") c-z (get_tile \"col_z\")) (done_dialog 1)")
    (action_tile "cancel" "(done_dialog 0)")  
    (setq status (start_dialog)) (unload_dialog dcl-id) (vl-file-delete dcl-file)  
    (cond ((= status 0) (setq dialog-running nil *error* old-err) (exit)) ((= status 1) (setq dialog-running nil)) ((= status 2) (setq current-delim temp-delim))) 
  )  
  
  (setq format-choice current-format) (if (= format-choice "7") (setq c-nr (atoi c-nr) c-x (atoi c-x) c-y (atoi c-y) c-z (atoi c-z))) 
  (setq prec-geom (if (= prec-geom-str "") nil (atoi prec-geom-str)))

  (if
    (and
      (= use-file-nr "1")
      (or
        (member format-choice '("1" "2" "5" "6"))
        (and (= format-choice "7") (> c-nr 0))
      )
    )
    (progn
      (setq import-group (geocad-import-resolve-imported-group filename))
      (if (not import-group)
        (progn (setq *error* old-err) (exit))
      )
      (setq import-group (geocad-import-prepare-imported-group doc filename import-group))
      (princ (strcat "\nImport z nazwami z pliku: utworzono/aktywowano grupe " import-group "."))
    )
  )
  
  (vla-StartUndoMark doc)

  ;; Sesja importu pikiet.
  ;; Context, warstwy i konfiguracja sa przygotowane raz.
  ;; Numery z pliku / z lokalnego licznika importu sa przekazywane jawnie.
  (setq batch (geocad-pikieta-batch-start doc))

  (setq count 0 minX nil minY nil maxX nil maxY nil) 
  (princ "\nImportowanie z wylaczna obsluga interfejsu (Ustawienia rysunkowe pobrane z GEO_SETUP)...") 
  (setvar "OSMODE" 0) (setq file (open filename "r"))   
  
  (while (setq raw-line (read-line file))      
    (setq line (clean-raw-line raw-line) tokens (safe-tokenize line final-delim) valid nil file-has-nr nil len (length tokens) nr nil px 0.0 py 0.0 pz 0.0)
    (cond  
      ((and (= format-choice "1") (>= len 4)) (setq nr (nth 0 tokens) file-has-nr T px (safe-atof (nth 1 tokens)) py (safe-atof (nth 2 tokens)) pz (safe-atof (nth 3 tokens)) valid T))
      ((and (= format-choice "2") (>= len 4)) (setq nr (nth 0 tokens) file-has-nr T px (safe-atof (nth 2 tokens)) py (safe-atof (nth 1 tokens)) pz (safe-atof (nth 3 tokens)) valid T))
      ((and (= format-choice "3") (>= len 3)) (setq px (safe-atof (nth 0 tokens)) py (safe-atof (nth 1 tokens)) pz (safe-atof (nth 2 tokens)) valid T))  
      ((and (= format-choice "4") (>= len 3)) (setq px (safe-atof (nth 1 tokens)) py (safe-atof (nth 0 tokens)) pz (safe-atof (nth 2 tokens)) valid T))  
      ((and (= format-choice "5") (>= len 3)) (setq nr (nth 0 tokens) file-has-nr T px (safe-atof (nth 1 tokens)) py (safe-atof (nth 2 tokens)) pz 0.0 valid T))
      ((and (= format-choice "6") (>= len 3)) (setq nr (nth 0 tokens) file-has-nr T px (safe-atof (nth 2 tokens)) py (safe-atof (nth 1 tokens)) pz 0.0 valid T))
      ((= format-choice "7") (if (and (> c-x 0) (<= c-x len) (> c-y 0) (<= c-y len)) (progn (setq px (safe-atof (nth (1- c-x) tokens))) (setq py (safe-atof (nth (1- c-y) tokens))) (if (and (> c-z 0) (<= c-z len)) (setq pz (safe-atof (nth (1- c-z) tokens)))) (if (and (> c-nr 0) (<= c-nr len)) (setq nr (nth (1- c-nr) tokens) file-has-nr T)) (setq valid T))))
    )  
  
    (if (and valid (> px 1000.0) (> py 1000.0))      
      (progn      
        (if (not minX) (setq minX px maxX px minY py maxY py) (setq minX (min minX px) maxX (max maxX px) minY (min minY py) maxY (max maxY py))) 
        (if prec-geom (setq pz (round-to pz prec-geom))) (if (= is-flat "1") (setq pz-geom 0.0) (setq pz-geom pz)) 
        
        ;; Sprawdzamy czy dany plik w ogóle posiada kolumnę Z
        (setq show-z (if (member format-choice '("1" "2" "3" "4" "7")) T nil))

        ;; Czyste oddelegowanie do Biblioteki.
        ;; Gdy uzytkownik wylaczy numery z pliku albo format nie ma kolumny NR,
        ;; przekazujemy nil i batch dobiera kolejny numer z biezacej numeracji rysunku.
        (if (or (/= use-file-nr "1") (not file-has-nr))
          (setq nr nil)
        )

        (setq batch
          (geocad-pikieta-batch-insert
            batch
            mspace
            (list px py pz-geom)
            nr
            show-z
          )
        )
             
        (setq count (1+ count))      
      )      
    )      
  )   
  
  (if batch
  (progn
    (setq batch (geocad-pikieta-batch-end batch))
    (setq batch nil)
  )
)

(close file)

(setvar "OSMODE" old-osmode)

(vla-EndUndoMark doc)

(setq *error* old-err)
  (if (and minX (= do-zoom "1")) (progn (setq dXX (- maxX minX) dYY (- maxY minY) margX (if (> dXX 0) (* dXX 0.1) 20.0) margY (if (> dYY 0) (* dYY 0.1) 20.0) marg (max margX margY 10.0) p1 (list (- minX marg) (- minY marg) 0.0) p2 (list (+ maxX marg) (+ maxY marg) 0.0)) (vla-ZoomWindow acadObj (vlax-3d-point p1) (vlax-3d-point p2)))) 
  (princ (strcat "\nSukces! Zaimportowano " (itoa count) " pikiet.")) (princ)      
)      
  
(princ "\nSkrypt IMPORT_POINTS_V3_7 zaladowany.") (princ)
