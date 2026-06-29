(vl-load-com)

;; ======================================================
;; GEOPROFICAD - CORE RUNTIME ORCHESTRATOR
;; ======================================================
;;
;; Techniczny punkt przejscia dla refaktoru core.
;; Kolejnosc ladowania runtime/legacy/modulow jest obecnie jawnie
;; zapisana w PackageContents.xml.
;;
;; Ten plik celowo nie laduje jeszcze podmodulow samodzielnie,
;; zeby nie dublowac ladowania z manifestu bundle.
;; ======================================================

(setq *geocad-core-split-stage* "shadow-split")
(setq *geocad-core-runtime-loaded* T)

(defun geocad-core-split-status ()
  (list
    (cons "runtime" (if (boundp '*geocad-core-runtime-loaded*) *geocad-core-runtime-loaded* nil))
    (cons "project-memory" (if (boundp '*geocad-module-projectmemory-loaded*) *geocad-module-projectmemory-loaded* nil))
    (cons "numbering" (if (boundp '*geocad-module-numbering-loaded*) *geocad-module-numbering-loaded* nil))
    (cons "cad-objects" (if (boundp '*geocad-module-cadobjects-loaded*) *geocad-module-cadobjects-loaded* nil))
    (cons "pikieta-factory" (if (boundp '*geocad-module-pikietafactory-loaded*) *geocad-module-pikietafactory-loaded* nil))
    (cons "pikieta-style" (if (boundp '*geocad-module-pikietastyle-loaded*) *geocad-module-pikietastyle-loaded* nil))
    (cons "setup-dialog" (if (boundp '*geocad-module-setupdialog-loaded*) *geocad-module-setupdialog-loaded* nil))
  )
)

(defun c:GEOCAD_SPLIT_STATUS ()
  (princ "\nGEOPROFICAD CORE SPLIT STATUS:")
  (foreach item (geocad-core-split-status)
    (princ
      (strcat
        "\n - "
        (car item)
        ": "
        (if (cdr item) "loaded" "not loaded")
      )
    )
  )
  (princ)
)

(princ)
