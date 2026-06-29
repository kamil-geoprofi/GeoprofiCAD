;; ======================================================
;; GEOPROFICAD - PROJECT MEMORY / CONFIG
;; ======================================================
;;
;; LDATA, konfiguracja DWG i podstawowe parsowanie tagow.
;; Shadow split: te definicje sa zgodne z legacy i sa ladowane
;; po gp_CoreLegacy.lsp.
;; ======================================================

(setq *geocad-module-projectmemory-loaded* T)

(defun geocad-parse-tags (str / res tmp)
  (setq res '() tmp "")
  (if (and str (/= str ""))
    (foreach ch (vl-string->list (strcase str))
      (if (member ch '(44 59 32 9)) 
        (if (/= tmp "") (progn (setq res (cons tmp res)) (setq tmp "")))
        (setq tmp (strcat tmp (chr ch)))
      )
    )
  )
  (if (/= tmp "") (setq res (cons tmp res)))
  (reverse res)
)

(defun geocad-setup-ldata-get (key / res)
  (setq res
    (vl-catch-all-apply
      'vlax-ldata-get
      (list *geocad-ldata-setup-dict* key)
    )
  )

  (if (vl-catch-all-error-p res)
    nil
    res
  )
)

(defun geocad-setup-ldata-put (key value)
  (vl-catch-all-apply
    'vlax-ldata-put
    (list *geocad-ldata-setup-dict* key value)
  )

  value
)

(defun geocad-get-global-cfg (klucz domyslny / val)
  (setq val (vl-registry-read *geocad-registry-path* klucz))

  (if (not val)
    (progn
      (vl-registry-write *geocad-registry-path* klucz domyslny)
      domyslny
    )
    val
  )
)

(defun geocad-get-cfg (klucz domyslny / val)
  ;; Kolejnosc:
  ;; 1. pamiec konkretnego DWG,
  ;; 2. wartosc domyslna.
  ;;
  ;; Nie czytamy juz rejestru Windows jako fallbacku,
  ;; bo nowy rysunek nie powinien dziedziczyc ustawien
  ;; ze starego projektu.
  (setq val (geocad-setup-ldata-get klucz))

  (if val
    val
    domyslny
  )
)

(defun geocad-set-cfg (klucz value)
  ;; Zapisujemy tylko do DWG.
  ;;
  ;; DWG = pamiec projektu.
  ;; Rejestr Windows nie jest juz uzywany jako automatyczny fallback,
  ;; zeby nowe rysunki startowaly czysto od wartosci domyslnych.
  (geocad-setup-ldata-put klucz value)
  value
)

(princ)
