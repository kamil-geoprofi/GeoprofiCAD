;; ======================================================
;; GEOPROFICAD - PROJECT MEMORY / CONFIG
;; ======================================================

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

(princ)
