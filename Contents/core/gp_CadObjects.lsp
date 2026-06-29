;; ======================================================
;; GEOPROFICAD - CAD OBJECTS
;; ======================================================
;;
;; Niskopoziomowe helpery AutoCAD/VLAX.
;; Shadow split: definicje sa zgodne z gp_CoreLegacy.lsp
;; i sa ladowane po legacy, bez zmiany publicznego API.
;; ======================================================

(setq *geocad-module-cadobjects-loaded* T)

(defun geocad-block-attr-text
  (obj tag / result)
  (setq result nil)

  (if obj
    (foreach att (vlax-invoke obj 'GetAttributes)
      (if (= (strcase (vla-get-TagString att)) (strcase tag))
        (setq result (vla-get-TextString att))
      )
    )
  )

  result
)

(defun get-pt-from-obj (obj / type pt-list val z-tags)   
  (setq type (vla-get-ObjectName obj))
  (setq z-tags (geocad-parse-tags (geocad-get-cfg "ZTags" "H,Z,RZEDNA")))
  
  (cond   
    ((= type "AcDbPoint")   
     (setq pt-list (vlax-safearray->list (vlax-variant-value (vla-get-Coordinates obj))))  
    )  
    ((member type '("AcDbBlockReference" "AcDbText" "AcDbMText"))  
     (setq pt-list (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint obj))))  
     (if (= type "AcDbBlockReference")    
       (foreach att (vlax-invoke obj 'GetAttributes)    
         (if (member (strcase (vla-get-TagString att)) z-tags)  
           (progn  
             (setq val (vla-get-TextString att))  
             (if (and val (/= val "") (/= val "---") (distof (vl-string-translate "," "." val)))  
               (setq pt-list (list (car pt-list) (cadr pt-list) (atof (vl-string-translate "," "." val))))  
             )  
           )  
         )  
       )  
     )  
    )  
  )  
  pt-list  
)

(defun geocad-ensure-layer (doc layname color / layers lay res)
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

(defun geocad-safe-delete-object
  (obj)
  (if obj
    (vl-catch-all-apply
      'vla-Delete
      (list obj)
    )
  )

  nil
)

(defun geocad-object-point-list
  (obj / obj-name)
  (if obj
    (progn
      (setq obj-name (vla-get-ObjectName obj))

      (cond
        ((= obj-name "AcDbPoint")
          (vlax-safearray->list
            (vlax-variant-value
              (vla-get-Coordinates obj)
            )
          )
        )

        (T
          (vlax-safearray->list
            (vlax-variant-value
              (vla-get-InsertionPoint obj)
            )
          )
        )
      )
    )
    nil
  )
)

(defun geocad-set-object-visible
  (obj visible)
  ;; Dziala dla TEXT/POINT/INSERT, jezeli obiekt wspiera wlasciwosc Visible.
  ;; Jezeli dany obiekt jej nie wspiera, ignorujemy blad.
  (if obj
    (vl-catch-all-apply
      'vla-put-Visible
      (list
        obj
        (if visible :vlax-true :vlax-false)
      )
    )
  )

  obj
)

(defun geocad-text-string-or-empty
  (obj / val)
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

(defun geocad-make-text-entity
  (pt txt-h text-value layname visible / ent obj)
  (if (not text-value)
    (setq text-value "")
  )

  (setq ent
    (entmakex
      (list
        '(0 . "TEXT")
        (cons 10 pt)
        (cons 40 txt-h)
        (cons 1 text-value)
        (cons 8 layname)
      )
    )
  )

  (if ent
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (geocad-set-object-visible obj visible)
    )
  )

  ent
)

(defun geocad-make-point-entity
  (pt layname)
  (entmakex
    (list
      '(0 . "POINT")
      (cons 10 pt)
      (cons 8 layname)
    )
  )
)

(defun geocad-find-nearest-text-object
  (layname target-pt tol / ss i obj pt d best best-d)
  ;; Szuka najblizszego TEXT na podanej warstwie.
  ;; Uzywane do parowania tekstow NR/H z punktem pikiety.
  (setq best nil)
  (setq best-d nil)

  (if
    (and
      layname
      (/= layname "")
      target-pt
      (tblsearch "LAYER" layname)
    )
    (progn
      (setq ss
        (ssget
          "_X"
          (list
            '(0 . "TEXT")
            (cons 8 layname)
          )
        )
      )

      (if ss
        (progn
          (setq i 0)

          (while (< i (sslength ss))
            (setq obj (vlax-ename->vla-object (ssname ss i)))
            (setq pt (geocad-object-point-list obj))

            (if pt
              (progn
                (setq d (distance pt target-pt))

                (if
                  (and
                    (<= d tol)
                    (or
                      (not best-d)
                      (< d best-d)
                    )
                  )
                  (progn
                    (setq best obj)
                    (setq best-d d)
                  )
                )
              )
            )

            (setq i (1+ i))
          )
        )
      )
    )
  )

  best
)

(defun geocad-update-text-object
  (obj pt txt-h layname text-value visible)
  (if obj
    (progn
      (vl-catch-all-apply
        'vla-put-Layer
        (list obj layname)
      )

      (vl-catch-all-apply
        'vla-put-Height
        (list obj txt-h)
      )

      (if text-value
        (vl-catch-all-apply
          'vla-put-TextString
          (list obj text-value)
        )
      )

      (vl-catch-all-apply
        'vla-put-InsertionPoint
        (list obj (vlax-3d-point pt))
      )

      (geocad-set-object-visible obj visible)
    )
  )

  obj
)

(princ)
