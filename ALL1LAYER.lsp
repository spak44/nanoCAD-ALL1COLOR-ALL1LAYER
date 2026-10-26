; ALL1LAYER v1.1: selected entities + recursive block contents -> one existing layer.
; Requires Visual LISP/ActiveX (Windows). Test on a copy of the DWG.
; Shared block definitions affect ALL their references. Xrefs are skipped.
; Does not change explicit color/linetype/lineweight or unlock layers.
(vl-load-com)

(defun c:ALL1LAYER (/ *error* doc ss lay pick i obj visited done failed skipped undo
                       a1-set a1-block a1-object)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        done 0 failed 0 skipped 0 visited nil undo nil)
  (defun *error* (msg)
    (if undo (vla-EndUndoMark doc))
    (if msg (princ (strcat "\nStopped: " msg)))
    (princ))
  (defun a1-set (o / r)
    (setq r (vl-catch-all-apply 'vla-put-Layer (list o lay)))
    (if (vl-catch-all-error-p r)
      (setq failed (1+ failed))
      (setq done (1+ done))))
  (defun a1-block (name / b key child)
    (setq key (strcase name))
    (if (not (member key visited))
      (progn
        (setq visited (cons key visited)
              b (vl-catch-all-apply 'vla-Item
                  (list (vla-get-Blocks doc) name)))
        (if (vl-catch-all-error-p b)
          (setq failed (1+ failed))
          (if (= :vlax-true (vla-get-IsXRef b))
            (setq skipped (1+ skipped))
            (vlax-for child b (a1-object child)))))))
  (defun a1-object (o / nm eff ae ad)
    (a1-set o)
    (if (= "AcDbBlockReference" (vla-get-ObjectName o))
      (progn
        (setq nm (vla-get-Name o))
        (a1-block nm)
        ; Process both the displayed anonymous block and its original definition.
        (if (vlax-property-available-p o 'EffectiveName)
          (progn
            (setq eff (vlax-get-property o 'EffectiveName))
            (if (/= (strcase eff) (strcase nm)) (a1-block eff))))
        ; Walk attached ATTRIB entities directly: nanoCAD may return a
        ; VARIANT/SAFEARRAY from GetAttributes instead of a Lisp list.
        (setq ae (vlax-vla-object->ename o)
              ad (entget ae))
        (if (= 1 (cdr (assoc 66 ad)))
          (progn
            (setq ae (entnext ae))
            (while (and ae (= "ATTRIB" (cdr (assoc 0 (entget ae)))))
              (a1-set (vlax-ename->vla-object ae))
              (setq ae (entnext ae))))))))
  (setq ss (ssget "_I"))
  (if (not ss)
    (progn (princ "\nSelect objects and blocks: ") (setq ss (ssget))))
  (if ss
    (progn
      (princ "\nShared block contents change in ALL instances of those blocks.")
      (setq pick (entsel "\nPick object on target layer, or Enter for current layer: "))
      (setq lay (if pick (cdr (assoc 8 (entget (car pick)))) (getvar "CLAYER")))
      (vla-StartUndoMark doc)
      (setq undo T i 0)
      (repeat (sslength ss)
        (setq obj (vlax-ename->vla-object (ssname ss i)))
        (a1-object obj)
        (setq i (1+ i)))
      (vla-Regen doc 1)
      (vla-EndUndoMark doc)
      (setq undo nil)
      (princ (strcat "\nTarget layer: " lay
        ". Processed: " (itoa done)
        ". Failed: " (itoa failed)
        ". Skipped xref definitions: " (itoa skipped)
        ". Undo: U."))))
  (princ))
(princ "\nALL1LAYER v1.1 loaded. Command: ALL1LAYER")
(princ)
