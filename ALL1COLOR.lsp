; ALL1COLOR v1.0: selected entities + recursive block contents -> one assigned color.
; Requires Visual LISP/ActiveX (Windows). Test on a copy of the DWG.
; Shared block definitions affect ALL their references. Xrefs are skipped.
; Preserves layers, linetypes and lineweights. Does not unlock layers.
; Does not remove inline color formatting inside MTEXT or dimension overrides.
(vl-load-com)

(defun c:ALL1COLOR (/ *error* doc ss colorData colorNum pick sampleData i obj visited done failed skipped undo
                       a1-set a1-write a1-block a1-object)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        done 0 failed 0 skipped 0 visited nil undo nil)
  (defun *error* (msg)
    (if undo (vla-EndUndoMark doc))
    (if msg (princ (strcat "\nStopped: " msg)))
    (princ))
  (defun a1-write (o / en data)
    (setq en (vlax-vla-object->ename o) data (entget en))
    ; Remove old ACI, TrueColor and color-book overrides, keeping all other data.
    (setq data (vl-remove-if
      '(lambda (p) (member (car p) '(62 420 430))) data))
    (entmod (append data colorData)))
  (defun a1-set (o / r)
    (setq r (vl-catch-all-apply 'a1-write (list o)))
    (if (or (vl-catch-all-error-p r) (null r))
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
      (setq colorNum -1)
      (while (and colorNum (or (< colorNum 0) (> colorNum 256)))
        (initget 4)
        (setq colorNum (getint
          "\nColor 1..255, 0=ByBlock, 256=ByLayer, Enter=sample: "))
        (if (and colorNum (> colorNum 256))
          (princ "\nUse a number from 0 to 256.")))
      (if colorNum
        (setq colorData (list (cons 62 colorNum)))
        (progn
          (setq pick (nentsel "\nPick color sample, or Enter for ByLayer: "))
          (if pick
            (progn
              (setq sampleData (entget (car pick)))
              (setq colorData
                (vl-remove-if-not
                  '(lambda (p) (member (car p) '(62 420 430))) sampleData))
              (if (not colorData) (setq colorData '((62 . 256)))))
            (setq colorData '((62 . 256))))))
      (vla-StartUndoMark doc)
      (setq undo T i 0)
      (repeat (sslength ss)
        (setq obj (vlax-ename->vla-object (ssname ss i)))
        (a1-object obj)
        (setq i (1+ i)))
      (vla-Regen doc 1)
      (vla-EndUndoMark doc)
      (setq undo nil)
      (princ (strcat "\nColor assignment complete. Processed: " (itoa done)
        ". Failed: " (itoa failed)
        ". Skipped xref definitions: " (itoa skipped)
        ". Undo: U."))))
  (princ))
(princ "\nALL1COLOR v1.0 loaded. Command: ALL1COLOR")
(princ)
