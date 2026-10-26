; ALL1PROP v1.0 - recursive entity properties, without exploding blocks.
; Windows nanoCAD with Visual LISP/ActiveX. Test on a COPY of the DWG.
; Shared block definitions change ALL references, including unselected ones.
; Xref contents, locked layers, MTEXT inline formatting and dimension
; overrides are not changed. Scale means LINETYPE scale, not geometry scale.
(vl-load-com)

; Return DXF data with changes confined to the AcDbEntity subclass.
; Later subclasses and XDATA are preserved, even if they use matching codes.
(defun ncprop-data (data changes removeCodes / out inside found inserted p item)
  (setq out nil inside nil found nil inserted nil)
  (foreach p data
    (if (or (= 100 (car p)) (= -3 (car p)))
      (progn
        (if inside
          (progn
            (foreach item changes (setq out (cons item out)))
            (setq inserted T inside nil)))
        (if (and (= 100 (car p)) (= "AcDbEntity" (cdr p)))
          (setq inside T found T))))
    (if (not (and inside (member (car p) removeCodes)))
      (setq out (cons p out))))
  (if (and found (not inserted))
    (foreach item changes (setq out (cons item out))))
  (if found (reverse out) nil))

(defun ncprop-locked (layerName / rec)
  (setq rec (tblsearch "LAYER" layerName))
  (and rec (/= 0 (logand 4 (cdr (assoc 70 rec))))))

(defun ncprop-int (message low high / v valid)
  (setq valid nil)
  (while (not valid)
    (setq v (getint message))
    (if (or (null v) (and (>= v low) (<= v high)))
      (setq valid T)
      (princ (strcat "\nValid range: " (itoa low) ".." (itoa high) "."))))
  v)

(defun ncprop-run (mode / *error* doc ss changes removeCodes targetLayer
       v s key sample d n weight allowedWeight scaleValue alpha
       visitedBlocks changed failed locked xrefs undo i
       ncprop-add ncprop-write ncprop-entity ncprop-block)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        changes nil removeCodes nil targetLayer nil
        changed 0 failed 0 locked 0 xrefs 0 undo nil
        visitedBlocks nil)
  (defun *error* (msg)
    (if undo (vl-catch-all-apply 'vla-EndUndoMark (list doc)))
    (if msg (princ (strcat "\nStopped: " msg
      "\nSome objects may have changed. Use U to undo, or reopen your copy.")))
    (princ))
  (defun ncprop-add (pairs codes)
    (setq changes (append changes pairs) removeCodes (append removeCodes codes)))
  (setq ss (ssget "_I"))
  (if (not ss)
    (progn (princ "\nSelect objects and blocks: ") (setq ss (ssget))))
  (if ss
    (progn
      (princ "\nALL1PROP v1.0. Enter at each property = KEEP its current values.")
      (princ "\nShared block contents change in ALL instances. Xref contents skipped.")
      ; Layer: existing layer name, or sample object.
      (if (= mode "ALL")
        (progn
          (setq key nil)
          (while (not key)
            (initget "Name Sample")
            (setq s (getkword "\nLayer [Name/Sample] <Keep>: "))
            (cond
              ((null s) (setq key T))
              ((= s "Name")
                (setq v (getstring T "\nExisting target layer name: "))
                (if (and (tblsearch "LAYER" v) (not (ncprop-locked v)))
                  (setq targetLayer (cdr (assoc 2 (tblsearch "LAYER" v))) key T)
                  (princ "\nLayer not found or locked. Create/unlock it first.")))
              ((= s "Sample")
                (setq sample (entsel "\nPick object on target layer: "))
                (if sample
                  (progn
                    (setq v (cdr (assoc 8 (entget (car sample)))))
                    (if (ncprop-locked v)
                      (princ "\nTarget layer is locked.")
                      (setq targetLayer v key T)))))))
          (if targetLayer (ncprop-add (list (cons 8 targetLayer)) '(8)))))
      ; Color: ACI, ByLayer/ByBlock, or assigned color from a nested sample.
      (if (= mode "ALL")
        (progn
          (initget "Number Sample")
          (setq s (getkword "\nColor [Number/Sample] <Keep>: "))
          (cond
            ((= s "Number")
              (setq v (ncprop-int
                "\nColor 1..255, 0=ByBlock, 256=ByLayer <Keep>: " 0 256))
              (if v (ncprop-add (list (cons 62 v)) '(62 420 430))))
            ((= s "Sample")
              (setq sample (nentsel "\nPick color sample, Enter=Keep: "))
              (if sample
                (progn
                  (setq d (entget (car sample))
                        v (vl-remove-if-not
                          '(lambda (p) (member (car p) '(62 420 430))) d))
                  (if (not v) (setq v '((62 . 256))))
                  (ncprop-add v '(62 420 430))))))))
      ; Explicit transparency: 0 = opaque, 100 = invisible.
      (if (member mode '("ALL" "TRANS"))
        (progn
          (initget "Value Layer Block")
          (setq s (getkword "\nTransparency [Value/Layer/Block] <Keep>: "))
          (cond
            ((= s "Layer") (ncprop-add nil '(440)))
            ((= s "Block") (ncprop-add '((440 . 16777216)) '(440)))
            ((= s "Value")
              (setq v (ncprop-int "\nTransparency 0..100 <Keep>: " 0 100))
              (if v
                (progn
                  (setq alpha (fix (+ 0.5 (* 255.0 (/ (- 100 v) 100.0)))))
                  (ncprop-add (list (cons 440 (+ 33554432 alpha))) '(440))))))))
      ; Linetype must already be loaded in this drawing.
      (if (member mode '("ALL" "LTYPE"))
        (progn
          (setq key nil)
          (while (not key)
            (initget "Name Sample Layer Block Continuous")
            (setq s (getkword
              "\nLinetype [Name/Sample/Layer/Block/Continuous] <Keep>: "))
            (cond
              ((null s) (setq key T v nil))
              ((= s "Layer") (setq v "ByLayer" key T))
              ((= s "Block") (setq v "ByBlock" key T))
              ((= s "Continuous") (setq v "Continuous" key T))
              ((= s "Sample")
                (setq sample (nentsel "\nPick linetype sample, Enter=Keep: "))
                (setq v (if sample (cdr (assoc 6 (entget (car sample)))) nil))
                (if (and sample (not v)) (setq v "ByLayer"))
                (setq key T))
              ((= s "Name")
                (setq v (getstring T "\nLoaded linetype name: "))
                (if (setq d (tblsearch "LTYPE" v))
                  (setq v (cdr (assoc 2 d)) key T)
                  (princ "\nLinetype not loaded. Load it using LINETYPE first.")))))
          (if v (ncprop-add (list (cons 6 v)) '(6)))))
      ; Lineweight is NOT polyline geometric width.
      (if (member mode '("ALL" "WEIGHT"))
        (progn
          (setq key nil weight nil
            allowedWeight '(0 5 9 13 15 18 20 25 30 35 40 50 53 60 70 80
                            90 100 106 120 140 158 200 211))
          (while (not key)
            (initget "Value Layer Block Default")
            (setq s (getkword "\nLineweight [Value/Layer/Block/Default] <Keep>: "))
            (cond
              ((null s) (setq key T))
              ((= s "Layer") (setq weight -1 key T))
              ((= s "Block") (setq weight -2 key T))
              ((= s "Default") (setq weight -3 key T))
              ((= s "Value")
                (initget 4)
                (setq v (getreal "\nLineweight in mm, e.g. 0.25 <Keep>: "))
                (cond
                  ((null v) (setq key T))
                  ((and (member (fix (+ 0.5 (* 100 v))) allowedWeight)
                        (< (abs (- (* 100 v) (fix (+ 0.5 (* 100 v))))) 0.000001))
                    (setq weight (fix (+ 0.5 (* 100 v))) key T))
                  (T (princ "\nUse a standard weight, e.g. 0.13, 0.18, 0.25, 0.35, 0.50."))))))
          (if weight (ncprop-add (list (cons 370 weight)) '(370)))))
      (if (= mode "ALL")
        (progn
          (initget 6)
          (setq scaleValue (getreal "\nLINETYPE scale, positive number <Keep>: "))
          (if scaleValue (ncprop-add (list (cons 48 scaleValue)) '(48)))))
      (if removeCodes
        (progn
          (defun ncprop-write (en / data newData)
            (setq data (entget en))
            (cond
              ((ncprop-locked (cdr (assoc 8 data))) (setq locked (1+ locked)))
              ((setq newData (ncprop-data data changes removeCodes))
                (if (entmod newData)
                  (setq changed (1+ changed))
                  (setq failed (1+ failed))))
              (T (setq failed (1+ failed)))))
          (defun ncprop-block (name / rec en next key)
            (setq key (strcase name))
            (if (not (member key visitedBlocks))
              (progn
                (setq visitedBlocks (cons key visitedBlocks)
                      rec (tblsearch "BLOCK" name))
                (cond
                  ((null rec) (setq failed (1+ failed)))
                  ((/= 0 (logand 12 (cdr (assoc 70 rec))))
                    (setq xrefs (1+ xrefs)))
                  (T
                    (setq en (entnext (tblobjname "BLOCK" name)))
                    (while (and en (/= "ENDBLK" (cdr (assoc 0 (entget en)))))
                      (setq next (entnext en))
                      (if (not (member (cdr (assoc 0 (entget en))) '("VERTEX" "SEQEND")))
                        (ncprop-entity en))
                      (setq en next)))))))
          (defun ncprop-entity (en / data result name obj effective ae)
            (setq data (entget en)
                  result (vl-catch-all-apply 'ncprop-write (list en)))
            (if (vl-catch-all-error-p result) (setq failed (1+ failed)))
            (if (= "INSERT" (cdr (assoc 0 data)))
              (progn
                (setq name (cdr (assoc 2 data)))
                (ncprop-block name)
                (setq obj (vlax-ename->vla-object en))
                (if (vlax-property-available-p obj 'EffectiveName)
                  (progn
                    (setq effective (vl-catch-all-apply 'vlax-get-property
                                      (list obj 'EffectiveName)))
                    (if (and (not (vl-catch-all-error-p effective))
                             (= (type effective) 'STR)
                             (/= (strcase effective) (strcase name)))
                      (ncprop-block effective))))
                (if (= 1 (cdr (assoc 66 data)))
                  (progn
                    (setq ae (entnext en))
                    (while (and ae (= "ATTRIB" (cdr (assoc 0 (entget ae)))))
                      (ncprop-entity ae)
                      (setq ae (entnext ae))))))))
          (vla-StartUndoMark doc)
          (setq undo T i 0)
          (repeat (sslength ss)
            (ncprop-entity (ssname ss i))
            (setq i (1+ i)))
          (vla-Regen doc 1)
          (vla-EndUndoMark doc)
          (setq undo nil)
          (princ (strcat "\nDone. Processed: " (itoa changed)
            ". Failed: " (itoa failed) ". Locked: " (itoa locked)
            ". Skipped xref definitions: " (itoa xrefs) ". Undo: U.")))
        (princ "\nNo properties selected. Nothing changed."))))
  (princ))

(defun c:ALL1PROP () (ncprop-run "ALL"))
(defun c:ALL1TRANS () (ncprop-run "TRANS"))
(defun c:ALL1WEIGHT () (ncprop-run "WEIGHT"))
(defun c:ALL1LTYPE () (ncprop-run "LTYPE"))
(princ "\nALL1PROP v1.0 loaded: ALL1PROP / ALL1TRANS / ALL1WEIGHT / ALL1LTYPE")
(princ)
