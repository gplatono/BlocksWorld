(defparameter *kb-ht* (make-hash-table :test #'equal))

(setq *init-facts* '((on B1 B2) (on B1 B3) (on B4 B5) (on B2 table) (on B3 table)
   (on B5 B6) (on B6 table) (block b1) (block b2) (block b3) (block b4) (block b5)
   (block b6) (red b1) (red b2) (blue b3) (blue b4) (red b5) (red b6)
   (between B3 B2 B6) (between B3 B1 B6) (between B1 B2 B3) (between B5 B4 B6) ))
 
(store-facts *init-facts* *kb-ht*)

(mark-objects-with-load *kb-ht*)

(declare-as-available '(table B1 B2 B3 B4 B5 B6) '*init-facts* *kb-ht*)