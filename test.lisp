(load "planning-via-schemas.lisp")

(load "init.lisp")
(load "goal-schema.lisp")

(mark-objects-with-load *kb-ht*)
(setq goal-schema-inst (copy-schema *goal-schema2*))

(setq obj+bind+supports (find-a-block-to-place goal-schema-inst *kb-ht*))

(defun f (x) (format t "~%~s" x) '-----------------------)

(f obj+bind+supports)