; (all ?x (?x sequence-of.n ?pred-name ?rel-name)
;  ; a sequence of two or more elements satisfying the predicate named ?pred-name,
;  ; with each non-initial element related to its predecessor by the binary
;  ; relation named by ?rel-name;
; (all ?y ((?y (semval.f ?pred-name)) and (?y part-of ?x))
;           (all ?z ((?z (semval.f ?pred-name.n)) and (?z part-of ?x) (?z successor-of ?y ?x))
;                    (?z (semval.f ?rel-name) ?y))))

;NOTE: If ?y and ?z are part of the sequence, then they should be of type "?pred-name" already, no need for further checks

;=========================================== PRIMITIVES ============================================
(setq ?bl_schema '(obj-schema (?x BW-block.n)
   :types
     !t0 (?x block.n)
     !t1 (?c color.n)
     !t2 (?l BW-logo.n)
   :rigid-conds
     !r0 ((dimensions-of.f ?x) = ($ dim .15 .15 .15)))
     !r1 ((color-of.f ?x) = ?c)
     !r2 (?x carry26.v ?l)); "carry26" is the WN sense of "carrying on the surface"

(obj-schema (?x row-of.n ?P)
; a row of items of type ?P (where ?P is a monadic predicate *name*)
 :types
    !t0 (?x (maximal.a (sequence-of.n ?P 'abut.v)))
 :rigid-conds
    !r1 (?x (form7.v (straight.a line.n))); "form" is a predicate-taking verb, like "is"
    !r2 (?x horizontal.a)
 :skeletal-prototype
    row.obj) ; horizontal straight line, representing 'orientation' of an abstract row

(obj-schema (?x stack-of.n ?P)
; a stack of items of type ?P (where ?P is a monadic predicate *name*)
 :types
   !t0 (?x (maximal.a (sequence-of.n ?P 'on.n))))
 :rigid-conds
   !r1 (?x (form7.v (straight.a line.n)))
   !r2 (?x vertical.a)
 :skeletal-prototype
   stack.obj) ; vertical straight line, representing 'orientation' of an abstract stack

(obj-schema (?x BW-row.n)
 :types
   !t0 (?x row-of.n 'BW-block.n)
 :skeletal-prototype
   bw-row1.obj
   bw-row2.obj
   bw-row3.obj) ;Various prototypes of rows (of blocks)

(obj-schema (?x BW-stack.n)
 :types
   !t0 (?x stack-of.n 'BW-block.n)
   !t1 (?b (1st ?x)) ; assume 1st is a special primitive function on sequences
   !t2 (?c (lst ?x)) ; assume lst is a special primitive function on sequences
 :skeletal-prototype
   bw-stack1.obj
   bw-stack2.obj
   bw-stack3.obj) ;Various prototypes of stacks (of blocks)

;=========================================== STRUCTURES ============================================

(obj-schema (?x BW-arch.n)
 :types
   !t0 (?stack1 BW-stack.n)
   !t1 (?stack2 BW-stack.n)
   !t2 (?top BW-block.n)
   ;!r3 (?table (-er (much.adv big.a)) ?b)
 :rigid-conds
   !r0 (?top on.p ?stack1)
   !r1 (?top on.p ?stack2)  
   !r2 (?stack1 next-to.p ?stack2)
   !r3 (not (?stack1 touching.p ?stack2))
   !r4 (?top clear.a)
   !r5 ((height-of.f ?stack1) = (height-of.f ?stack2))  ; assume height-of.f is a function that acts on stacks, and return # of items
 :skeletal-prototype
   bw-arch1.obj
   bw-arch2.obj) ;Various prototypes of arches

(obj-schema (?x BW-staircase.n)
 :types
   !t0 (?x row-of.n 'BW-stack.n)
 :rigid-conds
   !r0 ((height-of.f (1st ?x)) = 1) ; assume height-of.f is a function that acts on stacks, and return # of blocks
   !r1 (all ?y: (all ?z: (((?y part-of ?x) and (?z part-of ?x) (?y successor-of ?z ?x))
                    ((height-of.f ?y) = ((height-of.f ?z) + 1)))))
 :skeletal-prototype
   bw-staircase1.obj
   bw-staircase2.obj)

(obj-schema (?x BW-wedge.n)
 :types
   !t0 (?x BW-block.n)
   !t1 (?y BW-row.n)
 :rigid-conds
   !r0 (?x on.p ?y)
   !r1 ((length-of.f ?y) = 2) ; assume length-of.f is a function that acts on row-of-blocks, and return # of blocks
   !r2 (?x vertically-center-aligned.p ?y)
 :skeletal-prototype
   bw-wedge1.obj
   bw-wedge2.obj)

;# center-aligned.p ? (allow for left-aligned.p, right-aligned.p)
;Note: I decided to keep "vertically" to distinguish between vertical and horizontal sequences,
;since a line of stacks can also be 'center-aligned' if their center are on the same line.

(obj-schema (?x BW-pyramid.n)
 :types
   !t0 (?x stack-of.n 'BW-row.n)
 :rigid-conds
   ;Each next row is shorter than the previous
   !r0 (all ?y: (all ?z: (((?y part-of ?x) and (?z part-of ?x) (?y successor-of ?z ?x))
                    ((length-of.f ?y) < (length-of.f ?z)))))
   !r1 ((length-of.f ?y) > 1)
   !r2 (?x vertically-center-aligned.a)
 :skeletal-prototype
   bw-pyramid1.obj
   bw-pyramid2.obj
   bw-pyramid3.obj)